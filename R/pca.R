# PCA analysis ---------------------------------------------------------------

bp_pca_defaults <- function(project = NULL) {
  active_source_id <- project$active_data_source_id %||% "dataset_example"
  active_source <- Filter(function(source) identical(source$id, active_source_id), project$data_sources %||% list())
  if (length(active_source) && isTRUE(active_source[[1]]$derived)) active_source_id <- "dataset_example"
  list(
    chart_type = "pca",
    expression_source_id = active_source_id,
    metadata_source_id = "",
    expression_orientation = "auto",
    feature_id_location = "auto",
    feature_id_field = "",
    expression_sample_id_location = "auto",
    expression_sample_id_field = "",
    metadata_sample_id_field = "",
    unmatched_sample_policy = "strict",
    transform = "auto",
    variable_feature_count = 1000L,
    custom_feature_count = 1000L,
    remove_zero_variance = TRUE,
    missing_value_policy = "stop",
    center = TRUE,
    scale = FALSE,
    x_component = "PC1",
    y_component = "PC2",
    color_field = "",
    shape_field = "",
    label_field = "",
    point_color = "#2C7FB8",
    point_size = 3,
    alpha = 0.8,
    shape = "16",
    palette = "default",
    show_ellipse = FALSE,
    ellipse_level = 0.95,
    title = "PCA",
    legend_title = "",
    theme = "classic",
    base_size = 12,
    advanced_preserved = FALSE
  )
}

bp_normalize_pca_config <- function(config, project = NULL) {
  defaults <- bp_pca_defaults(project)
  config <- utils::modifyList(defaults, config %||% list(), keep.null = TRUE)
  character_fields <- c(
    "chart_type", "expression_source_id", "metadata_source_id", "expression_orientation",
    "feature_id_location", "feature_id_field", "expression_sample_id_location",
    "expression_sample_id_field", "metadata_sample_id_field", "unmatched_sample_policy",
    "transform", "missing_value_policy", "x_component", "y_component", "color_field",
    "shape_field", "label_field", "point_color", "shape", "palette", "title",
    "legend_title", "theme"
  )
  for (name in character_fields) {
    value <- config[[name]]
    config[[name]] <- if (is.null(value) || !length(value) || is.na(value[[1]])) defaults[[name]] else as.character(value[[1]])
  }
  expression_source <- Filter(function(source) identical(source$id, config$expression_source_id), project$data_sources %||% list())
  if (!nzchar(config$expression_source_id) || (length(expression_source) && isTRUE(expression_source[[1]]$derived))) {
    config$expression_source_id <- defaults$expression_source_id
  }
  config$chart_type <- "pca"
  if (!config$expression_orientation %in% c("auto", "genes_by_samples", "samples_by_features")) config$expression_orientation <- "auto"
  if (!config$feature_id_location %in% c("auto", "rownames", "column", "none")) config$feature_id_location <- "auto"
  if (!config$expression_sample_id_location %in% c("auto", "column_names", "rownames", "column")) config$expression_sample_id_location <- "auto"
  if (!config$unmatched_sample_policy %in% c("strict", "matched_only")) config$unmatched_sample_policy <- "strict"
  if (!config$transform %in% c("auto", "none", "log2p1")) config$transform <- "auto"
  if (!config$missing_value_policy %in% c("stop", "omit_features")) config$missing_value_policy <- "stop"
  feature_count <- config$variable_feature_count
  if (is.character(feature_count) && identical(feature_count[[1]], "all")) {
    config$variable_feature_count <- "all"
  } else {
    feature_count <- suppressWarnings(as.integer(feature_count %||% defaults$variable_feature_count))
    if (!length(feature_count) || is.na(feature_count[[1]]) || feature_count[[1]] < 2L) feature_count <- defaults$variable_feature_count
    config$variable_feature_count <- feature_count[[1]]
  }
  custom_count <- suppressWarnings(as.integer(config$custom_feature_count %||% defaults$custom_feature_count))
  if (!length(custom_count) || is.na(custom_count[[1]]) || custom_count[[1]] < 2L) custom_count <- defaults$custom_feature_count
  config$custom_feature_count <- custom_count[[1]]
  number <- function(value, default, minimum, maximum) {
    value <- suppressWarnings(as.numeric(value %||% default))
    if (!length(value) || !is.finite(value[[1]])) value <- default
    min(max(value[[1]], minimum), maximum)
  }
  config$point_size <- number(config$point_size, defaults$point_size, 0.1, 20)
  config$alpha <- number(config$alpha, defaults$alpha, 0, 1)
  config$ellipse_level <- number(config$ellipse_level, defaults$ellipse_level, 0.5, 0.999)
  config$base_size <- number(config$base_size, defaults$base_size, 6, 40)
  config$remove_zero_variance <- if (is.null(config$remove_zero_variance)) TRUE else isTRUE(config$remove_zero_variance)
  config$center <- if (is.null(config$center)) TRUE else isTRUE(config$center)
  config$scale <- isTRUE(config$scale)
  config$show_ellipse <- isTRUE(config$show_ellipse)
  config$advanced_preserved <- isTRUE(config$advanced_preserved)
  if (!grepl("^#[0-9A-Fa-f]{6}$", config$point_color)) config$point_color <- defaults$point_color
  if (!config$shape %in% as.character(0:25)) config$shape <- defaults$shape
  if (!config$palette %in% c("default", "blue_red", "viridis_like")) config$palette <- defaults$palette
  if (!config$theme %in% c("classic", "minimal", "bw")) config$theme <- defaults$theme
  config
}

# This function intentionally uses only base/stats R so its deparsed body can be
# embedded verbatim in exported, standalone PCA scripts.
bp_pca_compute_core <- function(expression_data, metadata_data = NULL, config = list()) {
  value <- function(name, default = NULL) {
    item <- config[[name]]
    if (is.null(item) || !length(item) || is.na(item[[1]])) default else item[[1]]
  }
  warnings <- character()
  diagnostics <- list(
    expression_samples = 0L, metadata_samples = 0L, matched_samples = 0L,
    expression_only = character(), metadata_only = character(), duplicate_expression_ids = character(),
    duplicate_metadata_ids = character()
  )
  abort <- function(message) {
    condition <- simpleError(message)
    condition$diagnostics <- diagnostics
    condition$warnings <- unique(warnings)
    stop(condition)
  }
  meaningful_rownames <- function(frame) {
    ids <- row.names(frame)
    length(ids) == nrow(frame) && !identical(as.character(ids), as.character(seq_len(nrow(frame))))
  }
  duplicate_values <- function(ids) unique(ids[duplicated(ids) | duplicated(ids, fromLast = TRUE)])
  collapse_ids <- function(ids, limit = 8L) {
    if (!length(ids)) return("")
    shown <- utils::head(ids, limit)
    paste0(paste(shown, collapse = "、"), if (length(ids) > limit) paste0(" 等 ", length(ids), " 个") else "")
  }

  if (!(is.data.frame(expression_data) || is.matrix(expression_data))) {
    abort("表达数据必须是 data.frame 或二维矩阵。")
  }
  expression_frame <- as.data.frame(expression_data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  if (nrow(expression_frame) < 2L || ncol(expression_frame) < 2L) abort("PCA 表达数据至少需要 2 行和 2 列。")

  metadata_frame <- NULL
  metadata_ids <- character()
  metadata_id_field <- as.character(value("metadata_sample_id_field", ""))
  if (!is.null(metadata_data)) {
    if (!(is.data.frame(metadata_data) || is.matrix(metadata_data))) abort("样本信息必须是 data.frame 或二维矩阵。")
    metadata_frame <- as.data.frame(metadata_data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
    if (!nzchar(metadata_id_field)) {
      preferred <- names(metadata_frame)[tolower(names(metadata_frame)) %in% c("sample", "sample_id", "sampleid", "id")]
      metadata_id_field <- if (length(preferred)) preferred[[1]] else names(metadata_frame)[[1]]
    }
    if (!metadata_id_field %in% names(metadata_frame)) abort(paste0("样本信息中不存在 ID 字段 ‘", metadata_id_field, "’。"))
    metadata_ids <- trimws(as.character(metadata_frame[[metadata_id_field]]))
    diagnostics$metadata_samples <- length(metadata_ids)
    if (any(!nzchar(metadata_ids) | is.na(metadata_ids))) abort("样本信息中存在空的样本 ID。")
    diagnostics$duplicate_metadata_ids <- duplicate_values(metadata_ids)
    if (length(diagnostics$duplicate_metadata_ids)) {
      abort(paste0("无法关联样本信息。字段 ", metadata_id_field, " 中存在重复样本 ID：", collapse_ids(diagnostics$duplicate_metadata_ids), "。"))
    }
  }

  numeric_columns <- names(expression_frame)[vapply(expression_frame, function(column) is.numeric(column) || is.integer(column), logical(1))]
  orientation <- as.character(value("expression_orientation", "auto"))
  feature_id_field <- as.character(value("feature_id_field", ""))
  sample_id_field <- as.character(value("expression_sample_id_field", ""))
  if (identical(orientation, "auto")) {
    if (length(metadata_ids)) {
      column_candidates <- setdiff(numeric_columns, feature_id_field)
      row_candidates <- if (nzchar(sample_id_field) && sample_id_field %in% names(expression_frame)) {
        as.character(expression_frame[[sample_id_field]])
      } else if (meaningful_rownames(expression_frame)) {
        row.names(expression_frame)
      } else character()
      column_rate <- if (length(column_candidates)) mean(column_candidates %in% metadata_ids) else 0
      row_rate <- if (length(row_candidates)) mean(row_candidates %in% metadata_ids) else 0
      if (column_rate >= 0.5 && column_rate > row_rate) {
        orientation <- "genes_by_samples"
      } else if (row_rate >= 0.5 && row_rate > column_rate) {
        orientation <- "samples_by_features"
      } else {
        abort("无法安全自动识别表达矩阵方向；请手动选择“基因 × 样本”或“样本 × 基因”。")
      }
    } else if (nrow(expression_frame) >= 2 * ncol(expression_frame)) {
      orientation <- "genes_by_samples"
    } else if (ncol(expression_frame) >= 2 * nrow(expression_frame)) {
      orientation <- "samples_by_features"
    } else {
      abort("表达矩阵的行列规模接近，无法安全自动识别方向；请手动指定数据布局。")
    }
  }

  if (!orientation %in% c("genes_by_samples", "samples_by_features")) abort("不支持的表达矩阵方向。")
  feature_id_location <- as.character(value("feature_id_location", "auto"))
  sample_id_location <- as.character(value("expression_sample_id_location", "auto"))
  excluded_columns <- character()
  feature_ids <- character()

  if (identical(orientation, "genes_by_samples")) {
    if (identical(feature_id_location, "column") && nzchar(feature_id_field)) {
      if (!feature_id_field %in% names(expression_frame)) abort(paste0("表达数据中不存在特征 ID 字段 ‘", feature_id_field, "’。"))
      feature_ids <- as.character(expression_frame[[feature_id_field]])
      excluded_columns <- c(excluded_columns, feature_id_field)
    } else if (identical(feature_id_location, "rownames") || (identical(feature_id_location, "auto") && meaningful_rownames(expression_frame))) {
      feature_ids <- row.names(expression_frame)
    } else if (identical(feature_id_location, "auto")) {
      candidates <- setdiff(names(expression_frame)[!vapply(expression_frame, is.numeric, logical(1))], sample_id_field)
      unique_candidates <- candidates[vapply(candidates, function(name) !anyNA(expression_frame[[name]]) && !anyDuplicated(expression_frame[[name]]), logical(1))]
      if (length(unique_candidates)) {
        feature_id_field <- unique_candidates[[1]]
        feature_ids <- as.character(expression_frame[[feature_id_field]])
        excluded_columns <- c(excluded_columns, feature_id_field)
      }
    }
    if (!length(feature_ids)) feature_ids <- paste0("Feature_", seq_len(nrow(expression_frame)))
    numeric_columns <- setdiff(names(expression_frame)[vapply(expression_frame, is.numeric, logical(1))], excluded_columns)
    if (length(numeric_columns) < 3L) abort("基因 × 样本矩阵至少需要 3 个数值型样本列。")
    non_numeric <- setdiff(names(expression_frame), c(numeric_columns, excluded_columns))
    if (length(non_numeric)) warnings <- c(warnings, paste0("已排除非数值表达字段：", paste(non_numeric, collapse = "、"), "。"))
    expression_matrix <- data.matrix(expression_frame[, numeric_columns, drop = FALSE])
    row.names(expression_matrix) <- make.unique(feature_ids)
    pca_input <- t(expression_matrix)
    sample_ids <- colnames(expression_matrix)
  } else {
    if (identical(sample_id_location, "column") && nzchar(sample_id_field)) {
      if (!sample_id_field %in% names(expression_frame)) abort(paste0("表达数据中不存在样本 ID 字段 ‘", sample_id_field, "’。"))
      sample_ids <- as.character(expression_frame[[sample_id_field]])
      excluded_columns <- c(excluded_columns, sample_id_field)
    } else if (identical(sample_id_location, "rownames") || (identical(sample_id_location, "auto") && meaningful_rownames(expression_frame))) {
      sample_ids <- row.names(expression_frame)
    } else if (identical(sample_id_location, "auto")) {
      candidates <- names(expression_frame)[!vapply(expression_frame, is.numeric, logical(1))]
      unique_candidates <- candidates[vapply(candidates, function(name) !anyNA(expression_frame[[name]]) && !anyDuplicated(expression_frame[[name]]), logical(1))]
      if (length(unique_candidates)) {
        sample_id_field <- unique_candidates[[1]]
        sample_ids <- as.character(expression_frame[[sample_id_field]])
        excluded_columns <- c(excluded_columns, sample_id_field)
      } else {
        sample_ids <- row.names(expression_frame)
      }
    } else {
      sample_ids <- row.names(expression_frame)
    }
    numeric_columns <- setdiff(names(expression_frame)[vapply(expression_frame, is.numeric, logical(1))], excluded_columns)
    if (length(numeric_columns) < 2L) abort("样本 × 特征矩阵至少需要 2 个数值型特征列。")
    non_numeric <- setdiff(names(expression_frame), c(numeric_columns, excluded_columns))
    if (length(non_numeric)) warnings <- c(warnings, paste0("已排除非数值表达字段：", paste(non_numeric, collapse = "、"), "。"))
    pca_input <- data.matrix(expression_frame[, numeric_columns, drop = FALSE])
    row.names(pca_input) <- sample_ids
    feature_ids <- colnames(pca_input)
  }

  sample_ids <- trimws(as.character(sample_ids))
  diagnostics$expression_samples <- length(sample_ids)
  if (any(!nzchar(sample_ids) | is.na(sample_ids))) abort("表达矩阵中存在空的样本 ID。")
  diagnostics$duplicate_expression_ids <- duplicate_values(sample_ids)
  if (length(diagnostics$duplicate_expression_ids)) {
    abort(paste0("表达矩阵中存在重复样本 ID：", collapse_ids(diagnostics$duplicate_expression_ids), "。"))
  }
  if (length(sample_ids) < 3L) abort("PCA 至少需要 3 个样本。")

  matched_metadata <- NULL
  if (!is.null(metadata_frame)) {
    metadata_index <- match(sample_ids, metadata_ids)
    diagnostics$expression_only <- sample_ids[is.na(metadata_index)]
    diagnostics$metadata_only <- metadata_ids[!metadata_ids %in% sample_ids]
    diagnostics$matched_samples <- sum(!is.na(metadata_index))
    policy <- as.character(value("unmatched_sample_policy", "strict"))
    if ((length(diagnostics$expression_only) || length(diagnostics$metadata_only)) && identical(policy, "strict")) {
      detail <- c(
        if (length(diagnostics$expression_only)) paste0("表达矩阵独有：", collapse_ids(diagnostics$expression_only), "。"),
        if (length(diagnostics$metadata_only)) paste0("分组表独有：", collapse_ids(diagnostics$metadata_only), "。")
      )
      abort(paste0("样本 ID 未完全匹配。", paste(detail, collapse = " "), "请选择“仅使用成功匹配的样本”或修正 ID。"))
    }
    keep <- !is.na(metadata_index)
    if (!all(keep)) {
      pca_input <- pca_input[keep, , drop = FALSE]
      sample_ids <- sample_ids[keep]
      metadata_index <- metadata_index[keep]
      warnings <- c(warnings, paste0("仅使用了 ", sum(keep), " 个成功匹配的样本。"))
    }
    if (length(sample_ids) < 3L) abort("成功匹配的样本少于 3 个，无法运行 PCA。")
    matched_metadata <- metadata_frame[metadata_index, , drop = FALSE]
    row.names(matched_metadata) <- sample_ids
  } else {
    diagnostics$matched_samples <- length(sample_ids)
  }

  all_missing <- vapply(seq_len(ncol(pca_input)), function(index) all(is.na(pca_input[, index])), logical(1))
  if (any(all_missing)) {
    warnings <- c(warnings, paste0("已移除 ", sum(all_missing), " 个全为缺失值的特征。"))
    pca_input <- pca_input[, !all_missing, drop = FALSE]
  }
  if (anyNA(pca_input)) {
    if (identical(as.character(value("missing_value_policy", "stop")), "omit_features")) {
      invalid <- vapply(seq_len(ncol(pca_input)), function(index) anyNA(pca_input[, index]), logical(1))
      warnings <- c(warnings, paste0("已移除 ", sum(invalid), " 个包含缺失值的特征。"))
      pca_input <- pca_input[, !invalid, drop = FALSE]
    } else {
      abort("表达矩阵包含缺失值；请选择删除含缺失值的特征，或先处理缺失数据。")
    }
  }
  if (ncol(pca_input) < 2L) abort("缺失值处理后不足 2 个有效特征。")

  transform <- as.character(value("transform", "auto"))
  transform_applied <- transform
  if (identical(transform, "auto")) transform_applied <- if (all(pca_input >= 0, na.rm = TRUE)) "log2p1" else "none"
  if (identical(transform_applied, "log2p1")) {
    if (any(pca_input < 0, na.rm = TRUE)) abort("数据包含负值，不能应用 log2(x + 1) 转换。")
    pca_input <- log2(pca_input + 1)
  }

  feature_variance <- apply(pca_input, 2, stats::var, na.rm = TRUE)
  zero_variance <- !is.finite(feature_variance) | feature_variance <= 0
  zero_variance_removed <- 0L
  if (isTRUE(value("remove_zero_variance", TRUE))) {
    zero_variance_removed <- sum(zero_variance)
    if (zero_variance_removed) warnings <- c(warnings, paste0("已移除 ", zero_variance_removed, " 个零方差特征。"))
    pca_input <- pca_input[, !zero_variance, drop = FALSE]
    feature_variance <- feature_variance[!zero_variance]
  } else if (isTRUE(value("scale", FALSE)) && any(zero_variance)) {
    abort("启用特征标准化时必须移除零方差特征。")
  }
  if (ncol(pca_input) < 2L) abort("至少需要 2 个非零方差特征才能运行 PCA。")

  feature_count <- value("variable_feature_count", 1000L)
  if (!(is.character(feature_count) && identical(feature_count, "all"))) {
    feature_count <- suppressWarnings(as.integer(feature_count))
    if (!is.finite(feature_count) || feature_count < 2L) feature_count <- 1000L
    order_index <- order(feature_variance, decreasing = TRUE)
    selected <- utils::head(order_index, min(feature_count, length(order_index)))
    pca_input <- pca_input[, selected, drop = FALSE]
    feature_variance <- feature_variance[selected]
  }

  pca_result <- stats::prcomp(
    pca_input,
    center = isTRUE(value("center", TRUE)),
    scale. = isTRUE(value("scale", FALSE))
  )
  scores <- as.data.frame(pca_result$x, check.names = FALSE)
  scores$Sample <- row.names(scores)
  scores <- scores[, c("Sample", setdiff(names(scores), "Sample")), drop = FALSE]
  if (!is.null(matched_metadata)) {
    metadata_columns <- setdiff(names(matched_metadata), metadata_id_field)
    if (length(metadata_columns)) {
      metadata_to_add <- matched_metadata[, metadata_columns, drop = FALSE]
      names(metadata_to_add) <- make.unique(c(names(scores), names(metadata_to_add)))[seq_along(metadata_to_add) + ncol(scores)]
      scores <- cbind(scores, metadata_to_add)
    }
  }
  loadings <- as.data.frame(pca_result$rotation, check.names = FALSE)
  loadings$Feature <- row.names(loadings)
  loadings <- loadings[, c("Feature", setdiff(names(loadings), "Feature")), drop = FALSE]
  explained <- 100 * pca_result$sdev^2 / sum(pca_result$sdev^2)
  names(explained) <- paste0("PC", seq_along(explained))
  list(
    ok = TRUE,
    scores = scores,
    loadings = loadings,
    explained_variance = explained,
    diagnostics = diagnostics,
    warnings = unique(warnings),
    orientation = orientation,
    transform_applied = transform_applied,
    selected_feature_count = ncol(pca_input),
    zero_variance_removed = zero_variance_removed,
    metadata_id_field = metadata_id_field
  )
}

bp_compute_pca <- function(expression_data, metadata_data = NULL, config = list()) {
  tryCatch(
    bp_pca_compute_core(expression_data, metadata_data, bp_normalize_pca_config(config)),
    error = function(error) list(
      ok = FALSE,
      error = conditionMessage(error),
      diagnostics = error$diagnostics %||% list(),
      warnings = error$warnings %||% character()
    )
  )
}

bp_pca_derived_source <- function(id, name, kind, config, result = NULL) {
  data <- if (!is.null(result) && isTRUE(result$ok)) result[[kind]] else NULL
  list(
    id = id,
    name = name,
    source_type = "derived_pca",
    original_file_name = "Computed from PCA inputs",
    object_type = "data.frame",
    object_name = name,
    rows = if (is.data.frame(data)) nrow(data) else 0L,
    columns = if (is.data.frame(data)) ncol(data) else 0L,
    status = if (is.data.frame(data)) "ready" else "derived_stale",
    example = FALSE,
    derived = TRUE,
    readonly = TRUE,
    relink_required = FALSE,
    derived_kind = kind,
    input_source_ids = Filter(nzchar, c(config$expression_source_id, config$metadata_source_id)),
    column_metadata = if (is.data.frame(data)) bp_profile_dataset(data)$column_metadata else list(),
    quality = list(warnings = list()),
    parse_options = list()
  )
}

bp_pca_upsert_derived_sources <- function(project, config, result = NULL) {
  project <- unserialize(serialize(project, NULL))
  config <- bp_normalize_pca_config(config, project)
  reserved <- c(pca_scores = "dataset_pca_scores", pca_loadings = "dataset_pca_loadings")
  conflicts <- Filter(function(source) {
    source$name %in% names(reserved) && !isTRUE(source$derived) && !source$id %in% unname(reserved)
  }, project$data_sources %||% list())
  if (length(conflicts)) stop("数据源名称 pca_scores 和 pca_loadings 为 PCA 派生结果保留，请先重命名冲突的数据源。", call. = FALSE)
  project$data_sources <- Filter(function(source) !source$id %in% unname(reserved), project$data_sources %||% list())
  project$data_sources <- c(project$data_sources, list(
    bp_pca_derived_source(reserved[["pca_scores"]], "pca_scores", "scores", config, result),
    bp_pca_derived_source(reserved[["pca_loadings"]], "pca_loadings", "loadings", config, result)
  ))
  project
}

bp_pca_config_from_project <- function(project) {
  bp_normalize_pca_config(project$visual_config$pca %||% list(), project)
}

bp_pca_source <- function(project, source_id) {
  sources <- Filter(function(source) identical(source$id, source_id), project$data_sources %||% list())
  if (length(sources)) sources[[1]] else if (identical(source_id, "dataset_example")) bp_example_data_source() else NULL
}

bp_generate_pca_analysis_code <- function(project) {
  if (!identical(project$visual_config$active_chart_type %||% "scatter", "pca")) return("")
  config <- bp_pca_config_from_project(project)
  expression_source <- bp_pca_source(project, config$expression_source_id)
  if (is.null(expression_source)) stop("PCA expression data source is not registered.", call. = FALSE)
  metadata_source <- if (nzchar(config$metadata_source_id)) bp_pca_source(project, config$metadata_source_id) else NULL
  if (nzchar(config$metadata_source_id) && is.null(metadata_source)) stop("PCA metadata data source is not registered.", call. = FALSE)
  config_source <- paste(capture.output(dput(config)), collapse = "\n")
  core_source <- paste(deparse(bp_pca_compute_core, width.cutoff = 120L), collapse = "\n")
  metadata_symbol <- if (is.null(metadata_source)) "NULL" else bp_symbol_source_name(metadata_source$name)
  paste(
    "# PCA analysis preparation (generated by BioPlotBlocks)",
    paste0(".bioplotblocks_compute_pca <- ", core_source),
    paste0(".pca_config <- ", config_source),
    paste0(
      ".pca_analysis <- .bioplotblocks_compute_pca(",
      bp_symbol_source_name(expression_source$name), ", ", metadata_symbol, ", .pca_config)"
    ),
    "pca_scores <- .pca_analysis$scores",
    "pca_loadings <- .pca_analysis$loadings",
    "explained_variance <- .pca_analysis$explained_variance",
    "rm(.pca_analysis, .pca_config, .bioplotblocks_compute_pca)",
    sep = "\n"
  )
}

bp_pca_setup_lines <- function(project) {
  if (!identical(project$visual_config$active_chart_type %||% "scatter", "pca")) return(character())
  config <- bp_pca_config_from_project(project)
  ids <- unique(Filter(nzchar, c(config$expression_source_id, config$metadata_source_id)))
  sources <- project$data_sources %||% list()
  unlist(lapply(ids, function(id) {
    source <- bp_pca_source(project, id)
    if (is.null(source) || isTRUE(source$example) || isTRUE(source$derived)) return(character())
    bp_data_source_setup_line(source)
  }), use.names = FALSE)
}
