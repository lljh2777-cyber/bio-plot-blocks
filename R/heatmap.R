# Heatmap analysis -----------------------------------------------------------

bp_heatmap_defaults <- function(project = NULL) {
  active_source_id <- project$active_data_source_id %||% "dataset_example"
  active_source <- Filter(function(source) identical(source$id, active_source_id), project$data_sources %||% list())
  active_semantic <- if (length(active_source)) active_source[[1]]$semantic_type %||% active_source[[1]]$semantic_suggestion %||% "generic_table" else "generic_table"
  if (length(active_source) && (isTRUE(active_source[[1]]$derived) || active_semantic %in% c("differential_results", "sample_metadata"))) {
    expression_candidates <- Filter(function(source) {
      !isTRUE(source$derived) && (source$semantic_type %||% source$semantic_suggestion %||% "") %in% c("raw_counts", "normalized_expression")
    }, project$data_sources %||% list())
    active_source_id <- if (length(expression_candidates)) expression_candidates[[1]]$id else "dataset_example"
  }
  rna_mode <- identical(project$analysis_workflow_mode %||% "generic", "rna_seq")
  differential_sources <- Filter(function(source) {
    !isTRUE(source$derived) && identical(source$semantic_type %||% source$semantic_suggestion %||% "", "differential_results")
  }, project$data_sources %||% list())
  metadata_sources <- Filter(function(source) {
    !isTRUE(source$derived) && identical(source$semantic_type %||% source$semantic_suggestion %||% "", "sample_metadata")
  }, project$data_sources %||% list())
  list(
    chart_type = "heatmap",
    expression_source_id = active_source_id,
    metadata_source_id = if (rna_mode && length(metadata_sources)) metadata_sources[[1]]$id else "",
    feature_selection_mode = if (rna_mode) "differential_results" else "high_variance",
    differential_source_id = if (length(differential_sources)) differential_sources[[1]]$id else "",
    differential_gene_id_field = "",
    differential_status_field = "",
    differential_exclude_values = c("normal", "NS", "not significant", "non-significant"),
    expression_orientation = "auto",
    feature_id_location = "auto",
    feature_id_field = "",
    expression_sample_id_location = "auto",
    expression_sample_id_field = "",
    metadata_sample_id_field = "",
    unmatched_sample_policy = "strict",
    input_semantic_type = "generic_table",
    raw_count_filter_cpm = 0.5,
    raw_count_filter_min_samples = 2L,
    raw_count_normalization = bp_raw_count_default_normalization(),
    raw_count_prior_count = 2,
    raw_count_recipe_confirmed_signature = "",
    differential_match_confirmed_signature = "",
    transform = "none",
    variable_feature_count = 50L,
    custom_feature_count = 50L,
    remove_zero_variance = TRUE,
    missing_value_policy = "stop",
    row_zscore = TRUE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    sample_order = "auto_cluster",
    group_field = "",
    annotation_fields = character(),
    show_sample_names = FALSE,
    show_feature_names = FALSE,
    low_color = "#4575B4",
    mid_color = "#FFFFBF",
    high_color = "#D73027",
    title = if (rna_mode) "DEG heatmap" else "Expression heatmap",
    legend_title = "Gene Z-score",
    base_size = 9,
    advanced_preserved = FALSE
  )
}

bp_normalize_heatmap_config <- function(config, project = NULL) {
  defaults <- bp_heatmap_defaults(project)
  config <- utils::modifyList(defaults, config %||% list(), keep.null = TRUE)
  character_fields <- c(
    "chart_type", "expression_source_id", "metadata_source_id", "expression_orientation",
    "feature_id_location", "feature_id_field", "expression_sample_id_location",
    "expression_sample_id_field", "metadata_sample_id_field", "unmatched_sample_policy",
    "feature_selection_mode", "differential_source_id", "differential_gene_id_field",
    "differential_status_field",
    "input_semantic_type", "raw_count_normalization", "raw_count_recipe_confirmed_signature",
    "differential_match_confirmed_signature",
    "transform", "missing_value_policy", "sample_order", "group_field", "low_color",
    "mid_color", "high_color", "title", "legend_title"
  )
  for (name in character_fields) {
    value <- config[[name]]
    config[[name]] <- if (is.null(value) || !length(value) || is.na(value[[1]])) defaults[[name]] else as.character(value[[1]])
  }
  expression_source <- Filter(function(source) identical(source$id, config$expression_source_id), project$data_sources %||% list())
  if (!nzchar(config$expression_source_id) || (length(expression_source) && isTRUE(expression_source[[1]]$derived))) {
    config$expression_source_id <- defaults$expression_source_id
  }
  config$chart_type <- "heatmap"
  if (!config$expression_orientation %in% c("auto", "genes_by_samples", "samples_by_features")) config$expression_orientation <- "auto"
  if (!config$feature_id_location %in% c("auto", "rownames", "column", "none")) config$feature_id_location <- "auto"
  if (!config$expression_sample_id_location %in% c("auto", "column_names", "rownames", "column")) config$expression_sample_id_location <- "auto"
  if (!config$unmatched_sample_policy %in% c("strict", "matched_only")) config$unmatched_sample_policy <- "strict"
  if (!config$feature_selection_mode %in% c("differential_results", "high_variance", "all")) config$feature_selection_mode <- defaults$feature_selection_mode
  if (!config$input_semantic_type %in% c("generic_table", "raw_counts", "normalized_expression", "unconfirmed_raw_counts", "heatmap_matrix")) config$input_semantic_type <- "generic_table"
  if (!config$raw_count_normalization %in% c("tmm_logcpm", "log2p1")) config$raw_count_normalization <- defaults$raw_count_normalization
  if (!config$transform %in% c("none", "log2p1")) config$transform <- defaults$transform
  if (!config$missing_value_policy %in% c("stop", "omit_features")) config$missing_value_policy <- defaults$missing_value_policy
  if (!config$sample_order %in% c("auto_cluster", "group_fixed", "group_split")) config$sample_order <- defaults$sample_order

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
  config$base_size <- number(config$base_size, defaults$base_size, 6, 30)
  config$raw_count_filter_cpm <- number(config$raw_count_filter_cpm, defaults$raw_count_filter_cpm, 0, 1e6)
  config$raw_count_prior_count <- number(config$raw_count_prior_count, defaults$raw_count_prior_count, .Machine$double.eps, 1e6)
  min_samples <- suppressWarnings(as.integer(config$raw_count_filter_min_samples %||% defaults$raw_count_filter_min_samples))
  if (!length(min_samples) || is.na(min_samples[[1]]) || min_samples[[1]] < 1L) min_samples <- defaults$raw_count_filter_min_samples
  config$raw_count_filter_min_samples <- min_samples[[1]]
  config$remove_zero_variance <- if (is.null(config$remove_zero_variance)) TRUE else isTRUE(config$remove_zero_variance)
  config$row_zscore <- if (is.null(config$row_zscore)) TRUE else isTRUE(config$row_zscore)
  if (config$input_semantic_type %in% c("raw_counts", "unconfirmed_raw_counts")) config$row_zscore <- TRUE
  config$cluster_rows <- if (is.null(config$cluster_rows)) TRUE else isTRUE(config$cluster_rows)
  config$cluster_columns <- if (is.null(config$cluster_columns)) TRUE else isTRUE(config$cluster_columns)
  config$show_sample_names <- if (is.null(config$show_sample_names)) TRUE else isTRUE(config$show_sample_names)
  config$show_feature_names <- if (is.null(config$show_feature_names)) TRUE else isTRUE(config$show_feature_names)
  config$advanced_preserved <- isTRUE(config$advanced_preserved)
  annotation_fields <- as.character(config$annotation_fields %||% character())
  config$annotation_fields <- unique(annotation_fields[nzchar(annotation_fields) & !is.na(annotation_fields)])
  exclude_values <- as.character(config$differential_exclude_values %||% defaults$differential_exclude_values)
  config$differential_exclude_values <- unique(trimws(exclude_values[nzchar(trimws(exclude_values)) & !is.na(exclude_values)]))
  for (name in c("low_color", "mid_color", "high_color")) {
    if (!grepl("^#[0-9A-Fa-f]{6}$", config[[name]])) config[[name]] <- defaults[[name]]
  }
  config
}

bp_heatmap_recipe_signature <- function(config) {
  fields <- c(
    "expression_source_id", "expression_orientation",
    "feature_id_location", "feature_id_field", "expression_sample_id_location",
    "expression_sample_id_field",
    "raw_count_filter_cpm", "raw_count_filter_min_samples", "raw_count_normalization",
    "raw_count_prior_count"
  )
  values <- vapply(fields, function(name) paste(config[[name]] %||% "", collapse = ","), character(1))
  edge_r_version <- if (identical(config$raw_count_normalization %||% "", "tmm_logcpm") && requireNamespace("edgeR", quietly = TRUE)) {
    as.character(utils::packageVersion("edgeR"))
  } else "base"
  paste(c("raw-count-heatmap-preprocess-v0.2", values, as.character(getRversion()), edge_r_version), collapse = "|")
}

bp_heatmap_deg_match_signature <- function(config) {
  fields <- c(
    "expression_source_id", "feature_selection_mode", "differential_source_id",
    "differential_gene_id_field", "differential_status_field", "differential_exclude_values",
    "missing_value_policy", "remove_zero_variance"
  )
  values <- vapply(fields, function(name) paste(config[[name]] %||% "", collapse = ","), character(1))
  paste(c("heatmap-deg-match-v0.1", bp_heatmap_recipe_signature(config), values), collapse = "|")
}

# This function uses base/stats R only so its body can be embedded in exported scripts.
bp_heatmap_compute_core <- function(expression_data, metadata_data = NULL, config = list(), differential_data = NULL) {
  value <- function(name, default = NULL) {
    item <- config[[name]]
    if (is.null(item) || !length(item) || is.na(item[[1]])) default else item
  }
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
  cluster_order <- function(matrix_by_feature) {
    if (ncol(matrix_by_feature) < 2L) return(seq_len(ncol(matrix_by_feature)))
    stats::hclust(stats::dist(t(matrix_by_feature)))$order
  }
  match_name <- function(candidates, names_value) {
    if (!length(names_value)) return("")
    normalized <- tolower(gsub("[^a-z0-9]", "", names_value))
    wanted <- tolower(gsub("[^a-z0-9]", "", candidates))
    index <- match(wanted, normalized, nomatch = 0L)
    index <- index[index > 0L]
    if (length(index)) names_value[[index[[1]]]] else ""
  }
  warnings <- character()
  diagnostics <- list(
    expression_samples = 0L, metadata_samples = 0L, matched_samples = 0L,
    expression_only = character(), metadata_only = character(),
    duplicate_expression_ids = character(), duplicate_metadata_ids = character(),
    differential_rows = 0L, significant_gene_ids = 0L, matched_gene_ids = 0L,
    unmatched_gene_ids = character(), differential_gene_id_field = "",
    differential_status_field = ""
  )

  if (!(is.data.frame(expression_data) || is.matrix(expression_data))) abort("热图表达数据必须是 data.frame 或二维矩阵。")
  frame <- as.data.frame(expression_data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  if (nrow(frame) < 2L || ncol(frame) < 2L) abort("热图表达矩阵至少需要 2 个特征和 2 个样本。")

  metadata_frame <- NULL
  metadata_ids <- character()
  metadata_id_field <- as.character(value("metadata_sample_id_field", "")[[1]])
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
    if (length(diagnostics$duplicate_metadata_ids)) abort(paste0("无法关联样本信息。字段 ", metadata_id_field, " 中存在重复样本 ID：", collapse_ids(diagnostics$duplicate_metadata_ids), "。"))
  }

  numeric_columns <- names(frame)[vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))]
  orientation <- as.character(value("expression_orientation", "auto")[[1]])
  feature_field <- as.character(value("feature_id_field", "")[[1]])
  sample_field <- as.character(value("expression_sample_id_field", "")[[1]])
  if (identical(orientation, "auto")) {
    if (length(metadata_ids)) {
      column_candidates <- setdiff(numeric_columns, feature_field)
      row_candidates <- if (nzchar(sample_field) && sample_field %in% names(frame)) {
        as.character(frame[[sample_field]])
      } else if (meaningful_rownames(frame)) row.names(frame) else character()
      column_rate <- if (length(column_candidates)) mean(column_candidates %in% metadata_ids) else 0
      row_rate <- if (length(row_candidates)) mean(row_candidates %in% metadata_ids) else 0
      if (column_rate >= 0.5 && column_rate > row_rate) orientation <- "genes_by_samples"
      else if (row_rate >= 0.5 && row_rate > column_rate) orientation <- "samples_by_features"
      else abort("无法安全自动识别表达矩阵方向；请手动选择“基因 × 样本”或“样本 × 特征”。")
    } else if (nrow(frame) >= 2L * max(1L, length(numeric_columns))) orientation <- "genes_by_samples"
    else if (length(numeric_columns) >= 2L * max(1L, nrow(frame))) orientation <- "samples_by_features"
    else abort("表达矩阵的行列规模接近，无法安全自动识别方向；请手动指定数据布局。")
  }
  if (!orientation %in% c("genes_by_samples", "samples_by_features")) abort("不支持的热图矩阵方向。")

  if (identical(orientation, "genes_by_samples")) {
    feature_ids <- character()
    excluded <- character()
    if (nzchar(feature_field)) {
      if (!feature_field %in% names(frame)) abort(paste0("表达数据中不存在特征 ID 字段 ‘", feature_field, "’。"))
      feature_ids <- trimws(as.character(frame[[feature_field]]))
      excluded <- feature_field
    } else if (meaningful_rownames(frame)) {
      feature_ids <- trimws(row.names(frame))
    } else {
      candidates <- names(frame)[!vapply(frame, is.numeric, logical(1))]
      unique_candidates <- candidates[vapply(candidates, function(name) !anyNA(frame[[name]]) && !anyDuplicated(as.character(frame[[name]])), logical(1))]
      if (length(unique_candidates)) {
        feature_field <- unique_candidates[[1]]
        feature_ids <- trimws(as.character(frame[[feature_field]]))
        excluded <- feature_field
      }
    }
    if (!length(feature_ids)) feature_ids <- paste0("Feature_", seq_len(nrow(frame)))
    sample_columns <- setdiff(names(frame)[vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))], excluded)
    if (length(sample_columns) < 2L) abort("基因 × 样本热图矩阵至少需要 2 个数值型样本列。")
    expression_matrix <- data.matrix(frame[, sample_columns, drop = FALSE])
    row.names(expression_matrix) <- feature_ids
    sample_ids <- colnames(expression_matrix)
  } else {
    excluded <- character()
    if (nzchar(sample_field)) {
      if (!sample_field %in% names(frame)) abort(paste0("表达数据中不存在样本 ID 字段 ‘", sample_field, "’。"))
      sample_ids <- trimws(as.character(frame[[sample_field]]))
      excluded <- sample_field
    } else if (meaningful_rownames(frame)) {
      sample_ids <- trimws(row.names(frame))
    } else {
      candidates <- names(frame)[!vapply(frame, is.numeric, logical(1))]
      unique_candidates <- candidates[vapply(candidates, function(name) !anyNA(frame[[name]]) && !anyDuplicated(as.character(frame[[name]])), logical(1))]
      if (length(unique_candidates)) {
        sample_field <- unique_candidates[[1]]
        sample_ids <- trimws(as.character(frame[[sample_field]]))
        excluded <- sample_field
      } else sample_ids <- row.names(frame)
    }
    feature_columns <- setdiff(names(frame)[vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))], excluded)
    if (length(feature_columns) < 2L || nrow(frame) < 2L) abort("样本 × 特征热图矩阵至少需要 2 个样本和 2 个数值型特征。")
    expression_matrix <- t(data.matrix(frame[, feature_columns, drop = FALSE]))
    colnames(expression_matrix) <- sample_ids
  }

  feature_ids <- trimws(as.character(row.names(expression_matrix)))
  sample_ids <- trimws(as.character(colnames(expression_matrix)))
  if (any(!nzchar(feature_ids) | is.na(feature_ids))) abort("表达矩阵中存在空的特征 ID。")
  if (any(!nzchar(sample_ids) | is.na(sample_ids))) abort("表达矩阵中存在空的样本 ID。")
  if (length(duplicate_values(feature_ids))) abort(paste0("表达矩阵中存在重复特征 ID：", collapse_ids(duplicate_values(feature_ids)), "。"))
  diagnostics$duplicate_expression_ids <- duplicate_values(sample_ids)
  if (length(diagnostics$duplicate_expression_ids)) abort(paste0("表达矩阵中存在重复样本 ID：", collapse_ids(diagnostics$duplicate_expression_ids), "。"))
  diagnostics$expression_samples <- length(sample_ids)
  if (length(sample_ids) < 2L) abort("热图至少需要 2 个样本。")

  matched_metadata <- NULL
  if (!is.null(metadata_frame)) {
    metadata_index <- match(sample_ids, metadata_ids)
    diagnostics$expression_only <- sample_ids[is.na(metadata_index)]
    diagnostics$metadata_only <- metadata_ids[!metadata_ids %in% sample_ids]
    diagnostics$matched_samples <- sum(!is.na(metadata_index))
    policy <- as.character(value("unmatched_sample_policy", "strict")[[1]])
    if ((length(diagnostics$expression_only) || length(diagnostics$metadata_only)) && identical(policy, "strict")) {
      detail <- c(
        if (length(diagnostics$expression_only)) paste0("表达矩阵独有：", collapse_ids(diagnostics$expression_only), "。"),
        if (length(diagnostics$metadata_only)) paste0("样本信息独有：", collapse_ids(diagnostics$metadata_only), "。")
      )
      abort(paste0("样本 ID 未完全匹配。", paste(detail, collapse = " "), "请选择“仅使用成功匹配的样本”或修正 ID。"))
    }
    keep <- !is.na(metadata_index)
    if (!all(keep)) {
      expression_matrix <- expression_matrix[, keep, drop = FALSE]
      sample_ids <- sample_ids[keep]
      metadata_index <- metadata_index[keep]
      warnings <- c(warnings, paste0("仅使用了 ", sum(keep), " 个成功匹配的样本。"))
    }
    if (length(sample_ids) < 2L) abort("成功匹配的样本少于 2 个，无法绘制热图。")
    matched_metadata <- metadata_frame[metadata_index, , drop = FALSE]
    row.names(matched_metadata) <- sample_ids
  } else diagnostics$matched_samples <- length(sample_ids)

  feature_selection_mode <- as.character(value("feature_selection_mode", "high_variance")[[1]])
  if (identical(feature_selection_mode, "differential_results")) {
    if (is.null(differential_data) || !(is.data.frame(differential_data) || is.matrix(differential_data))) {
      abort("请选择差异分析结果数据源，以便从表达矩阵中提取显著基因。")
    }
    differential_frame <- as.data.frame(differential_data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
    diagnostics$differential_rows <- nrow(differential_frame)
    if (!nrow(differential_frame)) abort("差异分析结果为空，无法筛选热图基因。")
    gene_field <- as.character(value("differential_gene_id_field", "")[[1]])
    if (nzchar(gene_field) && !gene_field %in% names(differential_frame)) {
      abort(paste0("差异分析结果中不存在基因 ID 字段 ‘", gene_field, "’。"))
    }
    if (!nzchar(gene_field)) {
      gene_field <- match_name(
        c("ENSEMBL", "gene_id", "geneid", "gene", "SYMBOL", "feature_id", "feature", "id"),
        names(differential_frame)
      )
    }
    if (nzchar(gene_field)) {
      differential_gene_ids <- trimws(as.character(differential_frame[[gene_field]]))
    } else if (meaningful_rownames(differential_frame)) {
      gene_field <- ".rownames"
      differential_gene_ids <- trimws(row.names(differential_frame))
    } else {
      abort("无法自动识别差异分析结果中的基因 ID；请选择 ENSEMBL、SYMBOL 或其他基因 ID 字段。")
    }
    status_field <- as.character(value("differential_status_field", "")[[1]])
    if (nzchar(status_field) && !status_field %in% names(differential_frame)) {
      abort(paste0("差异分析结果中不存在状态字段 ‘", status_field, "’。"))
    }
    if (!nzchar(status_field)) {
      status_field <- match_name(
        c("regulated", "regulation", "status", "significance", "significant", "direction"),
        names(differential_frame)
      )
    }
    selected <- !is.na(differential_gene_ids) & nzchar(differential_gene_ids)
    excluded_values <- tolower(trimws(as.character(value(
      "differential_exclude_values", c("normal", "NS", "not significant", "non-significant")
    ))))
    excluded_values <- excluded_values[nzchar(excluded_values)]
    if (nzchar(status_field)) {
      status_values <- tolower(trimws(as.character(differential_frame[[status_field]])))
      selected <- selected & !is.na(status_values) & !status_values %in% excluded_values
    } else {
      warnings <- c(warnings, "差异结果未识别到状态字段；已将其中全部基因作为热图候选。")
    }
    significant_ids <- unique(differential_gene_ids[selected])
    diagnostics$significant_gene_ids <- length(significant_ids)
    diagnostics$differential_gene_id_field <- gene_field
    diagnostics$differential_status_field <- status_field
    if (!length(significant_ids)) abort("差异分析结果中没有通过当前状态筛选规则的基因。")
    matched_ids <- significant_ids[significant_ids %in% row.names(expression_matrix)]
    diagnostics$matched_gene_ids <- length(matched_ids)
    diagnostics$unmatched_gene_ids <- setdiff(significant_ids, row.names(expression_matrix))
    if (!length(matched_ids)) {
      abort("差异基因 ID 与表达矩阵的基因 ID 完全不匹配；请检查两侧使用的是 ENSEMBL、SYMBOL 还是其他标识。")
    }
    if (length(diagnostics$unmatched_gene_ids)) {
      warnings <- c(warnings, paste0(
        "差异结果中的 ", length(diagnostics$unmatched_gene_ids),
        " 个显著基因未在表达矩阵中找到；已使用 ", length(matched_ids), " 个成功匹配的基因。"
      ))
    }
    expression_matrix <- expression_matrix[matched_ids, , drop = FALSE]
  }

  all_missing <- apply(expression_matrix, 1L, function(row) all(is.na(row)))
  if (any(all_missing)) {
    warnings <- c(warnings, paste0("已移除 ", sum(all_missing), " 个全为缺失值的特征。"))
    expression_matrix <- expression_matrix[!all_missing, , drop = FALSE]
  }
  if (anyNA(expression_matrix)) {
    if (identical(as.character(value("missing_value_policy", "stop")[[1]]), "omit_features")) {
      invalid <- apply(expression_matrix, 1L, function(row) anyNA(row))
      warnings <- c(warnings, paste0("已移除 ", sum(invalid), " 个包含缺失值的特征。"))
      expression_matrix <- expression_matrix[!invalid, , drop = FALSE]
    } else abort("表达矩阵包含缺失值；请选择移除含缺失值的特征，或先处理缺失数据。")
  }
  if (any(!is.finite(expression_matrix))) abort("表达矩阵包含非有限数值。")
  if (identical(as.character(value("transform", "none")[[1]]), "log2p1")) {
    if (any(expression_matrix < 0)) abort("数据包含负值，不能应用 log2(x + 1) 转换。")
    expression_matrix <- log2(expression_matrix + 1)
  }

  feature_variance <- apply(expression_matrix, 1L, stats::var)
  zero_variance <- !is.finite(feature_variance) | feature_variance <= 0
  zero_variance_removed <- 0L
  if (isTRUE(value("remove_zero_variance", TRUE)[[1]])) {
    zero_variance_removed <- sum(zero_variance)
    if (zero_variance_removed) warnings <- c(warnings, paste0("已移除 ", zero_variance_removed, " 个零方差特征。"))
    expression_matrix <- expression_matrix[!zero_variance, , drop = FALSE]
    feature_variance <- feature_variance[!zero_variance]
  }
  if (nrow(expression_matrix) < 2L) abort("至少需要 2 个非零方差特征才能绘制热图。")

  feature_count <- value("variable_feature_count", 50L)[[1]]
  if (identical(feature_selection_mode, "high_variance") && !(is.character(feature_count) && identical(feature_count, "all"))) {
    feature_count <- suppressWarnings(as.integer(feature_count))
    if (!is.finite(feature_count) || feature_count < 2L) feature_count <- 50L
    selected <- utils::head(order(feature_variance, decreasing = TRUE), min(feature_count, length(feature_variance)))
    expression_matrix <- expression_matrix[selected, , drop = FALSE]
  }

  z_matrix <- expression_matrix
  if (isTRUE(value("row_zscore", TRUE)[[1]])) {
    z_matrix <- t(scale(t(expression_matrix), center = TRUE, scale = TRUE))
    if (any(!is.finite(z_matrix))) abort("按基因 Z-score 后产生非有限值；请确认已移除零方差特征。")
  }
  feature_order <- row.names(z_matrix)
  if (isTRUE(value("cluster_rows", TRUE)[[1]]) && nrow(z_matrix) > 1L) {
    feature_order <- feature_order[stats::hclust(stats::dist(z_matrix))$order]
  }

  group_field <- as.character(value("group_field", "")[[1]])
  annotation_fields <- unique(as.character(value("annotation_fields", character())))
  if (!is.null(matched_metadata)) {
    available <- setdiff(names(matched_metadata), metadata_id_field)
    if (!group_field %in% available) group_field <- ""
    annotation_fields <- intersect(annotation_fields, available)
    if (nzchar(group_field)) annotation_fields <- unique(c(group_field, annotation_fields))
    if (!length(annotation_fields)) annotation_fields <- utils::head(available, 4L)
  } else {
    group_field <- ""
    annotation_fields <- character()
  }

  order_mode <- as.character(value("sample_order", "auto_cluster")[[1]])
  split_values <- rep("All samples", length(sample_ids))
  cluster_columns <- isTRUE(value("cluster_columns", TRUE)[[1]])
  if (is.null(matched_metadata) || identical(order_mode, "auto_cluster") || !nzchar(group_field)) {
    sample_index <- if (cluster_columns) cluster_order(z_matrix) else seq_len(ncol(z_matrix))
  } else {
    groups <- as.character(matched_metadata[[group_field]])
    group_levels <- unique(groups)
    if (identical(order_mode, "group_split")) {
      sample_index <- unlist(lapply(group_levels, function(level) {
        indices <- which(groups == level)
        if (!cluster_columns || length(indices) < 2L) indices else indices[cluster_order(z_matrix[, indices, drop = FALSE])]
      }), use.names = FALSE)
      split_values <- groups
    } else {
      sample_index <- order(match(groups, group_levels), seq_along(groups))
    }
  }
  sample_ids <- sample_ids[sample_index]
  z_matrix <- z_matrix[feature_order, sample_index, drop = FALSE]
  split_values <- split_values[sample_index]
  if (!is.null(matched_metadata)) matched_metadata <- matched_metadata[sample_index, , drop = FALSE]

  heatmap_matrix <- data.frame(Feature = row.names(z_matrix), z_matrix, check.names = FALSE, stringsAsFactors = FALSE)
  heatmap_long <- data.frame(
    Feature = rep(row.names(z_matrix), times = ncol(z_matrix)),
    Sample = rep(colnames(z_matrix), each = nrow(z_matrix)),
    Value = as.vector(z_matrix),
    Split = rep(split_values, each = nrow(z_matrix)),
    stringsAsFactors = FALSE
  )
  heatmap_long$Feature <- factor(heatmap_long$Feature, levels = rev(feature_order))
  heatmap_long$Sample <- factor(heatmap_long$Sample, levels = sample_ids)
  heatmap_long$Split <- factor(heatmap_long$Split, levels = unique(split_values))

  annotation_long <- data.frame(Sample = character(), Attribute = character(), Value = character(), Key = character(), Split = character(), stringsAsFactors = FALSE)
  annotation_frame <- NULL
  if (!is.null(matched_metadata) && length(annotation_fields)) {
    annotation_frame <- data.frame(Sample = sample_ids, matched_metadata[, annotation_fields, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
    row.names(annotation_frame) <- sample_ids
    annotation_long <- do.call(rbind, lapply(annotation_fields, function(field) {
      values <- as.character(matched_metadata[[field]])
      values[is.na(values) | !nzchar(values)] <- "NA"
      data.frame(
        Sample = sample_ids, Attribute = field, Value = values,
        Key = paste(field, values, sep = "::"), Split = split_values,
        stringsAsFactors = FALSE
      )
    }))
    annotation_long$Sample <- factor(annotation_long$Sample, levels = sample_ids)
    annotation_long$Attribute <- factor(annotation_long$Attribute, levels = rev(annotation_fields))
    annotation_long$Split <- factor(annotation_long$Split, levels = unique(split_values))
  }

  list(
    ok = TRUE,
    heatmap_matrix = heatmap_matrix,
    long = heatmap_long,
    annotations = annotation_long,
    metadata = annotation_frame,
    diagnostics = diagnostics,
    warnings = unique(warnings),
    orientation = orientation,
    selected_feature_count = nrow(z_matrix),
    sample_count = ncol(z_matrix),
    zero_variance_removed = zero_variance_removed,
    annotation_fields = annotation_fields,
    group_field = group_field,
    feature_selection_mode = feature_selection_mode,
    sample_order = if (is.null(matched_metadata)) "auto_cluster" else order_mode
  )
}

bp_heatmap_plot_core <- function(heatmap_matrix, heatmap_metadata = NULL, config = list()) {
  value <- function(name, default = NULL) {
    item <- config[[name]]
    if (is.null(item) || !length(item) || is.na(item[[1]])) default else item[[1]]
  }
  if (!requireNamespace("pheatmap", quietly = TRUE)) stop("The pheatmap package is required to draw expression heatmaps.", call. = FALSE)
  if (!requireNamespace("ggplotify", quietly = TRUE)) stop("The ggplotify package is required to place pheatmap output in the ggplot2 workspace.", call. = FALSE)
  frame <- as.data.frame(heatmap_matrix, check.names = FALSE, stringsAsFactors = FALSE)
  if (nrow(frame) < 2L || ncol(frame) < 3L) stop("The heatmap matrix must contain a feature column and at least two sample columns.", call. = FALSE)
  feature_field <- if ("Feature" %in% names(frame)) "Feature" else names(frame)[[1]]
  matrix_value <- data.matrix(frame[, setdiff(names(frame), feature_field), drop = FALSE])
  row.names(matrix_value) <- as.character(frame[[feature_field]])

  annotation_col <- NULL
  if (is.data.frame(heatmap_metadata) && nrow(heatmap_metadata)) {
    metadata <- as.data.frame(heatmap_metadata, check.names = FALSE, stringsAsFactors = FALSE)
    sample_ids <- if ("Sample" %in% names(metadata)) as.character(metadata$Sample) else row.names(metadata)
    annotation_fields <- setdiff(names(metadata), "Sample")
    if (length(annotation_fields)) {
      index <- match(colnames(matrix_value), sample_ids)
      if (anyNA(index)) stop("Heatmap sample annotations no longer match the expression matrix columns.", call. = FALSE)
      annotation_col <- metadata[index, annotation_fields, drop = FALSE]
      row.names(annotation_col) <- colnames(matrix_value)
      for (name in names(annotation_col)) {
        if (is.character(annotation_col[[name]])) annotation_col[[name]] <- factor(annotation_col[[name]], levels = unique(annotation_col[[name]]))
      }
    }
  }

  order_mode <- as.character(value("sample_order", "auto_cluster"))
  cluster_columns <- isTRUE(value("cluster_columns", TRUE)) && identical(order_mode, "auto_cluster")
  gaps_col <- NULL
  group_field <- as.character(value("group_field", ""))
  if (identical(order_mode, "group_split") && !is.null(annotation_col) && group_field %in% names(annotation_col)) {
    groups <- as.character(annotation_col[[group_field]])
    runs <- rle(groups)$lengths
    if (length(runs) > 1L) gaps_col <- cumsum(runs)[-length(runs)]
  }
  palette <- grDevices::colorRampPalette(c(
    as.character(value("low_color", "#4575B4")),
    as.character(value("mid_color", "#FFFFBF")),
    as.character(value("high_color", "#D73027"))
  ))(101L)
  args <- list(
    mat = matrix_value,
    color = palette,
    scale = "none",
    cluster_rows = isTRUE(value("cluster_rows", TRUE)),
    cluster_cols = cluster_columns,
    show_colnames = isTRUE(value("show_sample_names", FALSE)),
    show_rownames = isTRUE(value("show_feature_names", FALSE)),
    border_color = NA,
    main = as.character(value("title", "DEG heatmap")),
    fontsize = as.numeric(value("base_size", 9)),
    fontsize_row = max(4, as.numeric(value("base_size", 9)) * 0.72),
    fontsize_col = max(5, as.numeric(value("base_size", 9)) * 0.82),
    silent = TRUE
  )
  if (!is.null(annotation_col)) args$annotation_col <- annotation_col
  if (!is.null(gaps_col)) args$gaps_col <- gaps_col
  ggplotify::as.ggplot(do.call(pheatmap::pheatmap, args))
}

bp_compute_heatmap <- function(expression_data, metadata_data = NULL, config = list(), differential_data = NULL, render_plot = TRUE) {
  config <- bp_normalize_heatmap_config(config)
  if (identical(config$input_semantic_type, "unconfirmed_raw_counts")) {
    return(list(ok = FALSE, error = "当前数据结构可能是 RNA-seq Raw count。请先确认数据语义，软件不会静默执行关键分析。", requires_semantic_confirmation = TRUE, diagnostics = list(), warnings = character()))
  }
  if (identical(config$input_semantic_type, "raw_counts") &&
      !identical(config$raw_count_recipe_confirmed_signature, bp_heatmap_recipe_signature(config))) {
    return(list(ok = FALSE, error = "Raw Count 预处理尚未完成，或 CPM/TMM 参数已发生变化。请先点击“生成/更新 logCPM”。", requires_recipe_confirmation = TRUE, diagnostics = list(), warnings = character()))
  }
  tryCatch({
    preparation <- NULL
    heatmap_data <- expression_data
    heatmap_config <- config
    if (identical(config$input_semantic_type, "raw_counts")) {
      preparation <- bp_raw_count_prepare_core(expression_data, config)
      heatmap_data <- preparation$expression
      heatmap_config$expression_orientation <- "genes_by_samples"
      heatmap_config$feature_id_location <- "column"
      heatmap_config$feature_id_field <- "Feature"
      heatmap_config$expression_sample_id_location <- "column_names"
      heatmap_config$expression_sample_id_field <- ""
      heatmap_config$transform <- "none"
      heatmap_config$row_zscore <- TRUE
    }
    result <- bp_heatmap_compute_core(heatmap_data, metadata_data, heatmap_config, differential_data = differential_data)
    if (!is.null(preparation)) {
      result$normalized_expression <- preparation$expression
      result$preparation <- preparation
      result$warnings <- unique(c(
        result$warnings,
        paste0("Raw count 低表达过滤保留 ", preparation$retained_feature_count, " / ", preparation$original_feature_count, " 个特征；使用 ", preparation$normalization_label, "，随后按基因执行 Z-score。")
      ))
    }
    if (isTRUE(render_plot)) result$plot <- bp_heatmap_plot_core(result$heatmap_matrix, result$metadata, heatmap_config)
    result
  }, error = function(error) list(
    ok = FALSE, error = conditionMessage(error), diagnostics = error$diagnostics %||% list(), warnings = error$warnings %||% character()
  ))
}

bp_heatmap_derived_source <- function(id, name, kind, config, result = NULL) {
  data <- if (!is.null(result) && isTRUE(result$ok)) result[[kind]] else NULL
  preparation <- result$preparation %||% list()
  package_versions <- preparation$package_versions %||% list(R = as.character(getRversion()))
  if (requireNamespace("ggplot2", quietly = TRUE)) package_versions$ggplot2 <- as.character(utils::packageVersion("ggplot2"))
  if (requireNamespace("pheatmap", quietly = TRUE)) package_versions$pheatmap <- as.character(utils::packageVersion("pheatmap"))
  if (requireNamespace("ggplotify", quietly = TRUE)) package_versions$ggplotify <- as.character(utils::packageVersion("ggplotify"))
  semantic_type <- if (identical(kind, "normalized_expression")) "normalized_expression" else "heatmap_matrix"
  list(
    id = id, name = name,
    source_type = if (identical(kind, "normalized_expression")) "derived_expression" else "derived_heatmap",
    original_file_name = "Computed from heatmap inputs", object_type = "data.frame", object_name = name,
    rows = if (is.data.frame(data)) nrow(data) else 0L,
    columns = if (is.data.frame(data)) ncol(data) else 0L,
    status = if (is.data.frame(data)) "ready" else "derived_stale",
    example = FALSE, derived = TRUE, readonly = TRUE,
    semantic_type = semantic_type, semantic_confirmed = TRUE, semantic_contract_version = "0.1.0",
    relink_required = FALSE, derived_kind = kind,
    input_source_ids = Filter(nzchar, c(config$expression_source_id, config$metadata_source_id, config$differential_source_id)),
    lineage = list(
      analysis = if (identical(config$feature_selection_mode, "differential_results")) "deg_heatmap" else if (identical(config$input_semantic_type, "raw_counts")) "raw_count_heatmap" else "expression_heatmap",
      contract_version = "0.1.0",
      parent_source_ids = Filter(nzchar, c(config$expression_source_id, config$metadata_source_id, config$differential_source_id)),
      recipe_signature = if (identical(config$input_semantic_type, "raw_counts")) bp_heatmap_recipe_signature(config) else "",
      normalization = preparation$normalization_label %||% config$transform,
      parameters = list(
        filter_cpm = config$raw_count_filter_cpm, filter_min_samples = config$raw_count_filter_min_samples,
        normalization = config$raw_count_normalization, prior_count = config$raw_count_prior_count,
        variable_feature_count = config$variable_feature_count, row_zscore = config$row_zscore,
        feature_selection_mode = config$feature_selection_mode,
        differential_gene_id_field = config$differential_gene_id_field,
        differential_status_field = config$differential_status_field,
        differential_exclude_values = config$differential_exclude_values,
        sample_order = config$sample_order, group_field = config$group_field,
        annotation_fields = config$annotation_fields
      ),
      package_versions = package_versions,
      created_at = if (is.data.frame(data)) format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z") else NULL
    ),
    processing_history = c(preparation$processing_history %||% list(), list(list(
      step = if (identical(config$feature_selection_mode, "differential_results")) "deg_filter_gene_zscore_heatmap" else "gene_zscore_and_heatmap",
      retained = if (is.data.frame(data)) nrow(data) else 0L
    ))),
    column_metadata = if (is.data.frame(data)) bp_profile_dataset(data)$column_metadata else list(),
    quality = list(warnings = list()), parse_options = list()
  )
}

bp_heatmap_upsert_derived_sources <- function(project, config, result = NULL) {
  project <- unserialize(serialize(project, NULL))
  config <- bp_normalize_heatmap_config(config, project)
  reserved <- c(normalized_expression = "dataset_normalized_expression", heatmap_matrix = "dataset_heatmap_matrix")
  conflicts <- Filter(function(source) source$name %in% names(reserved) && !isTRUE(source$derived) && !source$id %in% unname(reserved), project$data_sources %||% list())
  if (length(conflicts)) stop("数据源名称 normalized_expression 和 heatmap_matrix 为分析派生结果保留，请先重命名冲突的数据源。", call. = FALSE)
  input_ids <- Filter(nzchar, c(config$expression_source_id, config$metadata_source_id, config$differential_source_id))
  parent_sources <- Filter(function(source) source$id %in% input_ids, project$data_sources %||% list())
  parent_versions <- stats::setNames(
    lapply(parent_sources, function(source) source$data_version %||% source$passport$content_fingerprint %||% ""),
    vapply(parent_sources, `[[`, character(1), "id")
  )
  project$data_sources <- Filter(function(source) !source$id %in% unname(reserved), project$data_sources %||% list())
  derived <- list(bp_heatmap_derived_source(reserved[["heatmap_matrix"]], "heatmap_matrix", "heatmap_matrix", config, result))
  if (identical(config$input_semantic_type, "raw_counts")) {
    derived <- c(list(bp_heatmap_derived_source(reserved[["normalized_expression"]], "normalized_expression", "normalized_expression", config, result)), derived)
  }
  derived <- lapply(derived, function(source) {
    source$lineage$parent_source_versions <- parent_versions
    source
  })
  project$data_sources <- c(project$data_sources, derived)
  project
}

bp_heatmap_config_from_project <- function(project) {
  config <- bp_normalize_heatmap_config(project$visual_config$heatmap %||% list(), project)
  source <- Filter(function(item) identical(item$id, config$expression_source_id), project$data_sources %||% list())
  if (length(source)) {
    effective <- bp_data_source_effective_semantic(source[[1]])
    config$input_semantic_type <- if (!isTRUE(source[[1]]$semantic_confirmed) && identical(effective, "raw_counts")) "unconfirmed_raw_counts" else effective
  }
  config
}

bp_generate_heatmap_analysis_code <- function(project) {
  if (!identical(project$visual_config$active_chart_type %||% "scatter", "heatmap")) return("")
  config <- bp_heatmap_config_from_project(project)
  expression_source <- bp_pca_source(project, config$expression_source_id)
  if (is.null(expression_source)) stop("Heatmap expression data source is not registered.", call. = FALSE)
  metadata_source <- if (nzchar(config$metadata_source_id)) bp_pca_source(project, config$metadata_source_id) else NULL
  if (nzchar(config$metadata_source_id) && is.null(metadata_source)) stop("Heatmap metadata data source is not registered.", call. = FALSE)
  differential_source <- if (nzchar(config$differential_source_id)) bp_pca_source(project, config$differential_source_id) else NULL
  if (identical(config$feature_selection_mode, "differential_results") && is.null(differential_source)) {
    stop("DEG heatmap differential-results data source is not registered.", call. = FALSE)
  }
  if (identical(config$input_semantic_type, "unconfirmed_raw_counts")) stop("Raw count 数据语义尚未确认，不能生成热图分析代码。", call. = FALSE)
  if (identical(config$input_semantic_type, "raw_counts") &&
      !identical(config$raw_count_recipe_confirmed_signature, bp_heatmap_recipe_signature(config))) {
    stop("Raw Count 尚未生成 logCPM 中间矩阵，不能生成最终热图代码。", call. = FALSE)
  }
  if (identical(config$feature_selection_mode, "differential_results") &&
      !identical(config$differential_match_confirmed_signature, bp_heatmap_deg_match_signature(config))) {
    stop("DEG 与表达矩阵尚未完成匹配验证，不能生成最终热图代码。", call. = FALSE)
  }
  config_source <- paste(capture.output(dput(config)), collapse = "\n")
  compute_source <- paste(deparse(bp_heatmap_compute_core, width.cutoff = 120L), collapse = "\n")
  plot_source <- paste(deparse(bp_heatmap_plot_core, width.cutoff = 120L), collapse = "\n")
  metadata_symbol <- if (is.null(metadata_source)) "NULL" else bp_symbol_source_name(metadata_source$name)
  differential_symbol <- if (is.null(differential_source)) "NULL" else bp_symbol_source_name(differential_source$name)
  header <- c(
    "# Expression heatmap analysis preparation (generated by BioPlotBlocks)",
    paste0(".bioplotblocks_compute_heatmap <- ", compute_source),
    paste0(".bioplotblocks_plot_heatmap <- ", plot_source),
    paste0(".heatmap_config <- ", config_source)
  )
  preparation <- if (identical(config$input_semantic_type, "raw_counts")) {
    raw_source <- paste(deparse(bp_raw_count_prepare_core, width.cutoff = 120L), collapse = "\n")
    c(
      "# Raw count recipe: filter low expression -> TMM -> logCPM -> gene Z-score -> heatmap",
      paste0(".bioplotblocks_prepare_raw_counts <- ", raw_source),
      paste0(".raw_preparation <- .bioplotblocks_prepare_raw_counts(", bp_symbol_source_name(expression_source$name), ", .heatmap_config)"),
      "normalized_expression <- .raw_preparation$expression",
      ".heatmap_config$expression_orientation <- \"genes_by_samples\"",
      ".heatmap_config$feature_id_location <- \"column\"",
      ".heatmap_config$feature_id_field <- \"Feature\"",
      ".heatmap_config$expression_sample_id_location <- \"column_names\"",
      ".heatmap_config$expression_sample_id_field <- \"\"",
      ".heatmap_config$transform <- \"none\"",
      ".heatmap_config$row_zscore <- TRUE",
      paste0(".heatmap_analysis <- .bioplotblocks_compute_heatmap(normalized_expression, ", metadata_symbol, ", .heatmap_config, differential_data = ", differential_symbol, ")"),
      ".heatmap_analysis$preparation <- .raw_preparation"
    )
  } else {
    paste0(".heatmap_analysis <- .bioplotblocks_compute_heatmap(", bp_symbol_source_name(expression_source$name), ", ", metadata_symbol, ", .heatmap_config, differential_data = ", differential_symbol, ")")
  }
  footer <- c(
    "heatmap_matrix <- .heatmap_analysis$heatmap_matrix",
    "heatmap_metadata <- .heatmap_analysis$metadata",
    "heatmap_plot <- .bioplotblocks_plot_heatmap(heatmap_matrix, heatmap_metadata, .heatmap_config)",
    if (identical(config$input_semantic_type, "raw_counts")) {
      "rm(.raw_preparation, .heatmap_analysis, .heatmap_config, .bioplotblocks_prepare_raw_counts, .bioplotblocks_compute_heatmap, .bioplotblocks_plot_heatmap)"
    } else {
      "rm(.heatmap_analysis, .heatmap_config, .bioplotblocks_compute_heatmap, .bioplotblocks_plot_heatmap)"
    }
  )
  paste(c(header, preparation, footer), collapse = "\n")
}

bp_heatmap_setup_lines <- function(project) {
  if (!identical(project$visual_config$active_chart_type %||% "scatter", "heatmap")) return(character())
  config <- bp_heatmap_config_from_project(project)
  ids <- unique(Filter(nzchar, c(config$expression_source_id, config$metadata_source_id, config$differential_source_id)))
  source_lines <- unlist(lapply(ids, function(id) {
    source <- bp_pca_source(project, id)
    if (is.null(source) || isTRUE(source$example) || isTRUE(source$derived)) return(character())
    bp_data_source_setup_line(source)
  }), use.names = FALSE)
  c(
    'if (!requireNamespace("pheatmap", quietly = TRUE)) stop("The pheatmap package is required for expression heatmaps.")',
    'if (!requireNamespace("ggplotify", quietly = TRUE)) stop("The ggplotify package is required to place pheatmap output in the ggplot2 workspace.")',
    source_lines
  )
}

# Active analysis dispatch ---------------------------------------------------

bp_generate_analysis_code <- function(project) {
  if (identical(project$visual_config$active_chart_type %||% "scatter", "heatmap")) {
    return(bp_generate_heatmap_analysis_code(project))
  }
  bp_generate_pca_analysis_code(project)
}

bp_analysis_setup_lines <- function(project) {
  if (identical(project$visual_config$active_chart_type %||% "scatter", "heatmap")) {
    return(bp_heatmap_setup_lines(project))
  }
  bp_pca_setup_lines(project)
}
