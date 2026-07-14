# RNA-seq data semantics and raw-count preparation --------------------------

bp_data_semantic_choices <- function(include_auto = FALSE) {
  choices <- c(
    "普通表格" = "generic_table",
    "RNA-seq Raw count" = "raw_counts",
    "已标准化表达矩阵" = "normalized_expression",
    "样本信息表" = "sample_metadata",
    "差异分析结果" = "differential_results",
    "长格式测量数据" = "long_format_measurements"
  )
  if (isTRUE(include_auto)) c("根据结构建议，稍后确认" = "auto", choices) else choices
}

bp_data_semantic_label <- function(value, default = "普通表格") {
  selectable <- bp_data_semantic_choices()
  labels <- c(
    stats::setNames(names(selectable), unname(selectable)),
    pca_scores = "PCA 样本得分",
    pca_loadings = "PCA 特征载荷",
    correlation_matrix = "相关性矩阵",
    heatmap_matrix = "热图矩阵"
  )
  value <- as.character(value %||% "")[[1]]
  if (!nzchar(value)) return(default)
  label <- unname(labels[value])
  if (!length(label) || is.na(label) || !nzchar(label)) value else label
}

bp_normalize_semantic_type <- function(value, default = "generic_table") {
  allowed <- unname(bp_data_semantic_choices())
  value <- as.character(value %||% default)[[1]]
  if (value %in% allowed) value else default
}

bp_dataset_fingerprint <- function(data) {
  if (!(is.data.frame(data) || is.matrix(data))) return("")
  frame <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  types <- vapply(frame, bp_column_type, character(1))
  paste(nrow(frame), ncol(frame), paste(names(frame), collapse = "\u001f"), paste(types, collapse = "\u001f"), sep = "\u001e")
}

bp_dataset_content_fingerprint <- function(data) {
  if (!(is.data.frame(data) || is.matrix(data))) return("")
  frame <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(frame, algo = "xxhash64", serialize = TRUE))
  }
  sampled_columns <- vapply(frame, function(column) {
    length_column <- length(column)
    positions <- unique(c(
      seq_len(min(length_column, 64L)),
      if (length_column) seq.int(max(1L, length_column - 63L), length_column) else integer()
    ))
    sampled <- if (length(positions)) column[positions] else column
    paste(
      bp_column_type(column), length_column, sum(is.na(column)),
      paste(capture.output(dput(sampled)), collapse = ""), sep = "\u001f"
    )
  }, character(1))
  paste(bp_dataset_fingerprint(frame), paste(sampled_columns, collapse = "\u001e"), sep = "\u001d")
}

bp_meaningful_rownames <- function(data) {
  ids <- row.names(data)
  length(ids) == nrow(data) && !identical(as.character(ids), as.character(seq_len(nrow(data))))
}

bp_build_data_passport <- function(data, source = list()) {
  if (!(is.data.frame(data) || is.matrix(data))) {
    return(list(
      contract_version = "0.1.0", structure = class(data)[[1]] %||% typeof(data),
      rows = NA_integer_, columns = NA_integer_, numeric_columns = 0L,
      suggested_semantic_type = "generic_table", confidence = 0,
      orientation_suggestion = "unknown", evidence = "当前对象不是二维表格或矩阵。",
      fingerprint = ""
    ))
  }
  frame <- as.data.frame(data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  numeric_mask <- vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))
  numeric_names <- names(frame)[numeric_mask]
  id_candidates <- names(frame)[!numeric_mask & vapply(frame, function(column) {
    !anyNA(column) && !anyDuplicated(as.character(column))
  }, logical(1))]

  sampled <- unlist(lapply(frame[numeric_mask], function(column) {
    column <- as.numeric(column)
    column[seq_len(min(length(column), 20000L))]
  }), use.names = FALSE)
  if (length(sampled) > 200000L) sampled <- sampled[seq_len(200000L)]
  finite <- sampled[is.finite(sampled)]
  nonnegative_ratio <- if (length(finite)) mean(finite >= 0) else 0
  integer_ratio <- if (length(finite)) mean(abs(finite - round(finite)) < 1e-8) else 0
  missing_values <- sum(vapply(frame, function(column) sum(is.na(column)), integer(1)))
  total_values <- max(1, nrow(frame) * ncol(frame))

  orientation <- if (nrow(frame) >= 2L * max(1L, length(numeric_names))) {
    "genes_by_samples"
  } else if (length(numeric_names) >= 2L * max(1L, nrow(frame))) {
    "samples_by_features"
  } else {
    "ambiguous"
  }
  raw_candidate <- length(numeric_names) >= 3L && length(finite) > 0L &&
    nonnegative_ratio >= 0.995 && integer_ratio >= 0.98
  normalized_candidate <- length(numeric_names) >= 2L && length(finite) > 0L &&
    nonnegative_ratio >= 0.95 && integer_ratio < 0.98
  suggested <- if (raw_candidate) "raw_counts" else if (normalized_candidate) "normalized_expression" else "generic_table"
  confidence <- if (raw_candidate) {
    min(0.99, 0.6 + 0.2 * nonnegative_ratio + 0.2 * integer_ratio)
  } else if (normalized_candidate) {
    0.72
  } else {
    0.55
  }
  library_sizes <- if (identical(orientation, "genes_by_samples") && length(numeric_names)) {
    vapply(frame[numeric_names], function(column) sum(as.numeric(column), na.rm = TRUE), numeric(1))
  } else numeric()
  evidence <- c(
    paste0(sprintf("%.1f%%", 100 * nonnegative_ratio), " 的抽样数值为非负数。"),
    paste0(sprintf("%.1f%%", 100 * integer_ratio), " 的抽样数值为整数。"),
    paste0("识别到 ", length(numeric_names), " 个数值列。"),
    if (length(id_candidates)) paste0("唯一文本 ID 候选：", id_candidates[[1]], "。") else "未发现唯一文本 ID 列。",
    if (missing_values) paste0("包含 ", format(missing_values, big.mark = ","), " 个缺失值。") else "未发现缺失值。"
  )
  list(
    contract_version = "0.1.0",
    structure = if (is.matrix(data)) "matrix" else "data.frame",
    rows = nrow(frame), columns = ncol(frame), numeric_columns = length(numeric_names),
    suggested_semantic_type = suggested, confidence = confidence,
    orientation_suggestion = orientation,
    feature_id_suggestion = if (length(id_candidates)) id_candidates[[1]] else if (bp_meaningful_rownames(frame)) "row.names" else "",
    sample_id_suggestion = if (identical(orientation, "genes_by_samples")) "column_names" else if (bp_meaningful_rownames(frame)) "row.names" else "",
    nonnegative_ratio = nonnegative_ratio, integer_ratio = integer_ratio,
    missing_values = missing_values, missing_ratio = missing_values / total_values,
    library_size_min = if (length(library_sizes)) min(library_sizes) else NA_real_,
    library_size_max = if (length(library_sizes)) max(library_sizes) else NA_real_,
    evidence = evidence,
    fingerprint = bp_dataset_fingerprint(frame),
    content_fingerprint = bp_dataset_content_fingerprint(frame),
    source = source$original_file_name %||% if (isTRUE(source$example)) "Built-in example" else ""
  )
}

bp_normalize_data_source_semantics <- function(source) {
  if (isTRUE(source$example)) {
    source$semantic_type <- source$semantic_type %||% "generic_table"
    source$semantic_confirmed <- TRUE
  } else if (isTRUE(source$derived)) {
    source$semantic_type <- source$semantic_type %||% switch(
      source$derived_kind %||% "",
      scores = "pca_scores", loadings = "pca_loadings", normalized_expression = "normalized_expression",
      "generic_table"
    )
    source$semantic_confirmed <- TRUE
  } else {
    source$semantic_type <- bp_normalize_semantic_type(source$semantic_type %||% "generic_table")
    source$semantic_confirmed <- isTRUE(source$semantic_confirmed)
  }
  source$semantic_contract_version <- source$semantic_contract_version %||% "0.1.0"
  source$data_version <- source$data_version %||% source$passport$content_fingerprint %||% ""
  source$processing_history <- source$processing_history %||% list()
  source
}

bp_enrich_data_source <- function(source, data, previous = NULL) {
  passport <- bp_build_data_passport(data, source)
  preserve <- !is.null(previous) && isTRUE(previous$semantic_confirmed) &&
    identical(previous$passport$fingerprint %||% "", passport$fingerprint %||% "")
  source$passport <- passport
  source$data_version <- passport$content_fingerprint %||% passport$fingerprint
  source$semantic_suggestion <- passport$suggested_semantic_type
  source$semantic_contract_version <- "0.1.0"
  source$processing_history <- if (preserve) previous$processing_history %||% list() else source$processing_history %||% list()
  if (preserve) {
    source$semantic_type <- previous$semantic_type
    source$semantic_confirmed <- TRUE
    source$semantic_confirmed_at <- previous$semantic_confirmed_at %||% NULL
  } else {
    source$semantic_type <- bp_normalize_semantic_type(source$semantic_type %||% "generic_table")
    source$semantic_confirmed <- isTRUE(source$semantic_confirmed)
  }
  source
}

bp_confirm_data_source_semantic <- function(project, source_id, semantic_type, data) {
  semantic_type <- bp_normalize_semantic_type(semantic_type)
  project <- unserialize(serialize(project, NULL))
  index <- which(vapply(project$data_sources %||% list(), function(source) identical(source$id, source_id), logical(1)))
  if (!length(index)) stop("Unknown data source: ", source_id, call. = FALSE)
  source <- project$data_sources[[index[[1]]]]
  if (isTRUE(source$derived) || isTRUE(source$readonly)) stop("派生数据源的语义由分析流程确定，不能手动修改。", call. = FALSE)
  if (!(is.data.frame(data) || is.matrix(data))) stop("只能为二维表格或矩阵确认数据语义。", call. = FALSE)
  source$passport <- bp_build_data_passport(data, source)
  source$data_version <- source$passport$content_fingerprint %||% source$passport$fingerprint
  source$semantic_suggestion <- source$passport$suggested_semantic_type
  source$semantic_type <- semantic_type
  source$semantic_confirmed <- TRUE
  source$semantic_confirmed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  source$semantic_user_override <- !identical(semantic_type, source$semantic_suggestion)
  source$semantic_contract_version <- "0.1.0"
  project$data_sources[[index[[1]]]] <- source
  project$analysis_workflow_mode <- if (semantic_type %in% c("raw_counts", "sample_metadata")) "rna_seq" else project$analysis_workflow_mode %||% "generic"
  project$data_sources <- lapply(project$data_sources, function(item) {
    if (isTRUE(item$derived) && source_id %in% (item$input_source_ids %||% character())) item$status <- "derived_stale"
    item
  })
  project
}

bp_data_source_effective_semantic <- function(source, data = NULL) {
  if (is.null(source)) return("generic_table")
  if (isTRUE(source$semantic_confirmed) || isTRUE(source$derived) || isTRUE(source$example)) {
    return(source$semantic_type %||% "generic_table")
  }
  passport <- source$passport
  if (is.null(passport) && !is.null(data)) passport <- bp_build_data_passport(data, source)
  passport$suggested_semantic_type %||% source$semantic_suggestion %||% "generic_table"
}

bp_chart_data_compatibility <- function(source, chart_type, data = NULL) {
  result <- function(status, reason, action = "") list(status = status, reason = reason, action = action)
  if (is.null(source)) return(result("incompatible", "没有可用数据源。", "导入或选择数据源"))
  if (identical(source$status, "relink_required") || isTRUE(source$relink_required)) {
    return(result("relink", "原始文件当前不可访问。", "重新链接原始文件"))
  }
  semantic <- bp_data_source_effective_semantic(source, data)
  confirmed <- isTRUE(source$semantic_confirmed) || isTRUE(source$derived) || isTRUE(source$example)
  passport <- source$passport
  if (is.null(passport) && !is.null(data)) passport <- bp_build_data_passport(data, source)
  if (!confirmed && identical(semantic, "raw_counts")) {
    return(result("supplement", "结构可能是 Raw count，但尚未由用户确认。", "确认数据语义"))
  }
  if (identical(chart_type, "pca")) {
    if (identical(semantic, "raw_counts")) return(result("transform", "需要低表达过滤和 TMM-logCPM 或 log2(count + 1) 转换。", "确认 PCA 分析配方"))
    if (semantic %in% c("normalized_expression", "pca_scores")) return(result("direct", "数据满足 PCA 表达矩阵契约。"))
    if (semantic %in% c("sample_metadata", "differential_results", "pca_loadings")) return(result("incompatible", "该语义类型不是多样本表达矩阵。", "选择表达矩阵"))
    if ((passport$numeric_columns %||% 0L) >= 2L) return(result("direct", "可按通用数值矩阵配置 PCA；请确认矩阵方向。"))
    return(result("incompatible", "有效数值特征不足。", "选择至少包含 2 个数值特征的数据"))
  }
  if (identical(chart_type, "volcano")) {
    if (identical(semantic, "differential_results")) return(result("direct", "差异分析结果可直接映射到火山图。"))
    if (identical(semantic, "raw_counts")) return(result("supplement", "Raw count 需要样本分组、比较关系和差异分析。", "补充样本信息和对照关系"))
    columns <- tolower(bp_data_source_columns(source, data))
    if (any(grepl("logfc|log2fold", columns)) && any(grepl("pvalue|p.value|fdr|padj", columns))) return(result("direct", "检测到倍数变化和显著性字段。"))
    return(result("incompatible", "缺少差异分析的 logFC 与 PValue/FDR 字段。", "选择差异结果或先运行差异分析"))
  }
  if (identical(chart_type, "boxplot")) {
    if (semantic %in% c("generic_table", "long_format_measurements")) return(result("direct", "可直接选择分组字段和数值字段。"))
    if (semantic %in% c("raw_counts", "normalized_expression")) return(result("transform", "宽表达矩阵需要选择目标基因并转换成长表。", "使用基因表达配方"))
  }
  if (identical(semantic, "raw_counts")) return(result("transform", "Raw count 不应直接作为普通坐标字段绘图。", "先生成适合目标图表的派生数据"))
  result("direct", "可作为通用数据表映射字段。")
}

bp_raw_count_default_normalization <- function() {
  if (requireNamespace("edgeR", quietly = TRUE)) "tmm_logcpm" else "log2p1"
}

bp_raw_count_recipe_signature <- function(config) {
  fields <- c(
    "expression_source_id", "metadata_source_id", "expression_orientation",
    "feature_id_location", "feature_id_field", "expression_sample_id_location",
    "expression_sample_id_field", "metadata_sample_id_field", "unmatched_sample_policy",
    "raw_count_filter_cpm", "raw_count_filter_min_samples", "raw_count_normalization",
    "raw_count_prior_count", "variable_feature_count", "remove_zero_variance",
    "missing_value_policy", "center", "scale"
  )
  values <- vapply(fields, function(name) paste(config[[name]] %||% "", collapse = ","), character(1))
  package_version <- if (identical(config$raw_count_normalization %||% "", "tmm_logcpm") && requireNamespace("edgeR", quietly = TRUE)) {
    as.character(utils::packageVersion("edgeR"))
  } else "base"
  paste(c("raw-count-pca-v0.1", values, as.character(getRversion()), package_version), collapse = "|")
}

# This function intentionally uses only base R plus optional edgeR namespace
# calls so its body can be embedded in standalone generated scripts.
bp_raw_count_prepare_core <- function(expression_data, config = list()) {
  value <- function(name, default = NULL) {
    item <- config[[name]]
    if (is.null(item) || !length(item) || is.na(item[[1]])) default else item[[1]]
  }
  abort <- function(message) stop(message, call. = FALSE)
  meaningful_rownames <- function(frame) {
    ids <- row.names(frame)
    length(ids) == nrow(frame) && !identical(as.character(ids), as.character(seq_len(nrow(frame))))
  }
  duplicate_values <- function(ids) unique(ids[duplicated(ids) | duplicated(ids, fromLast = TRUE)])
  collapse_ids <- function(ids) paste(utils::head(ids, 8L), collapse = "、")

  if (!(is.data.frame(expression_data) || is.matrix(expression_data))) abort("Raw count 输入必须是 data.frame 或二维矩阵。")
  frame <- as.data.frame(expression_data, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  if (nrow(frame) < 2L || ncol(frame) < 3L) abort("Raw count 矩阵至少需要 2 个特征和 3 个样本。")
  numeric_names <- names(frame)[vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))]
  orientation <- as.character(value("expression_orientation", "auto"))
  if (identical(orientation, "auto")) {
    if (nrow(frame) >= 2L * max(1L, length(numeric_names))) orientation <- "genes_by_samples"
    else if (length(numeric_names) >= 2L * max(1L, nrow(frame))) orientation <- "samples_by_features"
    else abort("无法安全自动识别 Raw count 矩阵方向；请手动选择“基因 × 样本”或“样本 × 特征”。")
  }

  feature_field <- as.character(value("feature_id_field", ""))
  sample_field <- as.character(value("expression_sample_id_field", ""))
  if (identical(orientation, "genes_by_samples")) {
    feature_ids <- character()
    excluded <- character()
    if (nzchar(feature_field)) {
      if (!feature_field %in% names(frame)) abort(paste0("Raw count 中不存在特征 ID 字段 ‘", feature_field, "’。"))
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
    numeric_names <- setdiff(names(frame)[vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))], excluded)
    if (length(numeric_names) < 3L) abort("基因 × 样本 Raw count 至少需要 3 个数值型样本列。")
    counts <- data.matrix(frame[, numeric_names, drop = FALSE])
    row.names(counts) <- feature_ids
    sample_ids <- colnames(counts)
  } else if (identical(orientation, "samples_by_features")) {
    excluded <- character()
    if (nzchar(sample_field)) {
      if (!sample_field %in% names(frame)) abort(paste0("Raw count 中不存在样本 ID 字段 ‘", sample_field, "’。"))
      sample_ids <- trimws(as.character(frame[[sample_field]]))
      excluded <- sample_field
    } else if (meaningful_rownames(frame)) {
      sample_ids <- trimws(row.names(frame))
    } else {
      candidates <- names(frame)[!vapply(frame, is.numeric, logical(1))]
      unique_candidates <- candidates[vapply(candidates, function(name) !anyNA(frame[[name]]) && !anyDuplicated(as.character(frame[[name]])), logical(1))]
      if (!length(unique_candidates)) abort("样本 × 特征 Raw count 需要行名或唯一的样本 ID 列。")
      sample_field <- unique_candidates[[1]]
      sample_ids <- trimws(as.character(frame[[sample_field]]))
      excluded <- sample_field
    }
    numeric_names <- setdiff(names(frame)[vapply(frame, function(column) is.numeric(column) || is.integer(column), logical(1))], excluded)
    if (length(numeric_names) < 2L || nrow(frame) < 3L) abort("样本 × 特征 Raw count 至少需要 3 个样本和 2 个数值型特征。")
    counts <- t(data.matrix(frame[, numeric_names, drop = FALSE]))
    colnames(counts) <- sample_ids
    feature_ids <- row.names(counts)
  } else {
    abort("不支持的 Raw count 矩阵方向。")
  }

  feature_ids <- trimws(as.character(feature_ids))
  sample_ids <- trimws(as.character(sample_ids))
  if (any(!nzchar(feature_ids) | is.na(feature_ids))) abort("Raw count 中存在空的特征 ID。")
  if (any(!nzchar(sample_ids) | is.na(sample_ids))) abort("Raw count 中存在空的样本 ID。")
  duplicate_features <- duplicate_values(feature_ids)
  duplicate_samples <- duplicate_values(sample_ids)
  if (length(duplicate_features)) abort(paste0("Raw count 中存在重复特征 ID：", collapse_ids(duplicate_features), "。"))
  if (length(duplicate_samples)) abort(paste0("Raw count 中存在重复样本 ID：", collapse_ids(duplicate_samples), "。"))
  if (anyNA(counts) || any(!is.finite(counts))) abort("Raw count 矩阵包含缺失值或非有限数值；请先修正数据。")
  if (any(counts < 0)) abort("Raw count 不能包含负值。")
  if (any(abs(counts - round(counts)) >= 1e-8)) abort("当前矩阵包含非整数值，不能按 Raw count 执行该配方。")
  storage.mode(counts) <- "integer"
  library_sizes <- colSums(counts)
  if (any(library_sizes <= 0)) abort(paste0("以下样本文库总量为 0：", collapse_ids(names(library_sizes)[library_sizes <= 0]), "。"))

  cpm_threshold <- suppressWarnings(as.numeric(value("raw_count_filter_cpm", 0.5)))
  if (!is.finite(cpm_threshold) || cpm_threshold < 0) cpm_threshold <- 0.5
  min_samples <- suppressWarnings(as.integer(value("raw_count_filter_min_samples", 2L)))
  if (!is.finite(min_samples) || min_samples < 1L) min_samples <- 2L
  min_samples <- min(min_samples, ncol(counts))
  cpm <- sweep(counts, 2L, library_sizes, "/") * 1e6
  keep <- rowSums(cpm > cpm_threshold) >= min_samples
  if (sum(keep) < 2L) abort("低表达过滤后不足 2 个特征；请降低 CPM 阈值或最少样本数。")
  filtered <- counts[keep, , drop = FALSE]

  method <- as.character(value("raw_count_normalization", "tmm_logcpm"))
  prior_count <- suppressWarnings(as.numeric(value("raw_count_prior_count", 2)))
  if (!is.finite(prior_count) || prior_count <= 0) prior_count <- 2
  package_versions <- list(R = as.character(getRversion()))
  if (identical(method, "tmm_logcpm")) {
    if (!requireNamespace("edgeR", quietly = TRUE)) {
      abort("当前 R 环境未安装 edgeR，不能执行 TMM + logCPM；请选择 log2(count + 1) 快速探索配方。")
    }
    dge <- edgeR::DGEList(counts = filtered)
    dge <- edgeR::calcNormFactors(dge, method = "TMM")
    normalized <- edgeR::cpm(dge, log = TRUE, prior.count = prior_count)
    method_label <- "edgeR TMM + logCPM"
    package_versions$edgeR <- as.character(utils::packageVersion("edgeR"))
  } else if (identical(method, "log2p1")) {
    normalized <- log2(filtered + 1)
    method_label <- "log2(count + 1)"
  } else {
    abort("不支持的 Raw count 标准化方法。")
  }
  normalized_frame <- data.frame(Feature = row.names(normalized), normalized, check.names = FALSE, stringsAsFactors = FALSE)
  list(
    ok = TRUE, expression = normalized_frame,
    orientation = orientation, normalization_method = method,
    normalization_label = method_label, package_versions = package_versions,
    original_feature_count = nrow(counts), retained_feature_count = nrow(filtered),
    removed_low_expression = nrow(counts) - nrow(filtered),
    sample_count = ncol(counts), sample_ids = colnames(counts),
    filter_cpm = cpm_threshold, filter_min_samples = min_samples,
    prior_count = prior_count,
    processing_history = list(
      list(step = "validate_raw_counts", result = paste0(nrow(counts), " features × ", ncol(counts), " samples")),
      list(step = "filter_low_expression", parameters = list(cpm = cpm_threshold, min_samples = min_samples), retained = nrow(filtered)),
      list(step = "normalize", method = method, label = method_label)
    )
  )
}
