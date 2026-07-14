make_raw_count_fixture <- function(features = 80L, samples = 6L) {
  set.seed(20260713)
  values <- matrix(stats::rpois(features * samples, lambda = 35), nrow = features)
  values[1, ] <- c(rep(0L, samples - 1L), 1L)
  frame <- data.frame(Gene = paste0("g", seq_len(features)), values, check.names = FALSE)
  names(frame)[-1] <- paste0("S", seq_len(samples))
  frame
}

make_count_source <- function(data, id = "dataset_counts", name = "counts_raw") {
  bp_enrich_data_source(list(
    id = id, name = name, source_type = "rdata", original_file_name = "counts.RData",
    object_type = "data.frame", rows = nrow(data), columns = ncol(data), status = "ready",
    example = FALSE, derived = FALSE, relink_required = FALSE,
    column_metadata = bp_profile_dataset(data)$column_metadata,
    quality = list(warnings = list()), parse_options = list()
  ), data)
}

test_that("data passport suggests raw counts without silently confirming semantics", {
  counts <- make_raw_count_fixture()
  passport <- bp_build_data_passport(counts)
  expect_identical(passport$suggested_semantic_type, "raw_counts")
  expect_identical(passport$orientation_suggestion, "genes_by_samples")
  expect_gte(passport$integer_ratio, 0.98)
  expect_gte(passport$nonnegative_ratio, 0.995)
  expect_identical(passport$feature_id_suggestion, "Gene")
  expect_true(nzchar(passport$content_fingerprint))

  changed_counts <- counts
  changed_counts$S1[[2]] <- changed_counts$S1[[2]] + 1L
  expect_identical(bp_dataset_fingerprint(changed_counts), passport$fingerprint)
  expect_false(identical(bp_dataset_content_fingerprint(changed_counts), passport$content_fingerprint))

  normalized <- counts
  normalized[-1] <- lapply(normalized[-1], function(column) log2(column + 1))
  normalized_passport <- bp_build_data_passport(normalized)
  expect_identical(normalized_passport$suggested_semantic_type, "normalized_expression")

  negative <- normalized
  negative[[2]][[1]] <- -2
  expect_false(identical(bp_build_data_passport(negative)$suggested_semantic_type, "raw_counts"))
})

test_that("semantic labels render derived and future data-source types safely", {
  expect_identical(bp_data_semantic_label("raw_counts"), "RNA-seq Raw count")
  expect_identical(bp_data_semantic_label("normalized_expression"), "已标准化表达矩阵")
  expect_identical(bp_data_semantic_label("pca_scores"), "PCA 样本得分")
  expect_identical(bp_data_semantic_label("pca_loadings"), "PCA 特征载荷")
  expect_identical(bp_data_semantic_label("future_semantic"), "future_semantic")
  expect_identical(bp_data_semantic_label(""), "普通表格")
})

test_that("semantic confirmation is versioned and drives chart compatibility", {
  counts <- make_raw_count_fixture()
  source <- make_count_source(counts)
  project <- bp_create_project()
  project <- bp_register_data_source(project, source)

  unconfirmed <- bp_chart_data_compatibility(source, "pca", counts)
  expect_identical(unconfirmed$status, "supplement")
  expect_match(unconfirmed$action, "确认", fixed = TRUE)

  project <- bp_confirm_data_source_semantic(project, source$id, "raw_counts", counts)
  confirmed <- Filter(function(item) identical(item$id, source$id), project$data_sources)[[1]]
  expect_true(confirmed$semantic_confirmed)
  expect_identical(confirmed$semantic_type, "raw_counts")
  expect_identical(project$analysis_workflow_mode, "rna_seq")
  expect_identical(bp_chart_data_compatibility(confirmed, "pca", counts)$status, "transform")
  expect_identical(bp_chart_data_compatibility(confirmed, "volcano", counts)$status, "supplement")
  expect_identical(bp_chart_data_compatibility(confirmed, "violin", counts)$status, "transform")
})

test_that("raw count preparation filters low expression and validates count semantics", {
  counts <- make_raw_count_fixture()
  config <- list(
    expression_orientation = "genes_by_samples", feature_id_field = "Gene",
    raw_count_filter_cpm = 0.5, raw_count_filter_min_samples = 2L,
    raw_count_normalization = "log2p1", raw_count_prior_count = 2
  )
  prepared <- bp_raw_count_prepare_core(counts, config)
  expect_true(prepared$ok)
  expect_identical(prepared$normalization_method, "log2p1")
  expect_lt(prepared$retained_feature_count, prepared$original_feature_count)
  expect_identical(names(prepared$expression)[[1]], "Feature")
  expect_identical(ncol(prepared$expression), 7L)
  expect_false("g1" %in% prepared$expression$Feature)

  duplicate <- counts
  duplicate$Gene[[2]] <- duplicate$Gene[[1]]
  expect_error(bp_raw_count_prepare_core(duplicate, config), "重复特征 ID")

  non_integer <- counts
  non_integer[[2]][[2]] <- non_integer[[2]][[2]] + 0.5
  expect_error(bp_raw_count_prepare_core(non_integer, config), "非整数值")
})

test_that("raw count PCA requires explicit recipe confirmation", {
  counts <- make_raw_count_fixture()
  config <- bp_pca_defaults()
  config$expression_source_id <- "dataset_counts"
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_location <- "column"
  config$feature_id_field <- "Gene"
  config$input_semantic_type <- "raw_counts"
  config$raw_count_normalization <- "log2p1"
  config$raw_count_filter_cpm <- 0.5
  config$raw_count_filter_min_samples <- 2L

  blocked <- bp_compute_pca(counts, NULL, config)
  expect_false(blocked$ok)
  expect_true(blocked$requires_recipe_confirmation)

  config$raw_count_recipe_confirmed_signature <- bp_raw_count_recipe_signature(config)
  result <- bp_compute_pca(counts, NULL, config)
  expect_true(result$ok)
  expect_true(is.data.frame(result$normalized_expression))
  expect_identical(nrow(result$scores), 6L)
  expect_equal(sum(result$explained_variance), 100, tolerance = 1e-7)
  expect_identical(result$transform_applied, "log2p1")
})

test_that("raw count PCA creates reproducible derived lineage and standalone R", {
  counts <- make_raw_count_fixture()
  source <- make_count_source(counts)
  project <- bp_ggplot_only_project(registry)
  project <- bp_register_data_source(project, source)
  project <- bp_confirm_data_source_semantic(project, source$id, "raw_counts", counts)
  config <- bp_pca_defaults(project)
  config$expression_source_id <- source$id
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_location <- "column"
  config$feature_id_field <- "Gene"
  config$input_semantic_type <- "raw_counts"
  config$raw_count_normalization <- "log2p1"
  config$raw_count_recipe_confirmed_signature <- bp_raw_count_recipe_signature(config)
  result <- bp_compute_pca(counts, NULL, config)
  expect_true(result$ok)

  applied <- bp_apply_visual_pca_config(project, config, registry, analysis_result = result)
  derived <- Filter(function(item) isTRUE(item$derived), applied$project$data_sources)
  expect_setequal(vapply(derived, `[[`, character(1), "name"), c("normalized_expression", "pca_scores", "pca_loadings"))
  normalized_source <- Filter(function(item) identical(item$name, "normalized_expression"), derived)[[1]]
  expect_identical(normalized_source$semantic_type, "normalized_expression")
  expect_identical(normalized_source$lineage$parent_source_ids, source$id)
  expect_identical(normalized_source$lineage$parent_source_versions[[source$id]], source$data_version)
  expect_identical(normalized_source$status, "ready")
  expect_identical(applied$project$analysis_recipes$pca$type, "raw_count_pca")
  expect_true(all(c("R", "stats", "ggplot2") %in% names(applied$project$analysis_recipes$pca$package_versions)))

  code <- bp_generate_pca_analysis_code(applied$project)
  expect_match(code, "filter low expression", fixed = TRUE)
  expect_match(code, "normalized_expression", fixed = TRUE)
  expect_match(code, "stats::prcomp", fixed = TRUE)
  environment <- bp_default_environment()
  environment$counts_raw <- counts
  expect_silent(eval(parse(text = code), envir = environment))
  expect_true(is.data.frame(environment$normalized_expression))
  expect_true(is.data.frame(environment$pca_scores))
})

test_that("relink preserves confirmed semantics only for a matching structure", {
  counts <- make_raw_count_fixture()
  source <- make_count_source(counts)
  project <- bp_create_project()
  project <- bp_register_data_source(project, source)
  project <- bp_confirm_data_source_semantic(project, source$id, "raw_counts", counts)
  previous <- Filter(function(item) identical(item$id, source$id), project$data_sources)[[1]]

  matching <- bp_enrich_data_source(source, counts, previous)
  expect_true(matching$semantic_confirmed)
  expect_identical(matching$semantic_type, "raw_counts")

  changed <- counts[, -ncol(counts), drop = FALSE]
  changed_source <- source
  changed_source$rows <- nrow(changed)
  changed_source$columns <- ncol(changed)
  incompatible <- bp_enrich_data_source(changed_source, changed, previous)
  expect_false(incompatible$semantic_confirmed)
})

test_that("source version changes invalidate derived data and recipe signatures cover analysis inputs", {
  counts <- make_raw_count_fixture()
  source <- make_count_source(counts)
  project <- bp_create_project()
  project <- bp_register_data_source(project, source)
  project$data_sources[[length(project$data_sources) + 1L]] <- list(
    id = "dataset_result", name = "pca_scores", derived = TRUE,
    input_source_ids = source$id, status = "ready", lineage = list()
  )

  changed <- counts
  changed$S1[[2]] <- changed$S1[[2]] + 1L
  updated <- bp_enrich_data_source(source, changed, source)
  project <- bp_register_data_source(project, updated)
  derived <- Filter(function(item) identical(item$id, "dataset_result"), project$data_sources)[[1]]
  expect_identical(derived$status, "derived_stale")
  expect_match(derived$lineage$stale_reason, source$id, fixed = TRUE)

  config <- bp_pca_defaults(project)
  config$expression_source_id <- source$id
  first <- bp_raw_count_recipe_signature(config)
  config$unmatched_sample_policy <- "matched_only"
  expect_false(identical(first, bp_raw_count_recipe_signature(config)))
})
