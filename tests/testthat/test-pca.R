test_that("PCA matches shuffled metadata by sample identifier", {
  expression <- data.frame(
    Feature = paste0("G", 1:6),
    S1 = c(2, 4, 7, 3, 8, 6),
    S2 = c(3, 5, 9, 4, 7, 5),
    S3 = c(8, 6, 2, 9, 3, 4),
    S4 = c(9, 7, 3, 8, 2, 5),
    check.names = FALSE
  )
  metadata <- data.frame(
    Sample = c("S3", "S1", "S4", "S2"),
    Group = c("Treatment", "Control", "Treatment", "Control"),
    Batch = c("B2", "B1", "B2", "B1"),
    stringsAsFactors = FALSE
  )

  result <- bp_compute_pca(expression, metadata, list(
    expression_orientation = "genes_by_samples",
    feature_id_location = "column",
    feature_id_field = "Feature",
    metadata_sample_id_field = "Sample",
    transform = "none",
    variable_feature_count = "all"
  ))

  expect_true(result$ok)
  expect_identical(result$scores$Sample, c("S1", "S2", "S3", "S4"))
  expect_identical(result$scores$Group, c("Control", "Control", "Treatment", "Treatment"))
  expect_identical(result$scores$Batch, c("B1", "B1", "B2", "B2"))
  expect_equal(sum(result$explained_variance), 100)
  expect_identical(result$diagnostics$matched_samples, 4L)
})

test_that("PCA reports duplicate and unmatched sample identifiers", {
  expression <- data.frame(
    Feature = paste0("G", 1:4),
    S1 = 1:4,
    S2 = 2:5,
    S3 = 4:7,
    S4 = c(2, 6, 3, 8),
    check.names = FALSE
  )
  duplicated_metadata <- data.frame(
    Sample = c("S1", "S1", "S3"),
    Group = c("A", "B", "B")
  )
  duplicate_result <- bp_compute_pca(expression, duplicated_metadata, list(
    expression_orientation = "genes_by_samples",
    feature_id_location = "column",
    feature_id_field = "Feature",
    metadata_sample_id_field = "Sample",
    transform = "none"
  ))
  expect_false(duplicate_result$ok)
  expect_match(duplicate_result$error, "duplicate|重复", ignore.case = TRUE)

  metadata <- data.frame(Sample = c("S1", "S3", "S4", "S5"), Group = c("A", "B", "C", "D"))
  strict <- bp_compute_pca(expression, metadata, list(
    expression_orientation = "genes_by_samples",
    feature_id_location = "column",
    feature_id_field = "Feature",
    metadata_sample_id_field = "Sample",
    unmatched_sample_policy = "strict",
    transform = "none"
  ))
  expect_false(strict$ok)
  expect_match(strict$error, "match|匹配", ignore.case = TRUE)

  matched <- bp_compute_pca(expression, metadata, list(
    expression_orientation = "genes_by_samples",
    feature_id_location = "column",
    feature_id_field = "Feature",
    metadata_sample_id_field = "Sample",
    unmatched_sample_policy = "matched_only",
    transform = "none"
  ))
  expect_true(matched$ok)
  expect_identical(matched$scores$Sample, c("S1", "S3", "S4"))
  expect_identical(matched$diagnostics$expression_only, "S2")
  expect_identical(matched$diagnostics$metadata_only, "S5")
})

test_that("PCA supports sample-by-feature data and preprocessing", {
  expression <- data.frame(
    Sample = paste0("S", 1:5),
    G1 = c(2, 3, 5, 8, 13),
    G2 = c(1, 4, 2, 7, 9),
    Constant = 4,
    Missing = c(1, 2, NA, 4, 5),
    stringsAsFactors = FALSE
  )
  result <- bp_compute_pca(expression, NULL, list(
    expression_orientation = "samples_by_features",
    expression_sample_id_location = "column",
    expression_sample_id_field = "Sample",
    transform = "log2p1",
    missing_value_policy = "omit_features",
    remove_zero_variance = TRUE,
    variable_feature_count = "all",
    scale = TRUE
  ))

  expect_true(result$ok)
  expect_identical(result$scores$Sample, paste0("S", 1:5))
  expect_identical(result$transform_applied, "log2p1")
  expect_gte(result$zero_variance_removed, 1L)
  expect_identical(result$selected_feature_count, 2L)
})

test_that("PCA analysis code and visual modules execute reproducibly", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_pca_defaults(project)
  config$expression_source_id <- "dataset_example"
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_location <- "column"
  config$feature_id_field <- "gene"
  config$transform <- "none"
  config$variable_feature_count <- 100L
  config$x_component <- "PC1"
  config$y_component <- "PC2"
  config$color_field <- ""
  config$show_ellipse <- FALSE

  applied <- bp_apply_visual_pca_config(project, config, registry)
  project <- applied$project
  analysis_code <- bp_generate_pca_analysis_code(project)
  plot_code <- bp_generate_code(project, registry)

  expect_identical(project$active_data_source_id, "dataset_pca_scores")
  expect_match(analysis_code, "stats::prcomp", fixed = TRUE)
  expect_match(plot_code, "pca_scores")
  expect_match(plot_code, "explained_variance")
  expect_true(all(c("dataset_pca_scores", "dataset_pca_loadings") %in%
    vapply(project$data_sources, `[[`, character(1), "id")))

  environment <- bp_default_environment()
  expect_silent(eval(parse(text = analysis_code), envir = environment))
  expect_true(is.data.frame(environment$pca_scores))
  expect_true(is.data.frame(environment$pca_loadings))
  execution <- bp_execute_project(project, registry, environment = bp_default_environment())
  expect_true(execution$ok, info = execution$error)
  expect_s3_class(execution$plot, "ggplot")
})

test_that("PCA ellipse uses a registered ordinary module", {
  expect_true("r.ggplot2.stat_ellipse" %in% names(registry))
  project <- bp_basic_scatter_project(registry)
  config <- bp_pca_defaults(project)
  config$expression_source_id <- "dataset_example"
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_location <- "column"
  config$feature_id_field <- "gene"
  config$transform <- "none"
  config$color_field <- "gene"
  config$show_ellipse <- TRUE
  applied <- bp_apply_visual_pca_config(project, config, registry)
  roles <- vapply(applied$project$modules, function(instance) instance$visual_role %||% "", character(1))
  expect_true("visual_pca_ellipse" %in% roles)
  expect_match(bp_generate_code(applied$project, registry), "stat_ellipse", fixed = TRUE)
})

test_that("restored PCA projects repair an empty or derived expression source", {
  project <- bp_basic_scatter_project(registry)
  project$active_data_source_id <- "dataset_pca_scores"
  project$data_sources <- c(project$data_sources, list(
    bp_pca_derived_source("dataset_pca_scores", "pca_scores", "scores", bp_pca_defaults(project))
  ))
  project$visual_config$pca <- list(expression_source_id = "")
  expect_identical(bp_pca_config_from_project(project)$expression_source_id, "dataset_example")
})
