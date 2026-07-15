test_that("heatmap clusters samples automatically without metadata", {
  expression <- data.frame(
    Feature = paste0("G", 1:8),
    S1 = c(2, 4, 7, 3, 8, 6, 5, 9),
    S2 = c(3, 5, 9, 4, 7, 5, 6, 8),
    S3 = c(8, 6, 2, 9, 3, 4, 7, 1),
    S4 = c(9, 7, 3, 8, 2, 5, 6, 1),
    check.names = FALSE
  )
  result <- bp_compute_heatmap(expression, NULL, list(
    expression_orientation = "genes_by_samples",
    feature_id_field = "Feature",
    variable_feature_count = "all",
    row_zscore = TRUE,
    sample_order = "auto_cluster"
  ))

  expect_true(result$ok, info = result$error)
  expect_identical(result$sample_order, "auto_cluster")
  expect_identical(result$sample_count, 4L)
  expect_length(result$annotation_fields, 0L)
  z <- as.matrix(result$heatmap_matrix[, -1, drop = FALSE])
  expect_equal(unname(rowMeans(z)), rep(0, nrow(z)), tolerance = 1e-10)
  expect_s3_class(result$plot, "ggplot")
})

test_that("heatmap uses metadata bars, fixed group order, and group splits", {
  expression <- data.frame(
    Feature = paste0("G", 1:10),
    S1 = 1:10, S2 = 2:11, S3 = 11:2, S4 = 10:1,
    S5 = c(3, 8, 2, 7, 4, 9, 5, 10, 6, 11),
    S6 = c(4, 9, 3, 8, 5, 10, 6, 11, 7, 12),
    check.names = FALSE
  )
  metadata <- data.frame(
    Sample = c("S4", "S1", "S6", "S2", "S5", "S3"),
    Group = c("Treatment", "Control", "Treatment", "Control", "Treatment", "Control"),
    Batch = c("B2", "B1", "B2", "B1", "B1", "B2"),
    Sex = c("F", "F", "M", "M", "F", "M")
  )
  base_config <- list(
    expression_orientation = "genes_by_samples",
    feature_id_field = "Feature",
    metadata_sample_id_field = "Sample",
    variable_feature_count = "all",
    group_field = "Group",
    annotation_fields = c("Group", "Batch", "Sex")
  )

  fixed <- bp_compute_heatmap(expression, metadata, c(base_config, list(sample_order = "group_fixed")))
  expect_true(fixed$ok, info = fixed$error)
  expect_identical(as.character(fixed$metadata$Group), c("Control", "Control", "Control", "Treatment", "Treatment", "Treatment"))
  expect_setequal(fixed$annotation_fields, c("Group", "Batch", "Sex"))

  split <- bp_compute_heatmap(expression, metadata, c(base_config, list(sample_order = "group_split")))
  expect_true(split$ok, info = split$error)
  expect_identical(levels(split$long$Split), c("Control", "Treatment"))
  expect_s3_class(split$plot, "ggplot")
})

test_that("DEG heatmap filters non-significant genes and matches expression IDs", {
  expression <- data.frame(
    ENSEMBL = paste0("ENSG", 1:8),
    Control_1 = c(8, 4, 3, 9, 2, 6, 3, 5),
    Control_2 = c(9, 5, 4, 8, 3, 7, 2, 4),
    Treatment_1 = c(2, 4, 9, 3, 8, 5, 7, 6),
    Treatment_2 = c(3, 3, 8, 2, 9, 4, 8, 7),
    check.names = FALSE
  )
  differential <- data.frame(
    ENSEMBL = c("ENSG1", "ENSG2", "ENSG4", "ENSG7", "ENSG999"),
    regulated = c("up", "normal", "down", "NS", "up"),
    logFC = c(2.1, 0.1, -2.4, 0.2, 3.2),
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    run_accession = c("Control_1", "Control_2", "Treatment_1", "Treatment_2"),
    sample_title = c("Control", "Control", "Treatment", "Treatment"),
    Batch = c("B1", "B2", "B1", "B2")
  )
  config <- bp_heatmap_defaults()
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_field <- "ENSEMBL"
  config$metadata_sample_id_field <- "run_accession"
  config$feature_selection_mode <- "differential_results"
  config$differential_gene_id_field <- "ENSEMBL"
  config$differential_status_field <- "regulated"
  config$differential_exclude_values <- c("normal", "NS")
  config$group_field <- "sample_title"
  config$annotation_fields <- c("sample_title", "Batch")
  config$sample_order <- "group_fixed"

  result <- bp_compute_heatmap(expression, metadata, config, differential_data = differential)

  expect_true(result$ok, info = result$error)
  expect_setequal(result$heatmap_matrix$Feature, c("ENSG1", "ENSG4"))
  expect_identical(result$diagnostics$significant_gene_ids, 3L)
  expect_identical(result$diagnostics$matched_gene_ids, 2L)
  expect_identical(result$diagnostics$unmatched_gene_ids, "ENSG999")
  expect_identical(as.character(result$metadata$sample_title), c("Control", "Control", "Treatment", "Treatment"))
  expect_match(paste(result$warnings, collapse = " "), "1 个显著基因未在表达矩阵中找到")
  expect_s3_class(result$plot, "ggplot")
})

test_that("raw count heatmap always runs TMM logCPM followed by gene Z-score", {
  skip_if_not_installed("edgeR")
  set.seed(42)
  counts <- data.frame(
    Gene = paste0("G", seq_len(40)),
    matrix(stats::rpois(40 * 6, lambda = 50), nrow = 40,
      dimnames = list(NULL, paste0("S", 1:6))),
    check.names = FALSE
  )
  config <- bp_heatmap_defaults()
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_field <- "Gene"
  config$input_semantic_type <- "raw_counts"
  config$raw_count_normalization <- "tmm_logcpm"
  config$variable_feature_count <- 20L
  config$row_zscore <- TRUE
  config$raw_count_recipe_confirmed_signature <- bp_heatmap_recipe_signature(config)

  result <- bp_compute_heatmap(counts, NULL, config)
  expect_true(result$ok, info = result$error)
  expect_true(is.data.frame(result$normalized_expression))
  expect_match(result$preparation$normalization_label, "TMM.*logCPM", ignore.case = TRUE)
  expect_identical(result$selected_feature_count, 20L)
  z <- as.matrix(result$heatmap_matrix[, -1, drop = FALSE])
  expect_equal(unname(rowMeans(z)), rep(0, nrow(z)), tolerance = 1e-10)
})

test_that("raw count DEG heatmap normalizes all counts before matching significant genes", {
  skip_if_not_installed("edgeR")
  set.seed(7)
  counts <- data.frame(
    ENSEMBL = paste0("ENSG", seq_len(30)),
    matrix(stats::rpois(30 * 6, lambda = 80), nrow = 30, dimnames = list(NULL, paste0("S", 1:6))),
    check.names = FALSE
  )
  differential <- data.frame(
    ENSEMBL = paste0("ENSG", c(2, 4, 6, 8, 10)),
    regulated = c("up", "normal", "down", "NS", "up")
  )
  config <- bp_heatmap_defaults()
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_field <- "ENSEMBL"
  config$input_semantic_type <- "raw_counts"
  config$feature_selection_mode <- "differential_results"
  config$differential_gene_id_field <- "ENSEMBL"
  config$differential_status_field <- "regulated"
  config$differential_exclude_values <- c("normal", "NS")
  config$raw_count_recipe_confirmed_signature <- bp_heatmap_recipe_signature(config)

  result <- bp_compute_heatmap(counts, NULL, config, differential_data = differential)

  expect_true(result$ok, info = result$error)
  expect_setequal(result$heatmap_matrix$Feature, c("ENSG2", "ENSG6", "ENSG10"))
  expect_true(is.data.frame(result$normalized_expression))
  expect_match(result$preparation$normalization_label, "TMM.*logCPM", ignore.case = TRUE)
})

test_that("heatmap preprocessing and DEG confirmations have independent signatures", {
  config <- bp_heatmap_defaults()
  config$input_semantic_type <- "raw_counts"
  config$expression_source_id <- "counts"
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_field <- "ENSEMBL"
  preprocessing <- bp_heatmap_recipe_signature(config)

  downstream <- config
  downstream$differential_source_id <- "deg"
  downstream$differential_status_field <- "regulated"
  downstream$metadata_source_id <- "samples"
  downstream$cluster_columns <- FALSE
  expect_identical(bp_heatmap_recipe_signature(downstream), preprocessing)

  changed_filter <- config
  changed_filter$raw_count_filter_cpm <- 1
  expect_false(identical(bp_heatmap_recipe_signature(changed_filter), preprocessing))

  first_match <- bp_heatmap_deg_match_signature(downstream)
  downstream$differential_exclude_values <- c("normal", "NS", "stable")
  expect_false(identical(bp_heatmap_deg_match_signature(downstream), first_match))
})

test_that("heatmap matrix preparation can run without rendering a plot", {
  expression <- data.frame(
    Feature = paste0("G", 1:8),
    S1 = 1:8, S2 = 2:9, S3 = 9:2, S4 = 8:1,
    check.names = FALSE
  )
  result <- bp_compute_heatmap(expression, NULL, list(
    expression_orientation = "genes_by_samples",
    feature_id_field = "Feature",
    variable_feature_count = "all"
  ), render_plot = FALSE)
  expect_true(result$ok, info = result$error)
  expect_true(is.null(result$plot))
  expect_true(is.data.frame(result$heatmap_matrix))
})

test_that("heatmap analysis code and visual project execute reproducibly", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_heatmap_defaults(project)
  config$expression_source_id <- "dataset_example"
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_field <- "gene"
  config$variable_feature_count <- 30L
  applied <- bp_apply_visual_heatmap_config(project, config, registry)
  project <- applied$project

  expect_identical(project$active_data_source_id, "dataset_heatmap_matrix")
  expect_identical(project$modules[[1]]$visual_role, "visual_heatmap_plot")
  expect_match(bp_generate_heatmap_analysis_code(project), "gene Z-score|heatmap", ignore.case = TRUE)
  expect_match(bp_generate_code(project, registry), "heatmap_plot", fixed = TRUE)
  expect_true("dataset_heatmap_matrix" %in% vapply(project$data_sources, `[[`, character(1), "id"))

  execution <- bp_execute_project(project, registry, environment = bp_default_environment())
  expect_true(execution$ok, info = execution$error)
  expect_s3_class(execution$plot, "ggplot")
})

test_that("DEG heatmap code generation references the differential-results source", {
  project <- bp_basic_scatter_project(registry)
  differential_source <- bp_example_data_source()
  differential_source$id <- "dataset_deg_results"
  differential_source$name <- "DEG_edgeR_symbol"
  differential_source$object_name <- "DEG_edgeR_symbol"
  differential_source$semantic_type <- "differential_results"
  differential_source$semantic_confirmed <- TRUE
  project$data_sources <- c(project$data_sources, list(differential_source))

  config <- bp_heatmap_defaults(project)
  config$expression_source_id <- "dataset_example"
  config$expression_orientation <- "genes_by_samples"
  config$feature_id_field <- "gene"
  config$feature_selection_mode <- "differential_results"
  config$differential_source_id <- differential_source$id
  config$differential_gene_id_field <- "ENSEMBL"
  config$differential_status_field <- "regulated"
  config$differential_match_confirmed_signature <- bp_heatmap_deg_match_signature(config)
  project <- bp_apply_visual_heatmap_config(project, config, registry)$project

  code <- bp_generate_heatmap_analysis_code(project)
  expect_match(code, "differential_data = DEG_edgeR_symbol", fixed = TRUE)
  expect_match(code, "pheatmap::pheatmap", fixed = TRUE)
  expect_true(differential_source$id %in% project$analysis_recipes$heatmap$input_source_ids)
})
