test_that("raw-count DEG heatmap advances through preprocessing, matching, and final rendering", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("edgeR")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }

  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
      state$preview_process <- NULL

      set.seed(91)
      counts <- data.frame(
        ENSEMBL = paste0("ENSG", seq_len(60)),
        matrix(stats::rpois(60 * 6, lambda = 45), nrow = 60, dimnames = list(NULL, paste0("S", seq_len(6)))),
        check.names = FALSE
      )
      differential <- data.frame(
        ENSEMBL = paste0("ENSG", c(2, 4, 6, 8, 10, 12)),
        regulated = c("up", "normal", "down", "NS", "up", "down"),
        stringsAsFactors = FALSE
      )
      count_source <- bp_enrich_data_source(list(
        id = "dataset_counts_heatmap", name = "filter_count", source_type = "rdata", original_file_name = "counts.RData",
        object_type = "data.frame", rows = nrow(counts), columns = ncol(counts), status = "ready",
        example = FALSE, derived = FALSE, relink_required = FALSE,
        semantic_type = "raw_counts", semantic_confirmed = TRUE,
        column_metadata = bp_profile_dataset(counts)$column_metadata,
        quality = list(warnings = list()), parse_options = list()
      ), counts)
      deg_source <- bp_enrich_data_source(list(
        id = "dataset_deg_heatmap", name = "DEG_edgeR_symbol", source_type = "rdata", original_file_name = "deg.RData",
        object_type = "data.frame", rows = nrow(differential), columns = ncol(differential), status = "ready",
        example = FALSE, derived = FALSE, relink_required = FALSE,
        semantic_type = "differential_results", semantic_confirmed = TRUE,
        column_metadata = bp_profile_dataset(differential)$column_metadata,
        quality = list(warnings = list()), parse_options = list()
      ), differential)
      project <- bp_register_data_source(state$project, count_source)
      project <- bp_register_data_source(project, deg_source)
      config <- bp_heatmap_defaults(project)
      config$expression_source_id <- count_source$id
      config$expression_orientation <- "genes_by_samples"
      config$feature_id_location <- "column"
      config$feature_id_field <- "ENSEMBL"
      config$input_semantic_type <- "raw_counts"
      config$feature_selection_mode <- "differential_results"
      config$differential_source_id <- deg_source$id
      config$differential_gene_id_field <- "ENSEMBL"
      config$differential_status_field <- "regulated"
      config$differential_exclude_values <- c("normal", "NS")
      applied <- bp_apply_visual_heatmap_config(project, config, registry)
      state$project <- applied$project
      state$data_objects <- list(
        dataset_counts_heatmap = counts,
        dataset_deg_heatmap = differential
      )
      session$setInputs(
        visual_auto_preview = FALSE,
        visual_pca_expression_source = count_source$id,
        visual_pca_orientation = "genes_by_samples",
        visual_pca_feature_id_field = "ENSEMBL",
        visual_heatmap_feature_mode = "differential_results",
        visual_heatmap_deg_source = deg_source$id,
        visual_heatmap_deg_gene_id = "ENSEMBL",
        visual_heatmap_deg_status = "regulated",
        visual_heatmap_deg_exclude = c("normal", "NS"),
        visual_heatmap_filter_cpm = 0.5,
        visual_heatmap_filter_min_samples = 2,
        visual_heatmap_prior_count = 2
      )
      session$flushReact()

      session$setInputs(visual_heatmap_confirm_recipe = 1)
      session$flushReact()
      expect_true(nzchar(state$project$visual_config$heatmap$raw_count_recipe_confirmed_signature))
      expect_identical(state$project$visual_config$heatmap$differential_match_confirmed_signature, "")
      expect_identical(state$project$analysis_recipes$heatmap$stage, "expression_ready")
      expect_identical(state$preview_status, "waiting")
      expect_true(is.data.frame(state$data_objects$dataset_normalized_expression))
      expect_null(state$data_objects$dataset_heatmap_matrix)
      expect_null(state$preview_process)
      preprocess_html <- htmltools::renderTags(output$visual_heatmap_preprocess_summary)$html
      expect_match(preprocess_html, "logCPM 中间矩阵已就绪", fixed = TRUE)

      session$setInputs(visual_heatmap_validate_deg = 1)
      session$flushReact()
      expect_true(nzchar(state$project$visual_config$heatmap$differential_match_confirmed_signature))
      expect_identical(state$project$analysis_recipes$heatmap$stage, "deg_ready")
      expect_identical(state$preview_status, "waiting")
      expect_true(is.data.frame(state$data_objects$dataset_heatmap_matched_expression))
      expect_null(state$data_objects$dataset_heatmap_matrix)
      expect_null(state$preview_process)
      match_html <- htmltools::renderTags(output$visual_heatmap_deg_match_summary)$html
      expect_match(match_html, "DEG 匹配矩阵已就绪", fixed = TRUE)
      expect_match(match_html, "尚未 Z-score", fixed = TRUE)

      session$setInputs(visual_run_preview = 1)
      session$flushReact()
      expect_identical(state$project$analysis_recipes$heatmap$stage, "plot_ready")
      expect_true(is.data.frame(state$data_objects$dataset_heatmap_matrix))
      expect_true(state$preview_status %in% c("running", "success"))
      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})
