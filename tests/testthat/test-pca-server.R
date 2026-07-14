test_that("visual PCA workflow creates derived results and reproducible code", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }

  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      session$setInputs(visual_auto_preview = FALSE, visual_chart_pca = 0)
      session$flushReact()
      session$setInputs(visual_chart_pca = 1)
      session$flushReact()
      session$elapse(900)
      session$flushReact()

      expect_identical(state$project$visual_config$active_chart_type, "pca")
      expect_identical(state$project$active_data_source_id, "dataset_pca_scores")
      expect_true(is.data.frame(state$data_objects$dataset_pca_scores))
      expect_true(is.data.frame(state$data_objects$dataset_pca_loadings))
      expect_true(all(c("Sample", "PC1", "PC2") %in% names(state$data_objects$dataset_pca_scores)))

      profile_html <- htmltools::renderTags(output$visual_data_profile)$html
      expect_match(profile_html, "数据对象", fixed = TRUE)
      expect_match(profile_html, ">df<", fixed = TRUE)
      expect_false(grepl(">pca_scores<", profile_html, fixed = TRUE))

      manager_html <- htmltools::renderTags(output$data_source_manager_list)$html
      expect_match(manager_html, "PCA 样本得分", fixed = TRUE)
      expect_match(manager_html, "PCA 特征载荷", fixed = TRUE)
      expect_false(grepl("subscript out of bounds", manager_html, fixed = TRUE))

      analysis <- bp_generate_pca_analysis_code(state$project)
      generated <- bp_generate_code(state$project, registry)
      expect_match(analysis, "stats::prcomp", fixed = TRUE)
      expect_match(generated, "pca_scores", fixed = TRUE)
      expect_match(generated, "explained_variance", fixed = TRUE)

      session$setInputs(
        visual_pca_label = "Sample",
        visual_pca_transform = "log2p1",
        visual_pca_feature_count = "500",
        visual_pca_center = TRUE,
        visual_pca_scale = TRUE
      )
      session$elapse(700)
      session$flushReact()
      expect_identical(state$project$visual_config$pca$label_field, "Sample")
      expect_identical(state$project$visual_config$pca$transform, "log2p1")
      expect_identical(state$project$visual_config$pca$variable_feature_count, 500L)
      expect_true(state$project$visual_config$pca$scale)
      expect_match(bp_generate_code(state$project, registry), "geom_text", fixed = TRUE)

      history_length <- length(state$history)
      session$setInputs(visual_pca_x_component = "PC2", visual_pca_y_component = "PC1")
      session$elapse(700)
      session$flushReact()
      expect_gt(length(state$history), history_length)
      expect_identical(state$project$visual_config$pca$x_component, "PC2")
      session$setInputs(visual_undo = 1)
      session$flushReact()
      expect_identical(state$project$visual_config$pca$x_component, "PC1")
      session$setInputs(visual_redo = 1)
      session$flushReact()
      expect_identical(state$project$visual_config$pca$x_component, "PC2")

      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})

test_that("visual raw-count workflow requires semantics and recipe confirmation", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }

  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      set.seed(17)
      counts <- data.frame(
        Gene = paste0("g", seq_len(60)),
        matrix(stats::rpois(60 * 6, lambda = 30), nrow = 60),
        check.names = FALSE
      )
      names(counts)[-1] <- paste0("S", seq_len(6))
      source <- bp_enrich_data_source(list(
        id = "dataset_counts", name = "counts_raw", source_type = "csv", original_file_name = "counts.csv",
        object_type = "data.frame", rows = nrow(counts), columns = ncol(counts), status = "ready",
        example = FALSE, derived = FALSE, relink_required = FALSE,
        column_metadata = bp_profile_dataset(counts)$column_metadata,
        quality = list(warnings = list()), parse_options = list()
      ), counts)
      project <- bp_register_data_source(state$project, source)
      config <- bp_pca_defaults(project)
      config$expression_source_id <- source$id
      config$expression_orientation <- "genes_by_samples"
      config$feature_id_location <- "column"
      config$feature_id_field <- "Gene"
      applied <- bp_apply_visual_pca_config(project, config, registry)
      state$project <- applied$project
      state$data_objects <- list(dataset_counts = counts)
      session$setInputs(visual_auto_preview = FALSE)
      session$flushReact()

      passport_html <- htmltools::renderTags(output$visual_data_semantics)$html
      expect_match(passport_html, "建议 · RNA-seq Raw count", fixed = TRUE)
      compatibility_html <- htmltools::renderTags(output$visual_chart_compatibility)$html
      expect_match(compatibility_html, "需要补充信息", fixed = TRUE)

      session$setInputs(visual_semantic_type = "raw_counts", visual_confirm_semantic = 1)
      session$flushReact()
      confirmed <- Filter(function(item) identical(item$id, source$id), state$project$data_sources)[[1]]
      expect_true(confirmed$semantic_confirmed)
      recipe_html <- htmltools::renderTags(output$visual_pca_recipe_panel)$html
      expect_match(recipe_html, "为 PCA 确认 Raw count 分析配方", fixed = TRUE)

      session$setInputs(
        visual_pca_filter_cpm = 0.5,
        visual_pca_filter_min_samples = 2,
        visual_pca_normalization = "log2p1",
        visual_pca_prior_count = 2,
        visual_pca_confirm_recipe = 1
      )
      session$flushReact()
      expect_true(nzchar(state$project$visual_config$pca$raw_count_recipe_confirmed_signature))
      expect_true(is.data.frame(state$data_objects$dataset_normalized_expression))
      expect_true(is.data.frame(state$data_objects$dataset_pca_scores))
      normalized_source <- Filter(function(item) identical(item$id, "dataset_normalized_expression"), state$project$data_sources)[[1]]
      expect_identical(normalized_source$status, "ready")
      expect_identical(normalized_source$lineage$analysis, "raw_count_pca")
      expect_match(bp_generate_pca_analysis_code(state$project), ".bioplotblocks_prepare_raw_counts", fixed = TRUE)
      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})

test_that("the complete matrix source form is shared by ordinary chart types", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }

  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      shared_data <- data.frame(
        sample_id = paste0("S", seq_len(8)),
        x_value = seq_len(8),
        y_value = seq_len(8) * 2,
        group = rep(c("A", "B"), each = 4),
        check.names = FALSE
      )
      source <- bp_enrich_data_source(list(
        id = "dataset_shared_matrix", name = "shared_matrix", source_type = "csv", original_file_name = "shared.csv",
        object_type = "data.frame", rows = nrow(shared_data), columns = ncol(shared_data), status = "ready",
        example = FALSE, derived = FALSE, relink_required = FALSE,
        column_metadata = bp_profile_dataset(shared_data)$column_metadata,
        quality = list(warnings = list()), parse_options = list()
      ), shared_data)
      state$project <- bp_register_data_source(state$project, source)
      state$data_objects <- list(dataset_shared_matrix = shared_data)
      session$setInputs(visual_auto_preview = FALSE)
      session$flushReact()
      session$elapse(1200)
      session$flushReact()

      history_length <- length(state$history)
      session$setInputs(
        visual_pca_expression_source = "dataset_shared_matrix",
        visual_pca_orientation = "samples_by_features",
        visual_pca_feature_id_field = "sample_id",
        visual_pca_expression_sample_id_field = "sample_id",
        visual_pca_metadata_source = "",
        visual_pca_unmatched_policy = "matched_only"
      )
      session$flushReact()
      session$elapse(1200)
      session$flushReact()

      expect_identical(bp_visual_chart_type(state$project), "scatter")
      expect_identical(state$project$active_data_source_id, "dataset_shared_matrix")
      expect_identical(bp_visual_config_from_project(state$project)$data_source_id, "dataset_shared_matrix")
      expect_identical(state$project$visual_config$pca$expression_source_id, "dataset_shared_matrix")
      expect_identical(state$project$visual_config$pca$expression_orientation, "samples_by_features")
      expect_identical(state$project$visual_config$pca$feature_id_field, "sample_id")
      expect_identical(state$project$visual_config$pca$expression_sample_id_field, "sample_id")
      expect_identical(state$project$visual_config$pca$unmatched_sample_policy, "matched_only")
      expect_gt(length(state$history), history_length)
      profile_html <- htmltools::renderTags(output$visual_data_profile)$html
      expect_match(profile_html, "shared_matrix", fixed = TRUE)
      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})
