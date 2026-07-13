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
