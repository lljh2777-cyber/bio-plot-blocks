test_that("visual violin card builds and restores an independent module stack", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }

  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      session$setInputs(
        visual_auto_preview = FALSE,
        visual_workflow_mode = "generic",
        visual_chart_violin = 0,
        visual_chart_boxplot = 0
      )
      session$flushReact()

      session$setInputs(visual_chart_violin = 1)
      session$flushReact()

      code <- bp_generate_code(state$project, registry)
      roles <- vapply(state$project$modules, function(instance) instance$visual_role %||% "", character(1))
      expect_identical(state$project$visual_config$active_chart_type, "violin")
      expect_identical(state$project$visual_config$violin$x_field, "group")
      expect_identical(state$project$visual_config$violin$y_field, "value")
      expect_match(code, "geom_violin", fixed = TRUE)
      expect_true("visual_violin_layer" %in% roles)

      session$elapse(900)
      session$flushReact()

      session$setInputs(
        visual_point_color = "#90CAF9",
        visual_violin_border_color = "#102A43",
        visual_violin_trim = FALSE,
        visual_violin_scale = "width",
        visual_violin_show_median = FALSE
      )
      session$elapse(900)
      session$flushReact()

      updated_code <- bp_generate_code(state$project, registry)
      expect_match(updated_code, 'color = "#102A43"', fixed = TRUE)
      expect_match(updated_code, 'trim = FALSE, scale = "width"', fixed = TRUE)
      expect_false(grepl("quantiles", updated_code, fixed = TRUE))

      session$setInputs(visual_chart_boxplot = 1)
      session$flushReact()
      expect_identical(state$project$visual_config$active_chart_type, "boxplot")
      expect_match(bp_generate_code(state$project, registry), "geom_boxplot", fixed = TRUE)

      session$setInputs(visual_chart_violin = 2)
      session$flushReact()
      restored_code <- bp_generate_code(state$project, registry)
      expect_identical(state$project$visual_config$active_chart_type, "violin")
      expect_match(restored_code, 'color = "#102A43"', fixed = TRUE)
      expect_match(restored_code, 'trim = FALSE, scale = "width"', fixed = TRUE)

      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})
