test_that("visual chart cards switch between volcano and scatter module stacks", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }

  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      session$setInputs(visual_auto_preview = FALSE, visual_chart_volcano = 0, visual_chart_scatter = 0)
      session$flushReact()
      session$setInputs(visual_chart_volcano = 1)
      session$flushReact()

      volcano_code <- bp_generate_code(state$project, registry)
      volcano_roles <- vapply(state$project$modules, function(instance) instance$visual_role %||% "", character(1))
      expect_identical(state$project$visual_config$active_chart_type, "volcano")
      expect_identical(state$project$visual_config$volcano$x_field, "log2FC")
      expect_identical(state$project$visual_config$volcano$y_field, "neg_log10_padj")
      expect_identical(state$project$visual_config$volcano$color_field, "")
      expect_match(volcano_code, "color = ifelse(log2FC >= 1 & neg_log10_padj >= -log10(0.05)", fixed = TRUE)
      expect_false(any(grepl("^volcano_", volcano_roles)))
      expect_false(grepl("geom_vline|geom_hline", volcano_code))

      session$setInputs(visual_chart_scatter = 1)
      session$flushReact()
      scatter_roles <- vapply(state$project$modules, function(instance) instance$visual_role %||% "", character(1))
      expect_identical(state$project$visual_config$active_chart_type, "scatter")
      expect_false(any(grepl("^volcano_", scatter_roles)))
      expect_false(grepl("ifelse\\(", bp_generate_code(state$project, registry)))

      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})
