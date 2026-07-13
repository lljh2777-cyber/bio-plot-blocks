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
      expect_length(state$project$modules, 1L)
      expect_identical(state$project$modules[[1]]$module_id, "r.ggplot2.ggplot")
      expect_identical(state$selected, state$project$modules[[1]]$instance_id)
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

      infected <- state$project
      volcano_defaults <- bp_visual_volcano_defaults(infected)
      label_fields <- c("title", "x_label", "y_label", "legend_title")
      for (field in label_fields) infected$visual_config$boxplot[[field]] <- volcano_defaults[[field]]
      state$project <- infected
      session$flushReact()

      session$setInputs(visual_chart_boxplot = 1)
      session$flushReact()
      boxplot_code <- bp_generate_code(state$project, registry)
      boxplot_roles <- vapply(state$project$modules, function(instance) instance$visual_role %||% "", character(1))
      expect_identical(state$project$visual_config$active_chart_type, "boxplot")
      expect_identical(state$project$visual_config$boxplot$x_field, "group")
      expect_identical(state$project$visual_config$boxplot$y_field, "value")
      expect_identical(state$project$visual_config$boxplot$title, "Boxplot")
      expect_identical(state$project$visual_config$boxplot$x_label, "Group")
      expect_identical(state$project$visual_config$boxplot$y_label, "Value")
      expect_identical(state$project$visual_config$boxplot$legend_title, "Group")
      expect_match(boxplot_code, "geom_boxplot", fixed = TRUE)
      expect_match(boxplot_code, 'labs(title = "Boxplot", x = "Group", y = "Value", fill = "Group")', fixed = TRUE)
      expect_true("visual_boxplot_layer" %in% boxplot_roles)

      session$setInputs(
        visual_box_jitter = TRUE,
        visual_box_jitter_color = "#102A43",
        visual_box_jitter_size = 1.8,
        visual_box_jitter_alpha = 0.4,
        visual_box_jitter_width = 0.22
      )
      session$elapse(500)
      session$flushReact()
      jitter_code <- bp_generate_code(state$project, registry)
      jitter_roles <- vapply(state$project$modules, function(instance) instance$visual_role %||% "", character(1))
      expect_match(jitter_code, "geom_jitter", fixed = TRUE)
      expect_match(jitter_code, "width = 0.22", fixed = TRUE)
      expect_true("visual_boxplot_jitter" %in% jitter_roles)
      expect_false(state$project$visual_config$boxplot$box_show_outliers)
      expect_true(state$project$visual_config$boxplot$box_outlier_restore)
      expect_match(jitter_code, "outlier.shape = NA_integer_", fixed = TRUE)

      session$setInputs(interface_mode = list(value = "advanced"))
      session$flushReact()
      offset_project <- state$project
      root_index <- bp_visual_first_instance(offset_project, "r.ggplot2.ggplot")
      root_mapping <- offset_project$modules[[root_index]]$arguments$mapping
      root_mapping$value$mappings$group <- bp_symbol("status")
      offset_project$modules[[root_index]]$arguments$mapping <- root_mapping
      box_index <- bp_visual_first_instance(offset_project, "r.ggplot2.geom_boxplot")
      offset_project$modules[[box_index]]$arguments$notch <- bp_argument("explicit", bp_logical(TRUE), "formal")
      state$project <- offset_project
      session$flushReact()
      expect_match(bp_generate_code(state$project, registry), "group = status", fixed = TRUE)

      session$setInputs(interface_mode = list(value = "visual"))
      session$flushReact()
      aligned_code <- bp_generate_code(state$project, registry)
      expect_false(grepl("group = status", aligned_code, fixed = TRUE))
      expect_match(aligned_code, "notch = TRUE", fixed = TRUE)
      session$elapse(500)
      session$flushReact()

      session$setInputs(visual_box_show_outliers = TRUE)
      session$elapse(500)
      session$flushReact()
      expect_true(state$project$visual_config$boxplot$box_show_outliers)

      session$setInputs(visual_box_jitter = FALSE)
      session$elapse(500)
      session$flushReact()
      restored_code <- bp_generate_code(state$project, registry)
      expect_true(state$project$visual_config$boxplot$box_show_outliers)
      expect_false(grepl("geom_jitter", restored_code, fixed = TRUE))
      expect_match(restored_code, "outlier.shape = 16", fixed = TRUE)

      session$setInputs(visual_box_show_outliers = FALSE)
      session$elapse(900)
      session$flushReact()
      expect_false(state$project$visual_config$boxplot$box_show_outliers)
      session$setInputs(visual_box_jitter = TRUE)
      session$elapse(900)
      session$flushReact()
      expect_false(state$project$visual_config$boxplot$box_show_outliers)
      expect_false(state$project$visual_config$boxplot$box_outlier_restore)
      session$setInputs(visual_box_jitter = FALSE)
      session$elapse(900)
      session$flushReact()
      expect_false(state$project$visual_config$boxplot$box_show_outliers)
      expect_false(state$project$visual_config$boxplot$box_outlier_restore)

      session$setInputs(visual_chart_volcano = 2)
      session$flushReact()
      volcano_restored_code <- bp_generate_code(state$project, registry)
      expect_identical(state$project$visual_config$active_chart_type, "volcano")
      expect_identical(state$project$visual_config$volcano$title, "Volcano plot")
      expect_identical(state$project$visual_config$volcano$x_label, "log2 Fold Change")
      expect_identical(state$project$visual_config$volcano$y_label, "-log10 adjusted p-value")
      expect_identical(state$project$visual_config$volcano$legend_title, "Regulation")
      expect_match(volcano_restored_code, 'labs(title = "Volcano plot", x = "log2 Fold Change", y = "-log10 adjusted p-value", color = "Regulation")', fixed = TRUE)

      session$setInputs(visual_chart_scatter = 1)
      session$flushReact()
      scatter_roles <- vapply(state$project$modules, function(instance) instance$visual_role %||% "", character(1))
      expect_identical(state$project$visual_config$active_chart_type, "scatter")
      expect_false(any(grepl("^volcano_", scatter_roles)))
      expect_false(any(grepl("^visual_boxplot", scatter_roles)))
      expect_false(grepl("ifelse\\(", bp_generate_code(state$project, registry)))

      session$setInputs(interface_mode = list(value = "advanced"))
      session$flushReact()
      session$setInputs(new_project = 1)
      session$elapse(900)
      session$flushReact()
      expect_length(state$project$modules, 1L)
      expect_identical(state$project$modules[[1]]$module_id, "r.ggplot2.ggplot")
      expect_identical(state$selected, state$project$modules[[1]]$instance_id)

      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})
