test_that("scatter recommendations use scientific column conventions", {
  source <- bp_example_data_source()
  recommendation <- bp_visual_recommend_scatter_fields(source, bp_default_environment()$df)
  expect_identical(recommendation$x_field, "PC1")
  expect_identical(recommendation$y_field, "PC2")
  expect_identical(recommendation$color_field, "status")
  expect_identical(recommendation$label_field, "gene")
})

test_that("volcano recommendations recognize fold-change and significance columns", {
  source <- bp_example_data_source()
  recommendation <- bp_visual_recommend_volcano_fields(source, bp_default_environment()$df)
  expect_true(recommendation$available)
  expect_identical(recommendation$x_field, "log2FC")
  expect_identical(recommendation$y_field, "neg_log10_padj")
  expect_identical(recommendation$y_scale, "linear")
  expect_identical(recommendation$color_field, "")
  expect_identical(recommendation$status_field, "status")
  expect_identical(recommendation$label_field, "gene")
})

test_that("volcano config compiles automatic regulation groups without automatic reference lines", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_volcano_defaults(project)
  config$x_field <- "log2FC"
  config$y_field <- "padj"
  config$y_scale <- "neg_log10"
  config$color_field <- ""
  config$label_field <- ""
  config$legend_title <- ""

  result <- bp_apply_visual_volcano_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_identical(result$project$visual_config$active_chart_type, "volcano")
  expect_match(code, "y = -log10(padj)", fixed = TRUE)
  expect_match(code, "color = ifelse(log2FC >= 1 & padj <= 0.05", fixed = TRUE)
  expect_match(code, 'color = "Regulation"', fixed = TRUE)
  expect_false(any(grepl("^volcano_", roles)))
  expect_false(grepl("geom_vline|geom_hline", code))
  expect_true(bp_execute_project(result$project, registry)$ok)
})

test_that("switching from volcano back to scatter removes managed threshold layers", {
  project <- bp_basic_scatter_project(registry)
  volcano <- bp_visual_volcano_defaults(project)
  volcano$x_field <- "log2FC"
  volcano$y_field <- "padj"
  volcano$y_scale <- "neg_log10"
  volcano$color_field <- ""
  project <- bp_apply_visual_volcano_config(project, volcano, registry)$project

  scatter <- bp_visual_scatter_defaults(project)
  scatter$x_field <- "PC1"
  scatter$y_field <- "PC2"
  scatter$color_field <- "group"
  result <- bp_apply_visual_scatter_config(project, scatter, registry)
  code <- bp_generate_code(result$project, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_identical(result$project$visual_config$active_chart_type, "scatter")
  expect_false(any(grepl("^volcano_", roles)))
  expect_false(grepl("ifelse\\(", code))
  expect_true(bp_execute_project(result$project, registry)$ok)
})

test_that("scatter config supports multiple vertical and horizontal reference lines", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_scatter_config_from_project(project)
  config$vertical_reference_lines <- "-1, 0, 1"
  config$horizontal_reference_lines <- "1.3; 2"
  config$reference_line_color <- "#6B7280"
  config$reference_line_width <- 0.8

  result <- bp_apply_visual_scatter_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_match(code, "geom_vline(xintercept = c(-1, 0, 1)", fixed = TRUE)
  expect_match(code, "geom_hline(yintercept = c(1.3, 2)", fixed = TRUE)
  expect_match(code, 'color = "#6B7280", linetype = "dashed", linewidth = 0.8', fixed = TRUE)
  expect_true("visual_vertical_reference_lines" %in% roles)
  expect_true("visual_horizontal_reference_lines" %in% roles)
  expect_true(bp_execute_project(result$project, registry)$ok)

  config <- result$config
  config$vertical_reference_lines <- ""
  config$horizontal_reference_lines <- ""
  cleared <- bp_apply_visual_scatter_config(result$project, config, registry)$project
  cleared_roles <- vapply(cleared$modules, function(instance) instance$visual_role %||% "", character(1))
  expect_false(any(grepl("^visual_(vertical|horizontal)_reference_lines$", cleared_roles)))
})

test_that("reference-line validation rejects non-numeric positions", {
  config <- bp_visual_scatter_defaults()
  config$x_field <- "PC1"
  config$y_field <- "PC2"
  config$vertical_reference_lines <- "-1, invalid, 1"
  validation <- bp_validate_visual_scatter_config(config, c("PC1", "PC2"))

  expect_false(validation$valid)
  expect_true(any(grepl("invalid", validation$errors, fixed = TRUE)))

  config$vertical_reference_lines <- "-1; 0 1"
  expect_true(bp_validate_visual_scatter_config(config, c("PC1", "PC2"))$valid)
})

test_that("volcano displays only custom reference lines", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_volcano_defaults(project)
  config$x_field <- "log2FC"
  config$y_field <- "padj"
  config$y_scale <- "neg_log10"
  config$color_field <- ""
  config$vertical_reference_lines <- "0"
  config$horizontal_reference_lines <- "2"

  result <- bp_apply_visual_volcano_config(project, config, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_true(all(c("visual_vertical_reference_lines", "visual_horizontal_reference_lines") %in% roles))
  expect_false(any(grepl("^volcano_", roles)))
  expect_equal(sum(vapply(result$project$modules, function(instance) identical(instance$module_id, "r.ggplot2.geom_vline"), logical(1))), 1L)
  expect_equal(sum(vapply(result$project$modules, function(instance) identical(instance$module_id, "r.ggplot2.geom_hline"), logical(1))), 1L)
  expect_true(bp_execute_project(result$project, registry)$ok)
})

test_that("visual scatter settings compile through ordinary modules", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  config <- bp_visual_scatter_config_from_project(project)
  config$trend_line <- "linear"
  config$palette <- "blue_red"
  config$label_field <- "gene"
  config$title <- "Visual volcano"
  config$y_field <- "padj"
  config$y_scale <- "neg_log10"

  result <- bp_apply_visual_scatter_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)
  module_ids <- vapply(result$project$modules, `[[`, character(1), "module_id")

  expect_match(code, "aes(x = log2FC, y = -log10(padj), color = status, label = gene)", fixed = TRUE)
  expect_match(code, 'geom_smooth(method = "lm"', fixed = TRUE)
  expect_match(code, "geom_text(", fixed = TRUE)
  expect_match(code, 'labs(title = "Visual volcano"', fixed = TRUE)
  expect_false("r.ggplot2.geom_vline" %in% module_ids)
  expect_false("r.ggplot2.geom_hline" %in% module_ids)
  expect_true(isTRUE(result$config$advanced_preserved))
  expect_true(bp_execute_project(result$project, registry)$ok)
})

test_that("fixed point aesthetics and mapped aesthetics stay distinct", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_scatter_config_from_project(project)
  config$color_field <- "group"
  config$size_field <- "expression"
  result <- bp_apply_visual_scatter_config(project, config, registry)
  point <- Filter(function(instance) identical(instance$module_id, "r.ggplot2.geom_point"), result$project$modules)[[1]]
  root <- Filter(function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), result$project$modules)[[1]]

  expect_true(bp_is_unset(point$arguments$color))
  expect_true(bp_is_unset(point$arguments$size))
  expect_identical(root$arguments$mapping$value$mappings$color$name, "group")
  expect_identical(root$arguments$mapping$value$mappings$size$name, "expression")
})

test_that("data-source changes clear unavailable visual fields", {
  project <- bp_basic_scatter_project(registry)
  root_index <- which(vapply(project$modules, function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), logical(1)))[[1]]
  project$modules[[root_index]]$arguments$mapping <- bp_argument(
    "explicit",
    bp_aes_mapping(list(x = bp_symbol("keep_x"), y = bp_symbol("missing_y"), color = bp_symbol("group"))),
    "formal"
  )
  data <- data.frame(keep_x = 1:4, group = c("A", "A", "B", "B"))
  source <- list(
    id = "dataset_visual", name = "visual_data", source_type = "csv",
    original_file_name = "visual.csv", object_type = "data.frame", rows = 4L,
    columns = 2L, status = "ready", example = FALSE, relink_required = FALSE,
    column_metadata = list(), quality = list(), parse_options = list()
  )
  switched <- bp_switch_project_data_source(project, source, data)
  config <- bp_visual_scatter_config_from_project(switched$project)

  expect_identical(config$x_field, "keep_x")
  expect_identical(config$y_field, "")
  expect_identical(config$color_field, "group")
  expect_false(bp_validate_visual_scatter_config(config, names(data))$valid)
})

test_that("visual config survives project persistence", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_scatter_config_from_project(project)
  config$title <- "Persistent visual plot"
  config$trend_line <- "smooth"
  project <- bp_apply_visual_scatter_config(project, config, registry)$project
  path <- tempfile(fileext = ".json")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  bp_save_project(project, path)
  restored <- bp_load_project(path)
  expect_identical(restored$visual_config$scatter$title, "Persistent visual plot")
  expect_identical(restored$visual_config$scatter$trend_line, "smooth")
})
