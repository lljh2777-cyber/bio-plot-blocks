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

test_that("boxplot recommendations recognize grouping and numeric value columns", {
  source <- bp_example_data_source()
  recommendation <- bp_visual_recommend_boxplot_fields(source, bp_default_environment()$df)
  expect_true(recommendation$available)
  expect_identical(recommendation$x_field, "group")
  expect_identical(recommendation$y_field, "value")
  expect_identical(recommendation$color_field, "group")
})

test_that("cross-chart default labels are repaired without overwriting custom labels", {
  volcano <- bp_visual_volcano_defaults()
  infected_box <- bp_visual_boxplot_defaults()
  fields <- c("title", "x_label", "y_label", "legend_title")
  for (field in fields) infected_box[[field]] <- volcano[[field]]

  repaired <- bp_visual_repair_cross_chart_labels(infected_box, "boxplot")
  box_defaults <- bp_visual_boxplot_defaults()
  expect_identical(repaired[fields], box_defaults[fields])

  infected_box$title <- "Custom distribution title"
  preserved <- bp_visual_repair_cross_chart_labels(infected_box, "boxplot")
  expect_identical(preserved[fields], infected_box[fields])
})

test_that("boxplot config compiles grouped distributions through ordinary modules", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_boxplot_defaults(project)
  config$x_field <- "group"
  config$y_field <- "value"
  config$color_field <- "group"
  config$palette <- "viridis_like"
  config$title <- "Grouped values"

  result <- bp_apply_visual_boxplot_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_identical(result$project$visual_config$active_chart_type, "boxplot")
  expect_match(code, "aes(x = group, y = value, fill = group)", fixed = TRUE)
  expect_match(code, 'geom_boxplot(color = "#334155", width = 0.65, alpha = 0.85', fixed = TRUE)
  expect_match(code, "scale_fill_manual", fixed = TRUE)
  expect_false(grepl("geom_point", code, fixed = TRUE))
  expect_true("visual_boxplot_layer" %in% roles)
  expect_true("visual_boxplot_fill_scale" %in% roles)
  expect_true(bp_execute_project(result$project, registry)$ok)
})

test_that("boxplot can hide outliers and use a fixed fill", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_boxplot_defaults(project)
  config$x_field <- "condition"
  config$y_field <- "expression"
  config$color_field <- ""
  config$point_color <- "#90CAF9"
  config$box_show_outliers <- FALSE

  result <- bp_apply_visual_boxplot_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)

  expect_match(code, 'geom_boxplot(color = "#334155", fill = "#90CAF9"', fixed = TRUE)
  expect_match(code, "outlier.shape = NA", fixed = TRUE)
  expect_false(grepl("scale_fill_manual", code, fixed = TRUE))
  expect_true(bp_execute_project(result$project, registry)$ok)
})

test_that("boxplot can overlay configurable geom_jitter observations", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_boxplot_defaults(project)
  config$x_field <- "group"
  config$y_field <- "value"
  config$color_field <- "group"
  config$box_jitter <- TRUE
  config$box_jitter_color <- "#102A43"
  config$box_jitter_size <- 1.8
  config$box_jitter_alpha <- 0.4
  config$box_jitter_width <- 0.22

  result <- bp_apply_visual_boxplot_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_match(
    code,
    'geom_jitter(width = 0.22, height = 0, color = "#102A43", size = 1.8, alpha = 0.4, shape = 16)',
    fixed = TRUE
  )
  expect_lt(regexpr("geom_boxplot", code, fixed = TRUE)[[1]], regexpr("geom_jitter", code, fixed = TRUE)[[1]])
  expect_true("visual_boxplot_jitter" %in% roles)
  expect_true(isTRUE(result$config$box_jitter))
  expect_true(isTRUE(result$config$box_outlier_restore))
  expect_true(bp_execute_project(result$project, registry)$ok)

  config$box_jitter <- FALSE
  disabled <- bp_apply_visual_boxplot_config(result$project, config, registry)
  disabled_code <- bp_generate_code(disabled$project, registry)
  disabled_roles <- vapply(disabled$project$modules, function(instance) instance$visual_role %||% "", character(1))
  expect_false(grepl("geom_jitter", disabled_code, fixed = TRUE))
  expect_false("visual_boxplot_jitter" %in% disabled_roles)
})

test_that("boxplot clears stale group mappings so boxes, ticks, and jitter stay aligned", {
  project <- bp_basic_scatter_project(registry)
  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  project$modules[[root_index]]$arguments$mapping <- bp_argument(
    "explicit",
    bp_aes_mapping(list(
      x = bp_symbol("PC1"),
      y = bp_symbol("PC2"),
      group = bp_symbol("status")
    )),
    "formal"
  )

  config <- bp_visual_boxplot_defaults(project)
  config$x_field <- "group"
  config$y_field <- "value"
  config$color_field <- "group"
  config$box_jitter <- TRUE
  result <- bp_apply_visual_boxplot_config(project, config, registry)
  code <- bp_generate_code(result$project, registry)
  execution <- bp_execute_project(result$project, registry)

  expect_false(grepl("group = status", code, fixed = TRUE))
  expect_true(execution$ok)

  built <- ggplot2::ggplot_build(execution$plot)
  box_centres <- sort(unique(round(built$data[[1]]$x, 6)))
  jitter_centres <- vapply(split(built$data[[2]]$x, built$data[[2]]$group), mean, numeric(1))
  expect_equal(box_centres, c(1, 2, 3))
  expect_equal(as.numeric(jitter_centres), c(1, 2, 3), tolerance = config$box_jitter_width)
})

test_that("entering visual boxplot mode removes only conflicting group mappings", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_boxplot_defaults(project)
  config$x_field <- "group"
  config$y_field <- "value"
  config$color_field <- "group"
  project <- bp_apply_visual_boxplot_config(project, config, registry)$project
  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  project$modules[[root_index]]$arguments$mapping <- bp_argument(
    "explicit",
    bp_aes_mapping(list(
      x = bp_symbol("group"),
      y = bp_symbol("value"),
      fill = bp_symbol("group"),
      group = bp_symbol("status")
    )),
    "formal"
  )
  box_index <- bp_visual_first_instance(project, "r.ggplot2.geom_boxplot")
  project$modules[[box_index]]$arguments$notch <- bp_argument("explicit", bp_logical(TRUE), "formal")

  repaired <- bp_visual_remove_boxplot_group_mappings(project)
  code <- bp_generate_code(repaired$project, registry)

  expect_true(repaired$changed)
  expect_false(grepl("group = status", code, fixed = TRUE))
  expect_match(code, "notch = TRUE", fixed = TRUE)
})

test_that("boxplot remembers a disabled outlier state while jitter is active", {
  project <- bp_basic_scatter_project(registry)
  config <- bp_visual_boxplot_defaults(project)
  config$x_field <- "group"
  config$y_field <- "value"
  config$box_show_outliers <- FALSE
  config$box_outlier_restore <- FALSE
  config$box_jitter <- TRUE

  result <- bp_apply_visual_boxplot_config(project, config, registry)

  expect_false(result$config$box_show_outliers)
  expect_false(result$config$box_outlier_restore)
  expect_true(result$config$box_jitter)
  expect_match(bp_generate_code(result$project, registry), "outlier.shape = NA_integer_", fixed = TRUE)
})

test_that("switching from boxplot to scatter removes managed boxplot layers", {
  project <- bp_basic_scatter_project(registry)
  box <- bp_visual_boxplot_defaults(project)
  box$x_field <- "group"
  box$y_field <- "value"
  box$color_field <- "group"
  box$palette <- "blue_red"
  box$box_jitter <- TRUE
  project <- bp_apply_visual_boxplot_config(project, box, registry)$project

  scatter <- bp_visual_scatter_defaults(project)
  scatter$x_field <- "PC1"
  scatter$y_field <- "PC2"
  result <- bp_apply_visual_scatter_config(project, scatter, registry)
  code <- bp_generate_code(result$project, registry)
  roles <- vapply(result$project$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_identical(result$project$visual_config$active_chart_type, "scatter")
  expect_false(any(grepl("^visual_boxplot", roles)))
  expect_false(grepl("geom_boxplot|geom_jitter|scale_fill_manual", code))
  expect_true(bp_execute_project(result$project, registry)$ok)
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
