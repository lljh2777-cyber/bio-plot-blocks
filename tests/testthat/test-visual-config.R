test_that("scatter recommendations use scientific column conventions", {
  source <- bp_example_data_source()
  recommendation <- bp_visual_recommend_scatter_fields(source, bp_default_environment()$df)
  expect_identical(recommendation$x_field, "PC1")
  expect_identical(recommendation$y_field, "PC2")
  expect_identical(recommendation$color_field, "status")
  expect_identical(recommendation$label_field, "gene")
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
  expect_true("r.ggplot2.geom_vline" %in% module_ids)
  expect_true("r.ggplot2.geom_hline" %in% module_ids)
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
