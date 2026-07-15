test_that("a fresh workspace starts with an executable scatter plot", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())

  project <- bp_initial_scatter_project(registry)
  module_ids <- vapply(project$modules, `[[`, character(1), "module_id")
  config <- bp_visual_config_from_project(project)
  code <- bp_generate_code(project, registry)

  expect_identical(project$analysis_workflow_mode, "generic")
  expect_identical(project$visual_config$active_chart_type, "scatter")
  expect_identical(config$x_field, "PC1")
  expect_identical(config$y_field, "PC2")
  expect_true("r.ggplot2.ggplot" %in% module_ids)
  expect_true("r.ggplot2.geom_point" %in% module_ids)
  expect_match(code, "geom_point", fixed = TRUE)
  expect_true(bp_execute_project(project, registry)$ok)
})
