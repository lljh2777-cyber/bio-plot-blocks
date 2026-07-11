test_that("real ggplot2 execution returns a plot", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  result <- bp_execute_project(project, registry)
  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
  expect_identical(result$versions$ggplot2, as.character(packageVersion("ggplot2")))
})

test_that("project JSON restores typed module state", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  path <- tempfile(fileext = ".json")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  bp_save_project(project, path)
  restored <- bp_load_project(path)
  expect_identical(restored$runtime, "R")
  expect_identical(restored$package_scope, "ggplot2")
  expect_length(restored$modules, length(project$modules))
  expect_identical(restored$modules[[2]]$arguments$alpha$state, "explicit")
})

test_that("scope scan permits only visible ggplot2 add-on calls", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  scope <- bp_scope_scan(bp_generate_code(project, registry, include_setup = TRUE))
  expect_true(scope$ok)
  expect_setequal(scope$packages, "ggplot2")
  prohibited <- bp_scope_scan("ggrepel::geom_text_repel()")
  expect_false(prohibited$ok)
  expect_identical(prohibited$prohibited, "ggrepel")
})
