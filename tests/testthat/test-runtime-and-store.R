test_that("real ggplot2 execution returns a plot", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  result <- bp_execute_project(project, registry)
  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
  expect_identical(result$versions$ggplot2, as.character(packageVersion("ggplot2")))
})

test_that("the blank advanced project contains only ggplot", {
  project <- bp_ggplot_only_project(registry)
  code <- bp_generate_code(project, registry)

  expect_length(project$modules, 1L)
  expect_identical(project$modules[[1]]$module_id, "r.ggplot2.ggplot")
  expect_identical(project$mapping_config$plot_id, project$modules[[1]]$instance_id)
  expect_match(code, "ggplot(data = df, mapping = aes(x = PC1, y = PC2))", fixed = TRUE)
  expect_false(grepl(" + ", code, fixed = TRUE))
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

test_that("browser persistence envelope preserves the current project", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  project$name <- "Persisted browser project"
  selected <- project$modules[[length(project$modules)]]$instance_id
  payload <- jsonlite::toJSON(
    list(format_version = 1L, project = project, selected = selected),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  )
  restored <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  restored$project <- bp_migrate_project(restored$project)
  expect_silent(bp_validate_project(restored$project, registry))
  expect_identical(restored$selected, selected)
  expect_identical(bp_generate_code(restored$project, registry), bp_generate_code(project, registry))
})

test_that("project migration removes legacy automatic volcano lines only", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  automatic <- bp_instantiate_module("r.ggplot2.geom_vline", registry)
  automatic$visual_managed <- TRUE
  automatic$visual_role <- "volcano_fc_threshold"
  custom <- bp_instantiate_module("r.ggplot2.geom_hline", registry)
  custom$visual_managed <- TRUE
  custom$visual_role <- "visual_horizontal_reference_lines"
  project$modules <- c(project$modules, list(automatic, custom))

  migrated <- bp_migrate_project(project)
  roles <- vapply(migrated$modules, function(instance) instance$visual_role %||% "", character(1))

  expect_false("volcano_fc_threshold" %in% roles)
  expect_true("visual_horizontal_reference_lines" %in% roles)
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
