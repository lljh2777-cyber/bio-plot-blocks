test_that("system-generated template code round-trips exactly", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  code <- bp_generate_code(project, registry)
  parsed <- bp_parse_code(code, registry)
  expect_identical(bp_generate_code(parsed, registry), code)
  expect_identical(vapply(parsed$modules, `[[`, character(1), "module_id"), vapply(project$modules, `[[`, character(1), "module_id"))
})

test_that("unknown inner calls degrade without loss", {
  code <- "p <- ggplot(df, aes(x = PC1, y = PC2)) + geom_point(size = calculate_size(config))"
  parsed <- bp_parse_code(code, registry)
  point <- parsed$modules[[2]]
  expect_identical(point$arguments$size$state, "raw_expression")
  expect_identical(point$arguments$size$value$source, "calculate_size(config)")
  expect_match(bp_generate_code(parsed, registry), "calculate_size\\(config\\)")
})

test_that("unsupported package calls remain Raw R", {
  parsed <- bp_parse_code("p <- ggplot(df, aes(PC1, PC2)) + ggrepel::geom_text_repel()", registry)
  expect_identical(parsed$modules[[2]]$module_id, "core.raw_r")
  expect_match(parsed$modules[[2]]$source_text, "ggrepel::geom_text_repel")
})
