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

test_that("geom_violin parses as a registered module and round-trips", {
  code <- paste0(
    "p <- ggplot(data = df, mapping = aes(x = group, y = value, fill = group)) + ",
    "geom_violin(color = \"#334155\", width = 0.8, alpha = 0.82, trim = TRUE, ",
    "scale = \"area\", quantiles = 0.5, quantile.linetype = 1, quantile.linewidth = 0.5)"
  )
  parsed <- bp_parse_code(code, registry)

  expect_identical(parsed$modules[[2]]$module_id, "r.ggplot2.geom_violin")
  generated <- bp_generate_code(parsed, registry)
  reparsed <- bp_parse_code(generated, registry)
  expect_identical(reparsed$modules[[2]]$module_id, "r.ggplot2.geom_violin")
  expect_identical(bp_generate_code(reparsed, registry), generated)
})

test_that("unsupported package calls remain Raw R", {
  parsed <- bp_parse_code("p <- ggplot(df, aes(PC1, PC2)) + ggrepel::geom_text_repel()", registry)
  expect_identical(parsed$modules[[2]]$module_id, "core.raw_r")
  expect_match(parsed$modules[[2]]$source_text, "ggrepel::geom_text_repel")
})

test_that("text controls round-trip without accumulating R string escapes", {
  input <- "bar plot"
  for (i in seq_len(4)) {
    value <- bp_value_from_text(input, "text")
    expect_identical(bp_value_to_source(value), '"bar plot"')
    input <- bp_value_to_input_text(value, "text")
  }
  expect_identical(input, "bar plot")
})

test_that("text controls preserve intentional whitespace", {
  value <- bp_value_from_text("  bar plot  ", "text")
  expect_identical(value$value, "  bar plot  ")
  expect_identical(bp_value_to_input_text(value, "text"), "  bar plot  ")
  expect_identical(bp_value_to_source(value), '"  bar plot  "')
})
