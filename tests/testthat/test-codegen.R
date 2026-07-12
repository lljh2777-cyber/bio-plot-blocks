test_that("template generates deterministic visible ggplot2 code", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  code <- bp_generate_code(project, registry)
  expect_match(code, "p <- ggplot\\(data = df")
  expect_match(code, "geom_point\\(mapping = aes\\(color = status\\)")
  expect_false(grepl("geom_vline|geom_hline", code))
  expect_match(code, "theme_classic\\(base_size = 12\\)")
  expect_false(grepl("dplyr|ggrepel|ggpubr|patchwork", code))
})

test_that("argument states remain distinct in generated code", {
  project <- bp_basic_scatter_project(registry)
  point <- project$modules[[2]]
  point$arguments$na.rm <- bp_argument("explicit_default", bp_logical(FALSE), "formal")
  point$arguments$color <- bp_argument("explicit_null", bp_null(), "dots_aesthetic")
  point$arguments$show.legend <- bp_argument("explicit_na", bp_na(), "formal")
  project$modules[[2]] <- point
  code <- bp_generate_code(project, registry)
  expect_match(code, "na.rm = FALSE")
  expect_match(code, "color = NULL")
  expect_match(code, "show.legend = NA")
})

test_that("unset defaults are not emitted", {
  project <- bp_basic_scatter_project(registry)
  code <- bp_generate_code(project, registry)
  expect_false(grepl("na.rm", code, fixed = TRUE))
  expect_false(grepl("inherit.aes", code, fixed = TRUE))
})
