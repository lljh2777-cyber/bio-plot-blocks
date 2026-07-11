test_that("registry is declarative, R-only, and ggplot2-scoped", {
  expect_gte(length(registry), 10L)
  expect_true(all(vapply(registry, function(spec) identical(spec$runtime, "R"), logical(1))))
  expect_true(all(vapply(registry, function(spec) spec$package %in% c("ggplot2", "BioPlotBlocks.core"), logical(1))))
  expect_true(all(vapply(registry, function(spec) {
    all(vapply(spec$parameters, function(parameter) isTRUE(parameter$raw_expression_allowed), logical(1)))
  }, logical(1))))
})

test_that("ordinary module instances come from ModuleSpec", {
  instance <- bp_instantiate_module("r.ggplot2.geom_point", registry, "point-1")
  expect_identical(instance$module_id, "r.ggplot2.geom_point")
  expect_true(all(c("mapping", "data", "size", "alpha", "shape") %in% names(instance$arguments)))
  expect_true(all(vapply(instance$arguments, function(argument) identical(argument$state, "unset"), logical(1))))
})
