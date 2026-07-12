test_that("workspace UI renders the full primary surface", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "help-ui.R"), local = environment())
  source(file.path(root, "app", "modules", "workspace-ui.R"), local = environment())
  ui <- bp_workspace_ui(root)
  html <- htmltools::renderTags(ui)$html
  expect_match(html, "BioPlotBlocks")
  expect_match(html, "Layer stack")
  expect_match(html, "bp-module-picker")
  expect_match(html, "Add module")
  expect_match(html, "Layer builder")
  expect_false(grepl("bp-library-panel", html, fixed = TRUE))
  expect_match(html, "Generated R")
  expect_match(html, "Run preview")
  expect_match(html, "Import Data")
  expect_match(html, "Data Sources")
  expect_match(html, "bp-preview-view-switch")
  expect_match(html, "preview_data_view")
  expect_match(html, "open-help-button")
  expect_match(html, "使用手册")
  expect_match(html, "User manual")
  expect_match(html, "bp-interface-mode-switch")
  expect_match(html, "bp-visual-workspace")
  expect_match(html, "创建散点图")
  expect_match(html, "visual_auto_preview")
})

test_that("visual and advanced modes share a visibility-aware shell", {
  js <- paste(readLines(file.path(root, "app", "www", "app.js"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")
  expect_match(js, "setInterfaceMode", fixed = TRUE)
  expect_match(js, 'trigger("shown")', fixed = TRUE)
  expect_match(js, "bioplotblocks.interface-mode.v1", fixed = TRUE)
  expect_match(css, "html.bp-interface-visual .bp-advanced-surface", fixed = TRUE)
  expect_match(css, ".bp-visual-preview-canvas", fixed = TRUE)
})

test_that("topbar has responsive overflow safeguards in both modes", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "help-ui.R"), local = environment())
  source(file.path(root, "app", "modules", "workspace-ui.R"), local = environment())
  html <- htmltools::renderTags(bp_workspace_ui(root))$html
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")

  expect_match(css, "@media (max-width: 1440px)", fixed = TRUE)
  expect_match(css, "overscroll-behavior-inline: contain", fixed = TRUE)
  expect_match(css, ".bp-command-bar .shiny-download-link > .fa-download", fixed = TRUE)
  expect_match(html, 'aria-label="Save project"', fixed = TRUE)
  expect_match(html, 'aria-label="Export R script"', fixed = TRUE)
})

test_that("module picker supports hover, click, search, and keyboard disclosure", {
  js <- paste(readLines(file.path(root, "app", "www", "app.js"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")
  expect_match(js, "openModulePicker", fixed = TRUE)
  expect_match(js, 'document.addEventListener("mouseover"', fixed = TRUE)
  expect_match(js, 'event.key === "ArrowDown"', fixed = TRUE)
  expect_match(js, "filterModulePicker", fixed = TRUE)
  expect_match(css, ".bp-picker-menu[hidden]", fixed = TRUE)
})

test_that("module picker remains reachable when the builder is narrow", {
  js <- paste(readLines(file.path(root, "app", "www", "app.js"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")
  expect_match(css, "#module_picker", fixed = TRUE)
  expect_match(css, "overflow-x: auto", fixed = TRUE)
  expect_match(css, "width: max-content", fixed = TRUE)
  expect_match(js, "finishPickerScroll", fixed = TRUE)
  expect_match(js, 'closest(event.target, "#module_picker")', fixed = TRUE)
  expect_match(js, "positionModulePickerMenu", fixed = TRUE)
})

test_that("upper workspace starts with equal builder and inspector widths", {
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")
  equal_split <- "--bp-inspector-width: calc(50% - 3.5px)"
  expect_gte(length(gregexpr(equal_split, css, fixed = TRUE)[[1]]), 2L)
})

test_that("mapping dropdown stays above Bootstrap modals and backdrops", {
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")
  expect_match(css, ".selectize-dropdown.bp-mapping-dropdown.form-control", fixed = TRUE)
  expect_match(css, "z-index: 1080 !important", fixed = TRUE)
})
