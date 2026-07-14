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
  expect_match(html, "创建科研图表")
  expect_match(html, "visual_auto_preview")
  expect_match(html, "visual_data_preview")
  expect_match(html, "visual_data_preview_toggle")
  expect_match(html, "visual_active_data_preview")
  expect_match(html, "前 30 行 · 全部列", fixed = TRUE)
  expect_match(html, "visual_chart_volcano")
  expect_match(html, "visual_chart_boxplot")
  expect_match(html, "visual_chart_pca")
  expect_match(html, "visual_box_border_color")
  expect_match(html, "visual_box_show_outliers")
  expect_match(html, "visual_box_outlier_size")
  expect_match(html, "visual_box_jitter")
  expect_match(html, "visual_box_jitter_color")
  expect_match(html, "visual_box_jitter_size")
  expect_match(html, "visual_box_jitter_alpha")
  expect_match(html, "visual_box_jitter_width")
  expect_match(html, "geom_jitter", fixed = TRUE)
  expect_match(html, "bp-boxplot-overlap-warning", fixed = TRUE)
  expect_match(html, "可能出现重叠或颜色加深", fixed = TRUE)
  expect_match(html, "bp-visual-chart-requirement")
  expect_match(html, "bp-visual-chart-requirement-copy")
  expect_match(html, "至少 2 个数值型字段（X、Y）", fixed = TRUE)
  expect_match(html, "logFC + P 值或 FDR", fixed = TRUE)
  expect_match(html, "1 个分组字段 + 1 个数值字段", fixed = TRUE)
  expect_match(html, "表达矩阵；可选样本分组信息", fixed = TRUE)
  expect_match(html, "降维图", fixed = TRUE)
  expect_match(html, "基因表达散点图", fixed = TRUE)
  expect_match(html, "表达箱线图", fixed = TRUE)
  expect_match(html, 'data-generic-visible="false"', fixed = TRUE)
  expect_match(html, "只切换图表目录与分析语义", fixed = TRUE)
  expect_match(html, "visual_pca_expression_source")
  expect_match(html, "bp-visual-source-card", fixed = TRUE)
  expect_match(html, 'class="bp-pca-source-card bp-visual-source-card"', fixed = TRUE)
  expect_false(grepl('id="visual_data_source"', html, fixed = TRUE))
  expect_lt(
    regexpr('id="visual-section-chart"', html, fixed = TRUE)[[1]],
    regexpr('id="visual-section-source"', html, fixed = TRUE)[[1]]
  )
  expect_match(html, 'class="bp-visual-step is-active"[^>]*data-visual-section="visual-section-chart"', perl = TRUE)
  expect_match(html, "visual_workflow_mode")
  expect_match(html, "visual_data_semantics")
  expect_match(html, "visual_chart_compatibility")
  expect_match(html, "visual_pca_recipe_panel")
  expect_match(html, "visual_pca_metadata_source")
  expect_match(html, "visual_pca_orientation")
  expect_match(html, "visual_pca_x_component")
  expect_match(html, "visual_pca_feature_count")
  expect_match(html, "visual_pca_show_ellipse")
  expect_match(html, "download_pca_scores")
  expect_match(html, "visual_pca_normalized_export")
  expect_match(html, "analysis_context_view")
  expect_match(html, "analysis_code_view")
  expect_match(html, "visual_vlines")
  expect_match(html, "visual_hlines")
  expect_match(html, "visual_reference_color")
  expect_match(html, "bp-visual-reference-card")
  expect_match(html, "倍数变化阈值")
  expect_match(html, "visual_auto_status")
})

test_that("visual and advanced modes share a visibility-aware shell", {
  js <- paste(readLines(file.path(root, "app", "www", "app.js"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(root, "app", "www", "app.css"), warn = FALSE), collapse = "\n")
  expect_match(js, "setInterfaceMode", fixed = TRUE)
  expect_match(js, 'trigger("shown")', fixed = TRUE)
  expect_match(js, "bioplotblocks.interface-mode.v1", fixed = TRUE)
  expect_match(css, "html.bp-interface-visual .bp-advanced-surface", fixed = TRUE)
  expect_match(css, ".bp-visual-preview-canvas", fixed = TRUE)
  expect_match(js, "bp_visual_chart_type", fixed = TRUE)
  expect_match(js, "setVisualChartType", fixed = TRUE)
  expect_match(js, "bp_visual_workflow_mode", fixed = TRUE)
  expect_match(js, "setVisualWorkflowMode", fixed = TRUE)
  expect_match(js, 'input[name="visual_workflow_mode"]', fixed = TRUE)
  expect_match(js, "GENE EXPRESSION SCATTER", fixed = TRUE)
  expect_match(js, "DIMENSION REDUCTION", fixed = TRUE)
  expect_match(js, "scrollVisualSection", fixed = TRUE)
  expect_match(js, "restoreVisualPageOrigin", fixed = TRUE)
  expect_match(js, "if (section) scrollVisualSection(section);", fixed = TRUE)
  expect_match(js, '["scatter", "volcano", "boxplot", "pca"]', fixed = TRUE)
  expect_match(js, "#visual_point_color, #visual_reference_color", fixed = TRUE)
  expect_match(js, "#visual_box_jitter_color", fixed = TRUE)
  expect_match(css, ".bp-volcano-only[hidden]", fixed = TRUE)
  expect_match(css, ".bp-boxplot-only[hidden]", fixed = TRUE)
  expect_match(css, ".bp-pca-only[hidden]", fixed = TRUE)
  expect_match(css, ".bp-pca-source-card", fixed = TRUE)
  expect_match(css, ".bp-visual-source-card", fixed = TRUE)
  expect_match(css, "@media (min-width: 761px)", fixed = TRUE)
  expect_match(css, "height: 100dvh", fixed = TRUE)
  expect_match(css, "overscroll-behavior: none", fixed = TRUE)
  expect_match(css, "overscroll-behavior: contain", fixed = TRUE)
  expect_match(css, ".bp-analysis-code", fixed = TRUE)
  expect_match(css, ".bp-data-passport", fixed = TRUE)
  expect_match(css, ".bp-chart-compatibility", fixed = TRUE)
  expect_match(css, ".bp-pca-recipe-card", fixed = TRUE)
  expect_match(css, ".bp-analysis-context", fixed = TRUE)
  expect_match(css, ".bp-boxplot-jitter-card", fixed = TRUE)
  expect_match(css, ".bp-boxplot-overlap-warning", fixed = TRUE)
  expect_match(css, ".bp-visual-reference-card", fixed = TRUE)
  expect_match(css, ".bp-visual-chart-requirement", fixed = TRUE)
  expect_match(css, 'body[data-visual-workflow-mode="generic"]', fixed = TRUE)
  expect_match(css, 'body[data-visual-workflow-mode="rna_seq"]', fixed = TRUE)
  expect_match(css, ".bp-visual-chart-card[hidden]", fixed = TRUE)
  expect_match(
    css,
    paste0(
      'body[data-visual-workflow-mode="rna_seq"] .bp-visual-chart-card[data-chart-type="pca"] {',
      "\n  order: 1;\n}"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      'body[data-visual-workflow-mode="rna_seq"] .bp-visual-chart-card[data-chart-type="boxplot"] {',
      "\n  order: 2;\n}"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      'body[data-visual-workflow-mode="rna_seq"] .bp-visual-chart-card[data-chart-type="scatter"] {',
      "\n  order: 3;\n}"
    ),
    fixed = TRUE
  )
  expect_match(
    css,
    paste0(
      'body[data-visual-workflow-mode="rna_seq"] .bp-visual-chart-card[data-chart-type="volcano"] {',
      "\n  order: 4;\n}"
    ),
    fixed = TRUE
  )
  expect_match(css, ".bp-visual-data-preview .bp-data-preview-scroll", fixed = TRUE)
  expect_match(css, ".bp-visual-data-preview-checkbox:checked ~ .bp-visual-data-preview-content", fixed = TRUE)
  expect_match(css, "touch-action: pan-x pan-y", fixed = TRUE)
  expect_match(css, ".bp-r-preview-picker", fixed = TRUE)
  expect_match(css, ".bp-r-object-preview .bp-data-preview-scroll", fixed = TRUE)
  expect_match(css, "overflow-x: scroll", fixed = TRUE)
  expect_match(css, "scrollbar-gutter: stable", fixed = TRUE)
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
