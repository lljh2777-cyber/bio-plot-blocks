bp_action_button <- function(id, label, icon, primary = FALSE, class = NULL, title = NULL) {
  shiny::actionButton(
    id,
    label = htmltools::tagList(bp_icon(icon, 17), htmltools::tags$span(class = "bp-command-label", label)),
    class = paste("bp-command-button", if (primary) "bp-command-primary", class),
    title = title,
    `aria-label` = label
  )
}

bp_resize_handle <- function(orientation, target, label, class = NULL) {
  htmltools::tags$div(
    class = paste("bp-resize-handle", paste0("bp-resize-", orientation), class),
    role = "separator",
    tabindex = "0",
    `aria-orientation` = orientation,
    `aria-label` = label,
    `data-resize-target` = target,
    title = "Drag to resize; double-click to reset",
    htmltools::tags$span(
      class = "bp-resize-grip",
      `aria-hidden` = "true",
      htmltools::tags$span(),
      htmltools::tags$span(),
      htmltools::tags$span()
    )
  )
}

bp_visual_step <- function(number, label, icon, section, active = FALSE) {
  htmltools::tags$button(
    type = "button",
    class = paste("bp-visual-step", if (active) "is-active"),
    `data-visual-section` = section,
    `aria-label` = paste0(number, ". ", label),
    htmltools::tags$span(class = "bp-visual-step-number", sprintf("%02d", number)),
    bp_icon(icon, 20),
    htmltools::tags$span(label)
  )
}

bp_visual_section_header <- function(number, title, description = NULL, action = NULL) {
  htmltools::tags$div(
    class = "bp-visual-section-header",
    htmltools::tags$div(
      class = "bp-visual-section-title",
      htmltools::tags$span(number),
      htmltools::tags$div(
        htmltools::tags$h2(title),
        if (!is.null(description)) htmltools::tags$p(description)
      )
    ),
    action
  )
}

bp_visual_chart_card <- function(id, chart_type, title, subtitle, icon, active = FALSE, disabled = FALSE) {
  shiny::actionButton(
    inputId = id,
    label = htmltools::tagList(
      bp_icon(icon, 22),
      htmltools::tags$span(
        htmltools::tags$strong(title),
        htmltools::tags$small(subtitle)
      ),
      if (disabled) htmltools::tags$em("后续阶段") else bp_icon("check", 16)
    ),
    class = paste("bp-visual-chart-card", if (active) "is-active", if (disabled) "is-disabled"),
    disabled = disabled,
    `data-chart-type` = chart_type,
    `aria-pressed` = if (active) "true" else "false",
    `aria-label` = paste(title, subtitle)
  )
}

bp_workspace_ui <- function(root) {
  shiny::fluidPage(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title("BioPlotBlocks"),
      htmltools::tags$script(htmltools::HTML(
        "(function(){try{var m=localStorage.getItem('bioplotblocks.interface-mode.v1');document.documentElement.classList.add(m==='advanced'?'bp-interface-advanced':'bp-interface-visual');}catch(e){document.documentElement.classList.add('bp-interface-visual');}})();"
      )),
      shiny::includeCSS(file.path(root, "app", "www", "app.css")),
      shiny::includeScript(file.path(root, "app", "www", "app.js"))
    ),
    htmltools::tags$div(
      class = "bp-app-shell",
      htmltools::tags$header(
        class = "bp-topbar",
        htmltools::tags$div(
          class = "bp-brand",
          bp_brand_mark(36),
          htmltools::tags$span(class = "bp-brand-name", "BioPlotBlocks")
        ),
        htmltools::tags$div(class = "bp-topbar-divider"),
        htmltools::tags$div(
          class = "bp-project-name-wrap",
          shiny::textInput("project_name", label = NULL, value = "Volcano analysis", placeholder = "Project name")
        ),
        htmltools::tags$div(
          class = "bp-interface-mode-switch",
          role = "group",
          `aria-label` = "Interface mode / 界面模式",
          htmltools::tags$button(type = "button", class = "bp-mode-button is-active", `data-interface-mode` = "visual", `aria-pressed` = "true", bp_icon("plot", 15), htmltools::tags$span("可视化模式")),
          htmltools::tags$button(type = "button", class = "bp-mode-button", `data-interface-mode` = "advanced", `aria-pressed` = "false", bp_icon("code", 15), htmltools::tags$span("R / 高级模式"))
        ),
        htmltools::tags$div(
          class = "bp-environment-lock",
          htmltools::tags$span(class = "bp-live-dot", `aria-hidden` = "true"),
          htmltools::tags$span("R 4.5.1 · ggplot2 4.0.1")
        ),
        htmltools::tags$div(class = "bp-active-data-source", shiny::uiOutput("active_data_source_badge")),
        htmltools::tags$nav(
          class = "bp-command-bar",
          `aria-label` = "Project commands",
          bp_action_button("visual_undo", "Undo", "undo", class = "bp-visual-command", title = "撤销 (Ctrl+Z)"),
          bp_action_button("visual_redo", "Redo", "redo", class = "bp-visual-command", title = "重做 (Ctrl+Y)"),
          bp_action_button("new_project", "New", "plus", class = "bp-advanced-command"),
          bp_action_button("import_r", "Import R", "import", class = "bp-advanced-command"),
          bp_action_button("import_data", "Import Data", "import", title = "Import CSV, TSV, TXT, RDS, RData, or rda"),
          bp_action_button("manage_data_sources", "Data Sources", "open", title = "Manage registered data sources"),
          bp_action_button("run_preview", "Run preview", "play", primary = TRUE, title = "Run preview (Ctrl+Enter)"),
          shiny::downloadButton(
            "download_project",
            label = htmltools::tagList(bp_icon("save", 17), htmltools::tags$span(class = "bp-command-label", "Save")),
            class = "bp-command-button",
            title = "Save project",
            `aria-label` = "Save project"
          ),
          shiny::downloadButton(
            "download_r",
            label = htmltools::tagList(bp_icon("export", 17), htmltools::tags$span(class = "bp-command-label", "Export")),
            class = "bp-command-button",
            title = "Export R script",
            `aria-label` = "Export R script"
          ),
          htmltools::tags$button(
            id = "open-help-button",
            type = "button",
            class = "bp-command-button bp-help-button",
            title = "Help / 使用手册",
            `aria-label` = "Open Help / 打开使用手册",
            `aria-haspopup` = "dialog",
            `aria-controls` = "bp-help-view",
            `aria-expanded` = "false",
            htmltools::tagList(bp_icon("help", 17), htmltools::tags$span(class = "bp-command-label", "Help"))
          ),
          htmltools::tags$button(
            id = "open-project-button",
            type = "button",
            class = "bp-icon-button bp-open-project",
            title = "Open a saved project",
            `aria-label` = "Open a saved project",
            bp_icon("open", 18)
          )
        )
      ),
      htmltools::tags$main(
        class = "bp-visual-workspace bp-visual-surface",
        htmltools::tags$aside(
          class = "bp-visual-step-rail",
          `aria-label` = "可视化绘图步骤",
          bp_visual_step(1, "数据源", "open", "visual-section-source", active = TRUE),
          bp_visual_step(2, "图表类型", "plot", "visual-section-chart"),
          bp_visual_step(3, "数据字段", "mapping", "visual-section-fields"),
          bp_visual_step(4, "样式", "theme", "visual-section-style"),
          bp_visual_step(5, "标题与坐标轴", "label", "visual-section-labels"),
          bp_visual_step(6, "导出", "export", "visual-section-export")
        ),
        htmltools::tags$section(
          class = "bp-visual-builder-panel",
          htmltools::tags$div(
            class = "bp-visual-builder-heading",
            htmltools::tags$div(
              htmltools::tags$span(class = "bp-visual-eyebrow", "SCIENTIFIC PLOT BUILDER · 科研绘图向导"),
              htmltools::tags$h1("创建科研图表"),
              htmltools::tags$p("按步骤选择数据和样式；所有改动会同步到同一个 R / ggplot2 项目。")
            ),
            shiny::actionButton("visual_new_scatter", "新建图表", icon = shiny::icon("plus"), class = "bp-command-button")
          ),
          htmltools::tags$div(
            class = "bp-visual-builder-scroll",
            htmltools::tags$section(
              id = "visual-section-source",
              class = "bp-visual-config-section",
              bp_visual_section_header("01", "选择数据源", "选择已注册且可用的数据；数据预览和字段候选会自动同步。"),
              shiny::selectInput("visual_data_source", label = "数据表", choices = c("正在读取…" = ""), selectize = FALSE, width = "100%"),
              shiny::uiOutput("visual_data_profile")
            ),
            htmltools::tags$section(
              id = "visual-section-chart",
              class = "bp-visual-config-section",
              bp_visual_section_header("02", "选择图表类型", "当前支持散点图和火山图；其他类型将在后续阶段接入同一配置层。"),
              htmltools::tags$div(
                class = "bp-visual-chart-grid",
                bp_visual_chart_card("visual_chart_scatter", "scatter", "散点图", "比较两个连续变量", "point", active = TRUE),
                bp_visual_chart_card("visual_chart_volcano", "volcano", "火山图", "差异表达结果", "plot"),
                bp_visual_chart_card("visual_chart_boxplot", "boxplot", "箱线图", "比较组间分布", "boxplot", disabled = TRUE),
                bp_visual_chart_card("visual_chart_pca", "pca", "PCA 图", "样本降维概览", "mapping", disabled = TRUE)
              )
            ),
            htmltools::tags$section(
              id = "visual-section-fields",
              class = "bp-visual-config-section",
              bp_visual_section_header(
                "03", "映射数据字段", "散点图需要 X/Y；火山图需要倍数变化和显著性字段。",
                shiny::actionButton("visual_recommend_fields", "智能推荐", icon = shiny::icon("wand-magic-sparkles"), class = "bp-link-button")
              ),
              shiny::uiOutput("visual_field_recommendation"),
              htmltools::tags$div(
                class = "bp-visual-field-grid",
                htmltools::tags$div(class = "bp-visual-field-control", `data-visual-field` = "x", shiny::selectizeInput("visual_x", "X 轴字段 *", choices = NULL, options = list(placeholder = "选择数值列"), width = "100%")),
                htmltools::tags$div(class = "bp-visual-field-control", `data-visual-field` = "y", shiny::selectizeInput("visual_y", "Y 轴字段 *", choices = NULL, options = list(placeholder = "选择数值列"), width = "100%")),
                htmltools::tags$div(class = "bp-visual-field-control", `data-visual-field` = "color", shiny::selectizeInput("visual_color", "颜色/状态分组", choices = NULL, options = list(placeholder = "不映射颜色"), width = "100%")),
                shiny::selectizeInput("visual_size", "点大小映射", choices = NULL, options = list(placeholder = "固定大小"), width = "100%"),
                shiny::selectizeInput("visual_label", "标签字段", choices = NULL, options = list(placeholder = "不显示标签"), width = "100%")
              ),
              htmltools::tags$div(
                class = "bp-visual-transform-grid",
                shiny::selectInput("visual_x_scale", "X 值转换", choices = c("线性" = "linear", "log10" = "log10", "-log10" = "neg_log10"), width = "100%"),
                shiny::selectInput("visual_y_scale", "Y 值转换", choices = c("线性" = "linear", "log10" = "log10", "-log10" = "neg_log10"), width = "100%")
              ),
              htmltools::tags$div(
                class = "bp-visual-transform-grid bp-volcano-only",
                hidden = "hidden",
                shiny::numericInput("visual_fc_cutoff", "倍数变化阈值 |log2FC|", value = 1, min = 0, max = 1000, step = 0.1, width = "100%"),
                shiny::numericInput("visual_p_cutoff", "显著性阈值", value = 0.05, min = 0.0000001, max = 1, step = 0.01, width = "100%"),
                htmltools::tags$div(
                  class = "bp-volcano-auto-status",
                  shiny::checkboxInput("visual_auto_status", "未选择状态列时，自动创建 Up / NS / Down 分组", value = TRUE)
                )
              )
            ),
            htmltools::tags$section(
              id = "visual-section-style",
              class = "bp-visual-config-section",
              bp_visual_section_header("04", "设置常用样式", "仅展示高频选项；原有高级参数不会被删除。"),
              htmltools::tags$div(
                class = "bp-visual-control-grid",
                htmltools::tags$div(class = "bp-visual-color-control", htmltools::tags$span(class = "bp-visual-color-swatch"), shiny::textInput("visual_point_color", "点颜色", value = "#2C7FB8", width = "100%")),
                shiny::numericInput("visual_point_size", "固定点大小", value = 2, min = 0.1, max = 20, step = 0.1, width = "100%"),
                shiny::numericInput("visual_alpha", "透明度", value = 0.72, min = 0, max = 1, step = 0.05, width = "100%"),
                shiny::selectInput("visual_shape", "点形状", choices = c("实心圆" = "16", "空心圆" = "1", "实心方形" = "15", "实心三角" = "17", "菱形" = "18"), width = "100%"),
                shiny::selectInput("visual_palette", "分组调色板", choices = c("沿用项目 / 默认" = "default", "蓝–灰–红" = "blue_red", "Viridis 风格" = "viridis_like"), width = "100%"),
                htmltools::tags$div(class = "bp-scatter-only", shiny::selectInput("visual_trend", "趋势线", choices = c("无" = "none", "线性拟合" = "linear", "平滑拟合" = "smooth"), width = "100%")),
                shiny::selectInput("visual_theme", "图表主题", choices = c("经典" = "classic", "简洁" = "minimal", "黑白" = "bw"), width = "100%"),
                shiny::numericInput("visual_base_size", "基础字号", value = 12, min = 6, max = 40, step = 1, width = "100%")
              ),
              htmltools::tags$div(
                class = "bp-visual-reference-card",
                htmltools::tags$div(
                  class = "bp-visual-reference-heading",
                  htmltools::tags$strong("参考虚线（可选）"),
                  htmltools::tags$span("可使用逗号、空格或分号分隔多个数值，例如 -1, 0, 1。")
                ),
                htmltools::tags$div(
                  class = "bp-visual-reference-grid",
                  shiny::textInput("visual_vlines", "纵向虚线位置", value = "", placeholder = "例如 -1, 1", width = "100%"),
                  shiny::textInput("visual_hlines", "横向虚线位置", value = "", placeholder = "例如 1.3, 2", width = "100%"),
                  htmltools::tags$div(
                    class = "bp-visual-color-control",
                    htmltools::tags$span(class = "bp-visual-color-swatch"),
                    shiny::textInput("visual_reference_color", "虚线颜色", value = "#6B7280", width = "100%")
                  ),
                  shiny::numericInput("visual_reference_width", "虚线宽度", value = 0.6, min = 0.1, max = 10, step = 0.1, width = "100%")
                )
              ),
              shiny::uiOutput("visual_advanced_state")
            ),
            htmltools::tags$section(
              id = "visual-section-labels",
              class = "bp-visual-config-section",
              bp_visual_section_header("05", "标题与坐标轴", "留空时沿用 ggplot2 默认标签。"),
              htmltools::tags$div(
                class = "bp-visual-label-grid",
                shiny::textInput("visual_title", "图标题", value = "", width = "100%"),
                shiny::textInput("visual_x_label", "X 轴标题", value = "", width = "100%"),
                shiny::textInput("visual_y_label", "Y 轴标题", value = "", width = "100%"),
                shiny::textInput("visual_legend_title", "图例标题", value = "", width = "100%")
              )
            ),
            htmltools::tags$section(
              id = "visual-section-export",
              class = "bp-visual-config-section bp-visual-export-section",
              bp_visual_section_header("06", "保存与导出", "顶部可保存项目或导出可复现的 R 脚本。"),
              htmltools::tags$p("可视化模式和 R / 高级模式共享同一项目；切换模式不会丢失设置。")
            )
          )
        ),
        htmltools::tags$section(
          class = "bp-visual-preview-panel",
          htmltools::tags$div(
            class = "bp-visual-preview-heading",
            htmltools::tags$div(htmltools::tags$span(class = "bp-visual-eyebrow", "LIVE PREVIEW"), htmltools::tags$h2("实时预览")),
            shiny::uiOutput("visual_preview_status")
          ),
          htmltools::tags$div(
            class = "bp-visual-preview-canvas",
            shiny::uiOutput("visual_preview_image"),
            shiny::uiOutput("visual_preview_overlay")
          ),
          shiny::uiOutput("visual_validation")
        )
      ),
      htmltools::tags$main(
        class = "bp-workspace bp-advanced-surface",
        htmltools::tags$section(
          class = "bp-panel bp-stack-panel bp-builder-panel",
          `aria-label` = "Layer builder",
          htmltools::tags$div(
            class = "bp-panel-titlebar",
            htmltools::tags$h2("Layer stack"),
            htmltools::tags$div(
              class = "bp-panel-actions",
              bp_action_button("undo", "Undo", "undo", title = "Undo (Ctrl+Z)"),
              bp_action_button("redo", "Redo", "redo", title = "Redo (Ctrl+Y)")
            )
          ),
          htmltools::tags$nav(
            class = "bp-module-picker",
            `aria-label` = "Add modules",
            htmltools::tags$div(
              class = "bp-module-picker-label",
              bp_icon("plus", 16),
              htmltools::tags$span("Add module")
            ),
            shiny::uiOutput("module_picker")
          ),
          shiny::uiOutput("assignment_editor"),
          shiny::uiOutput("layer_stack")
        ),
        bp_resize_handle("vertical", "inspector", "Resize layer stack and parameter inspector", class = "bp-advanced-surface"),
        htmltools::tags$aside(
          class = "bp-panel bp-inspector-panel",
          `aria-label` = "Parameter inspector",
          shiny::uiOutput("parameter_inspector")
        )
      ),
      bp_resize_handle("horizontal", "workspace", "Resize upper and lower workspaces", class = "bp-advanced-surface"),
      htmltools::tags$section(
        class = "bp-lower-workspace bp-advanced-surface",
        htmltools::tags$article(
          class = "bp-panel bp-preview-panel",
          htmltools::tags$div(
            class = "bp-panel-titlebar bp-lower-titlebar",
            htmltools::tags$h2("Preview"),
            htmltools::tags$div(
              class = "bp-panel-actions",
              htmltools::tags$div(
                class = "bp-preview-view-switch",
                role = "tablist",
                `aria-label` = "Preview content",
                htmltools::tags$button(
                  type = "button", class = "bp-preview-view-button is-active", role = "tab",
                  `aria-selected` = "true", `aria-controls` = "preview_plot_view",
                  `data-preview-view` = "plot", "Plot"
                ),
                htmltools::tags$button(
                  type = "button", class = "bp-preview-view-button", role = "tab",
                  `aria-selected` = "false", `aria-controls` = "preview_data_view", tabindex = "-1",
                  `data-preview-view` = "data", "Data"
                )
              ),
              shiny::actionButton(
                "cancel_preview",
                label = htmltools::tagList(bp_icon("close", 15), htmltools::tags$span("Cancel")),
                class = "bp-command-button bp-cancel-preview bp-plot-preview-control"
              ),
              htmltools::tags$span(class = "bp-preview-dimensions bp-plot-preview-control", "920 × 540 · 120 dpi")
            )
          ),
          htmltools::tags$div(
            id = "preview_plot_view",
            class = "bp-preview-canvas bp-preview-view",
            role = "tabpanel",
            shiny::uiOutput("preview_image"),
            shiny::uiOutput("preview_overlay")
          ),
          htmltools::tags$div(
            id = "preview_data_view",
            class = "bp-workspace-data-preview bp-preview-view",
            role = "tabpanel",
            hidden = "hidden",
            shiny::uiOutput("active_data_preview")
          )
        ),
        bp_resize_handle("vertical", "preview", "Resize preview and generated code"),
        htmltools::tags$article(
          class = "bp-panel bp-code-panel",
          htmltools::tags$div(
            class = "bp-panel-titlebar bp-lower-titlebar",
            htmltools::tags$div(
              class = "bp-code-title",
              htmltools::tags$h2("Generated R"),
              shiny::uiOutput("code_line_count")
            ),
            htmltools::tags$div(
              class = "bp-panel-actions",
              htmltools::tags$button(
                id = "copy-generated-code",
                type = "button",
                class = "bp-command-button",
                htmltools::tagList(bp_icon("copy", 16), htmltools::tags$span("Copy"))
              ),
              shiny::downloadButton(
                "download_r_secondary",
                label = htmltools::tagList(bp_icon("download", 16), htmltools::tags$span("Download .R")),
                class = "bp-command-button"
              )
            )
          ),
          shiny::uiOutput("code_view"),
          shiny::uiOutput("generated_code_transport"),
          shiny::uiOutput("project_state_transport")
        )
      ),
      htmltools::tags$section(
        class = "bp-visual-actions bp-visual-surface",
        htmltools::tags$div(class = "bp-visual-action-status", shiny::uiOutput("visual_action_status")),
        shiny::checkboxInput("visual_auto_preview", "自动预览", value = TRUE),
        shiny::actionButton("visual_run_preview", "生成并预览", icon = shiny::icon("play"), class = "bp-command-button bp-command-primary")
      ),
      htmltools::tags$footer(
        class = "bp-statusbar",
        shiny::uiOutput("status_bar")
      )
    ),
    bp_help_manual_ui(),
    htmltools::tags$div(
      class = "bp-hidden-file-input",
      shiny::fileInput("project_file", label = NULL, accept = c("application/json", ".json"))
    )
  )
}
