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

bp_visual_chart_card <- function(id, chart_type, title, subtitle, icon, requirement, active = FALSE, disabled = FALSE) {
  shiny::actionButton(
    inputId = id,
    label = htmltools::tagList(
      bp_icon(icon, 22),
      htmltools::tags$span(
        htmltools::tags$strong(title),
        htmltools::tags$small(subtitle),
        htmltools::tags$span(class = "bp-visual-chart-requirement", paste0("数据要求：", requirement))
      ),
      if (disabled) htmltools::tags$em("后续阶段") else bp_icon("check", 16)
    ),
    class = paste("bp-visual-chart-card", if (active) "is-active", if (disabled) "is-disabled"),
    disabled = disabled,
    `data-chart-type` = chart_type,
    `aria-pressed` = if (active) "true" else "false",
    `aria-label` = paste(title, subtitle, "数据要求", requirement)
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
              htmltools::tags$div(
                class = "bp-non-pca-only",
                shiny::selectInput("visual_data_source", label = "数据表", choices = c("正在读取…" = ""), selectize = FALSE, width = "100%"),
                shiny::uiOutput("visual_data_profile"),
                htmltools::tags$div(
                  id = "visual_data_preview",
                  class = "bp-visual-data-preview",
                  htmltools::tags$input(
                    id = "visual_data_preview_toggle",
                    class = "bp-visual-data-preview-checkbox",
                    type = "checkbox",
                    `aria-controls` = "visual_data_preview_content",
                    `aria-label` = "预览数据：前 30 行和全部列"
                  ),
                  htmltools::tags$label(
                    `for` = "visual_data_preview_toggle",
                    class = "bp-visual-data-preview-toggle",
                    htmltools::tags$span(class = "bp-visual-data-preview-title", bp_icon("table", 15), "预览数据"),
                    htmltools::tags$span(class = "bp-visual-data-preview-hint", "前 30 行 · 全部列"),
                    htmltools::tags$span(class = "bp-visual-data-preview-chevron", "⌄")
                  ),
                  htmltools::tags$div(
                    id = "visual_data_preview_content",
                    class = "bp-visual-data-preview-content",
                    shiny::uiOutput("visual_active_data_preview")
                  )
                )
              ),
              htmltools::tags$div(
                class = "bp-pca-only bp-pca-source-card",
                hidden = "hidden",
                shiny::selectInput("visual_pca_expression_source", "表达矩阵 *", choices = c("正在读取…" = ""), selectize = FALSE, width = "100%"),
                shiny::selectInput(
                  "visual_pca_orientation", "矩阵方向 *",
                  choices = c("自动识别" = "auto", "基因 × 样本" = "genes_by_samples", "样本 × 特征" = "samples_by_features"),
                  width = "100%"
                ),
                htmltools::tags$div(
                  class = "bp-pca-source-grid",
                  shiny::selectizeInput("visual_pca_feature_id_field", "特征 ID 列", choices = NULL, options = list(placeholder = "自动 / 行名"), width = "100%"),
                  shiny::selectizeInput("visual_pca_expression_sample_id_field", "表达表样本 ID 列", choices = NULL, options = list(placeholder = "自动 / 列名或行名"), width = "100%")
                ),
                shiny::selectInput("visual_pca_metadata_source", "样本信息表（可选）", choices = c("不使用样本信息" = ""), selectize = FALSE, width = "100%"),
                htmltools::tags$div(
                  class = "bp-pca-source-grid",
                  shiny::selectizeInput("visual_pca_metadata_id_field", "样本信息 ID 列", choices = NULL, options = list(placeholder = "自动识别 Sample / ID"), width = "100%"),
                  shiny::selectInput("visual_pca_unmatched_policy", "不匹配样本", choices = c("严格：必须完全匹配" = "strict", "仅使用交集" = "matched_only"), width = "100%")
                ),
                shiny::uiOutput("visual_pca_link_diagnostics")
              )
            ),
            htmltools::tags$section(
              id = "visual-section-chart",
              class = "bp-visual-config-section",
              bp_visual_section_header("02", "选择图表类型", "支持散点图、火山图、箱线图和基于表达矩阵的 PCA 图。"),
              htmltools::tags$div(
                class = "bp-visual-chart-grid",
                bp_visual_chart_card("visual_chart_scatter", "scatter", "散点图", "比较两个连续变量", "point", "至少 2 个数值型字段（X、Y）", active = TRUE),
                bp_visual_chart_card("visual_chart_volcano", "volcano", "火山图", "差异表达结果", "plot", "倍数变化列 + P 值或 FDR 列"),
                bp_visual_chart_card("visual_chart_boxplot", "boxplot", "箱线图", "比较组间分布", "boxplot", "1 个分组字段 + 1 个数值字段"),
                bp_visual_chart_card("visual_chart_pca", "pca", "PCA 图", "样本降维概览", "mapping", "表达矩阵；可选样本分组信息")
              )
            ),
            htmltools::tags$section(
              id = "visual-section-fields",
              class = "bp-visual-config-section",
              bp_visual_section_header(
                "03", "映射数据字段", "散点图需要 X/Y；火山图需要倍数变化和显著性字段；箱线图需要分组和数值字段；PCA 可选择主成分、颜色、形状和标签。",
                shiny::actionButton("visual_recommend_fields", "智能推荐", icon = shiny::icon("wand-magic-sparkles"), class = "bp-link-button")
              ),
              htmltools::tags$div(class = "bp-non-pca-only", shiny::uiOutput("visual_field_recommendation")),
              htmltools::tags$div(
                class = "bp-visual-field-grid bp-non-pca-only",
                htmltools::tags$div(class = "bp-visual-field-control", `data-visual-field` = "x", shiny::selectizeInput("visual_x", "X 轴字段 *", choices = NULL, options = list(placeholder = "选择数值列"), width = "100%")),
                htmltools::tags$div(class = "bp-visual-field-control", `data-visual-field` = "y", shiny::selectizeInput("visual_y", "Y 轴字段 *", choices = NULL, options = list(placeholder = "选择数值列"), width = "100%")),
                htmltools::tags$div(class = "bp-visual-field-control", `data-visual-field` = "color", shiny::selectizeInput("visual_color", "颜色/状态分组", choices = NULL, options = list(placeholder = "不映射颜色"), width = "100%")),
                htmltools::tags$div(class = "bp-point-only", shiny::selectizeInput("visual_size", "点大小映射", choices = NULL, options = list(placeholder = "固定大小"), width = "100%")),
                htmltools::tags$div(class = "bp-point-only", shiny::selectizeInput("visual_label", "标签字段", choices = NULL, options = list(placeholder = "不显示标签"), width = "100%"))
              ),
              htmltools::tags$div(
                class = "bp-visual-transform-grid bp-non-pca-only",
                htmltools::tags$div(class = "bp-non-boxplot-only", shiny::selectInput("visual_x_scale", "X 值转换", choices = c("线性" = "linear", "log10" = "log10", "-log10" = "neg_log10"), width = "100%")),
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
              ),
              htmltools::tags$div(
                class = "bp-pca-only bp-pca-fields-card",
                hidden = "hidden",
                htmltools::tags$div(
                  class = "bp-pca-source-grid",
                  shiny::selectInput("visual_pca_x_component", "横轴主成分 *", choices = c("PC1" = "PC1"), width = "100%"),
                  shiny::selectInput("visual_pca_y_component", "纵轴主成分 *", choices = c("PC2" = "PC2"), width = "100%")
                ),
                htmltools::tags$div(
                  class = "bp-visual-field-grid",
                  shiny::selectizeInput("visual_pca_color", "颜色分组", choices = NULL, options = list(placeholder = "不映射颜色"), width = "100%"),
                  shiny::selectizeInput("visual_pca_shape", "形状分组", choices = NULL, options = list(placeholder = "不映射形状"), width = "100%"),
                  shiny::selectizeInput("visual_pca_label", "样本标签", choices = NULL, options = list(placeholder = "不显示标签"), width = "100%")
                ),
                htmltools::tags$h3(class = "bp-pca-subheading", "预处理"),
                htmltools::tags$div(
                  class = "bp-pca-preprocess-grid",
                  shiny::selectInput("visual_pca_transform", "数据转换", choices = c("自动" = "auto", "不转换" = "none", "log2(x + 1)" = "log2p1"), width = "100%"),
                  shiny::selectInput("visual_pca_feature_count", "高变特征", choices = c("全部特征" = "all", "前 500" = "500", "前 1000" = "1000", "前 2000" = "2000", "自定义" = "custom"), width = "100%"),
                  shiny::numericInput("visual_pca_custom_feature_count", "自定义特征数", value = 1000, min = 2, step = 50, width = "100%"),
                  shiny::selectInput("visual_pca_missing_policy", "缺失值", choices = c("停止并提示" = "stop", "移除含缺失值的特征" = "omit_features"), width = "100%")
                ),
                htmltools::tags$div(
                  class = "bp-pca-check-grid",
                  shiny::checkboxInput("visual_pca_remove_zero_variance", "移除零方差特征", value = TRUE),
                  shiny::checkboxInput("visual_pca_center", "中心化", value = TRUE),
                  shiny::checkboxInput("visual_pca_scale", "标准化（scale）", value = FALSE)
                ),
                shiny::uiOutput("visual_pca_result_summary")
              )
            ),
            htmltools::tags$section(
              id = "visual-section-style",
              class = "bp-visual-config-section",
              bp_visual_section_header("04", "设置常用样式", "仅展示高频选项；原有高级参数不会被删除。"),
              htmltools::tags$div(
                class = "bp-visual-control-grid",
                htmltools::tags$div(class = "bp-visual-color-control", `data-visual-style` = "primary-color", htmltools::tags$span(class = "bp-visual-color-swatch"), shiny::textInput("visual_point_color", "点颜色", value = "#2C7FB8", width = "100%")),
                htmltools::tags$div(class = "bp-visual-size-control", shiny::numericInput("visual_point_size", "固定点大小", value = 2, min = 0.1, max = 20, step = 0.1, width = "100%")),
                shiny::numericInput("visual_alpha", "透明度", value = 0.72, min = 0, max = 1, step = 0.05, width = "100%"),
                htmltools::tags$div(class = "bp-point-only", shiny::selectInput("visual_shape", "点形状", choices = c("实心圆" = "16", "空心圆" = "1", "实心方形" = "15", "实心三角" = "17", "菱形" = "18"), width = "100%")),
                shiny::selectInput("visual_palette", "分组调色板", choices = c("沿用项目 / 默认" = "default", "蓝–灰–红" = "blue_red", "Viridis 风格" = "viridis_like"), width = "100%"),
                htmltools::tags$div(class = "bp-scatter-only", shiny::selectInput("visual_trend", "趋势线", choices = c("无" = "none", "线性拟合" = "linear", "平滑拟合" = "smooth"), width = "100%")),
                shiny::selectInput("visual_theme", "图表主题", choices = c("经典" = "classic", "简洁" = "minimal", "黑白" = "bw"), width = "100%"),
                shiny::numericInput("visual_base_size", "基础字号", value = 12, min = 6, max = 40, step = 1, width = "100%")
              ),
              htmltools::tags$div(
                class = "bp-visual-control-grid bp-boxplot-only",
                hidden = "hidden",
                htmltools::tags$div(
                  class = "bp-visual-color-control",
                  htmltools::tags$span(class = "bp-visual-color-swatch"),
                  shiny::textInput("visual_box_border_color", "箱体边框色", value = "#334155", width = "100%")
                ),
                shiny::checkboxInput("visual_box_show_outliers", "显示离群点", value = TRUE),
                shiny::numericInput("visual_box_outlier_size", "离群点大小", value = 1.5, min = 0.1, max = 10, step = 0.1, width = "100%")
              ),
              htmltools::tags$div(
                class = "bp-boxplot-jitter-card bp-boxplot-only",
                hidden = "hidden",
                shiny::checkboxInput("visual_box_jitter", "叠加抖动散点（geom_jitter）", value = FALSE),
                htmltools::tags$p("将原始观测点叠加在箱线图上；仅进行横向抖动，便于查看样本分布。"),
                shiny::conditionalPanel(
                  condition = "input.visual_box_jitter && input.visual_box_show_outliers",
                  htmltools::tags$div(
                    class = "bp-boxplot-overlap-warning",
                    bp_icon("warning", 14),
                    htmltools::tags$span("离群点会同时由 geom_boxplot() 和 geom_jitter() 绘制，可能出现重叠或颜色加深。")
                  )
                ),
                htmltools::tags$div(
                  class = "bp-visual-control-grid bp-boxplot-jitter-options",
                  htmltools::tags$div(
                    class = "bp-visual-color-control",
                    htmltools::tags$span(class = "bp-visual-color-swatch"),
                    shiny::textInput("visual_box_jitter_color", "抖动点颜色", value = "#334155", width = "100%")
                  ),
                  shiny::numericInput("visual_box_jitter_size", "抖动点大小", value = 1.4, min = 0.1, max = 10, step = 0.1, width = "100%"),
                  shiny::numericInput("visual_box_jitter_alpha", "抖动点透明度", value = 0.55, min = 0, max = 1, step = 0.05, width = "100%"),
                  shiny::numericInput("visual_box_jitter_width", "横向抖动宽度", value = 0.16, min = 0, max = 1, step = 0.01, width = "100%")
                )
              ),
              htmltools::tags$div(
                class = "bp-pca-only bp-pca-ellipse-card",
                hidden = "hidden",
                shiny::checkboxInput("visual_pca_show_ellipse", "显示分组置信椭圆（stat_ellipse）", value = FALSE),
                shiny::numericInput("visual_pca_ellipse_level", "置信水平", value = 0.95, min = 0.5, max = 0.999, step = 0.01, width = "100%"),
                htmltools::tags$p("需要先选择颜色分组；每组样本数过少时 ggplot2 可能无法计算椭圆。")
              ),
              htmltools::tags$div(
                class = "bp-visual-reference-card bp-non-pca-only",
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
                htmltools::tags$div(class = "bp-non-pca-only", shiny::textInput("visual_x_label", "X 轴标题", value = "", width = "100%")),
                htmltools::tags$div(class = "bp-non-pca-only", shiny::textInput("visual_y_label", "Y 轴标题", value = "", width = "100%")),
                shiny::textInput("visual_legend_title", "图例标题", value = "", width = "100%")
              ),
              htmltools::tags$p(class = "bp-pca-only bp-pca-axis-note", hidden = "hidden", "PCA 坐标轴会自动显示主成分及解释方差百分比。")
            ),
            htmltools::tags$section(
              id = "visual-section-export",
              class = "bp-visual-config-section bp-visual-export-section",
              bp_visual_section_header("06", "保存与导出", "顶部可保存项目或导出可复现的 R 脚本。"),
              htmltools::tags$p("可视化模式和 R / 高级模式共享同一项目；切换模式不会丢失设置。"),
              htmltools::tags$div(
                class = "bp-pca-only bp-pca-export-actions",
                hidden = "hidden",
                shiny::downloadButton("download_pca_scores", "导出 PCA 得分 CSV", class = "bp-command-button"),
                shiny::downloadButton("download_pca_loadings", "导出 PCA 载荷 CSV", class = "bp-command-button")
              )
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
          shiny::uiOutput("analysis_code_view"),
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
