bp_help_nav_entries <- function(language) {
  if (identical(language, "zh")) {
    return(c(
      overview = "认识工作台",
      quickstart = "快速开始",
      library = "模块按钮与模板",
      layers = "图层栈",
      parameters = "参数与映射",
      preview = "预览与 R 代码",
      projects = "项目与导入导出",
      layout = "布局与快捷键",
      scope = "范围、安全与常见问题"
    ))
  }
  c(
    overview = "Workspace tour",
    quickstart = "Quick start",
    library = "Module buttons and templates",
    layers = "Layer stack",
    parameters = "Parameters and mappings",
    preview = "Preview and R code",
    projects = "Projects and import/export",
    layout = "Layout and shortcuts",
    scope = "Scope, safety, and FAQ"
  )
}

bp_help_nav <- function(language) {
  entries <- bp_help_nav_entries(language)
  htmltools::tags$nav(
    class = "bp-help-nav",
    `data-help-lang` = language,
    `aria-label` = if (identical(language, "zh")) "手册目录" else "Manual contents",
    hidden = if (identical(language, "zh")) NULL else "hidden",
    lapply(seq_along(entries), function(index) {
      key <- names(entries)[[index]]
      htmltools::tags$button(
        type = "button",
        class = paste("bp-help-nav-link", if (index == 1L) "is-active"),
        `data-help-target` = paste0("help-", language, "-", key),
        htmltools::tags$span(class = "bp-help-nav-index", sprintf("%02d", index)),
        htmltools::tags$span(entries[[index]])
      )
    })
  )
}

bp_help_section <- function(language, key, number, title, lead = NULL, ...) {
  htmltools::tags$section(
    id = paste0("help-", language, "-", key),
    class = "bp-help-section",
    `data-help-section` = key,
    htmltools::tags$div(
      class = "bp-help-section-heading",
      htmltools::tags$span(class = "bp-help-section-number", number),
      htmltools::tags$div(
        htmltools::tags$h2(title),
        if (is.null(lead)) NULL else htmltools::tags$p(class = "bp-help-section-lead", lead)
      )
    ),
    ...
  )
}

bp_help_feature_grid <- function(items) {
  htmltools::tags$div(
    class = "bp-help-feature-grid",
    lapply(items, function(item) {
      htmltools::tags$article(
        class = "bp-help-feature-card",
        htmltools::tags$h3(item$title),
        htmltools::tags$p(item$text)
      )
    })
  )
}

bp_help_steps <- function(items) {
  htmltools::tags$ol(
    class = "bp-help-steps",
    lapply(seq_along(items), function(index) {
      item <- items[[index]]
      htmltools::tags$li(
        htmltools::tags$span(class = "bp-help-step-number", index),
        htmltools::tags$div(
          htmltools::tags$strong(item$title),
          htmltools::tags$p(item$text)
        )
      )
    })
  )
}

bp_help_bullets <- function(items) {
  htmltools::tags$ul(class = "bp-help-bullets", lapply(items, htmltools::tags$li))
}

bp_help_table <- function(headers, rows) {
  htmltools::tags$div(
    class = "bp-help-table-wrap",
    htmltools::tags$table(
      class = "bp-help-table",
      htmltools::tags$thead(htmltools::tags$tr(lapply(headers, htmltools::tags$th))),
      htmltools::tags$tbody(lapply(rows, function(row) htmltools::tags$tr(lapply(row, htmltools::tags$td))))
    )
  )
}

bp_help_note <- function(title, text, tone = "info") {
  htmltools::tags$aside(
    class = paste("bp-help-note", paste0("bp-help-note-", tone)),
    bp_icon(if (identical(tone, "warning")) "warning" else "info", 19),
    htmltools::tags$div(htmltools::tags$strong(title), htmltools::tags$p(text))
  )
}

bp_help_faq <- function(question, answer) {
  htmltools::tags$details(
    class = "bp-help-faq",
    htmltools::tags$summary(question),
    htmltools::tags$p(answer)
  )
}

bp_help_hero <- function(language) {
  chinese <- identical(language, "zh")
  htmltools::tags$header(
    class = "bp-help-hero",
    htmltools::tags$p(
      class = "bp-help-eyebrow",
      if (chinese) "BioPlotBlocks · R/ggplot2 可视化代码编排器" else "BioPlotBlocks · Visual R/ggplot2 code composer"
    ),
    htmltools::tags$h1(if (chinese) "使用手册" else "User manual"),
    htmltools::tags$p(
      class = "bp-help-hero-lead",
      if (chinese) {
        "用可视模块搭建 ggplot2 图形，同时让生成的 R 代码保持清晰、可检查、可复制和可导出。"
      } else {
        "Build ggplot2 figures with visual modules while keeping the generated R code clear, inspectable, copyable, and exportable."
      }
    ),
    htmltools::tags$div(
      class = "bp-help-version-row",
      htmltools::tags$span("BioPlotBlocks v0.2 MVP"),
      htmltools::tags$span("R 4.5.1"),
      htmltools::tags$span("ggplot2 4.0.1")
    )
  )
}

bp_help_visual_mode_intro <- function(language) {
  chinese <- identical(language, "zh")
  bp_help_section(
    language, "visual-mode", "NEW",
    if (chinese) "可视化绘图模式" else "Visual plotting mode",
    if (chinese) {
      "默认的任务式界面面向常规绘图：依次选择数据源、图表类型、字段、样式和标题；R / 高级模式仍保留完整模块、原生参数和生成代码。"
    } else {
      "The task-driven interface is now the default for routine plotting. Choose data, chart type, fields, style, and labels in order; R / Advanced mode retains the full module stack, native arguments, and generated code."
    },
    bp_help_feature_grid(list(
      list(
        title = if (chinese) "箱线图抖动散点" else "Boxplot jitter points",
        text = if (chinese) "箱线图可按需叠加 geom_jitter 原始观测点，并设置点颜色、大小、透明度与横向抖动宽度。开启时会自动隐藏箱线图离群点以避免重复；用户仍可重新开启并查看重叠提示，关闭抖动点后恢复此前设置。" else "Boxplots can overlay raw observations with geom_jitter and configure point color, size, opacity, and horizontal jitter width. Enabling jitter hides boxplot outliers to avoid duplicates; users may re-enable them with an overlap warning, and disabling jitter restores the previous setting."
      ),
      list(
        title = if (chinese) "共享同一项目" else "One shared project",
        text = if (chinese) "两个模式读写同一份语义化项目状态，切换模式不会复制或丢失图形设置。" else "Both modes read and write the same semantic project state, so switching never duplicates or discards plot settings."
      ),
      list(
        title = if (chinese) "散点图、火山图、箱线图与 PCA" else "Scatter, volcano, boxplot, and PCA builders",
        text = if (chinese) "除常用散点图、火山图和箱线图外，PCA 支持表达矩阵与可选样本信息表，按样本 ID 关联，配置矩阵方向、转换、高变特征、中心化/标准化、主成分、分组与置信椭圆。" else "Alongside scatter, volcano, and boxplot builders, PCA accepts an expression matrix plus optional sample metadata, joins by sample ID, and configures orientation, transformation, variable features, centering/scaling, components, grouping, and confidence ellipses."
      ),
      list(
        title = if (chinese) "可复现的 PCA 结果" else "Reproducible PCA outputs",
        text = if (chinese) "PCA 得分与载荷作为只读派生数据提供预览和 CSV 导出；高级模式分别显示分析 R 与绘图 R，导出的脚本包含 stats::prcomp() 全流程。" else "PCA scores and loadings are exposed as read-only derived data for preview and CSV export. Advanced mode separates analysis R from plot R, while the exported script includes the complete stats::prcomp() workflow."
      ),
      list(
        title = if (chinese) "Raw count 引导分析" else "Guided raw-count analysis",
        text = if (chinese) "数据护照只提出语义建议；用户确认 Raw count 和 PCA 配方后，软件才执行低表达过滤、edgeR TMM + logCPM（或 log2 快速探索），并生成只读标准化表达、PCA 得分与载荷来源链。" else "The data passport only suggests semantics. After the user confirms raw counts and the PCA recipe, BioPlotBlocks filters low expression, runs edgeR TMM + logCPM (or the log2 exploratory fallback), and creates read-only normalized-expression, PCA-score, and loading lineage."
      ),
      list(
        title = if (chinese) "自动预览" else "Automatic preview",
        text = if (chinese) "有效设置会自动运行本地 ggplot2；失败时保留上一次成功图片，并显示可滚动的错误信息。" else "Valid edits run local ggplot2 automatically. On failure, the last successful image stays visible with scrollable error details."
      ),
      list(
        title = if (chinese) "高级设置已保留" else "Advanced settings preserved",
        text = if (chinese) "可视化界面无法安全表达的图层或参数不会被删除；界面会提示并允许切换到高级模式继续编辑。" else "Layers or arguments that cannot be represented safely are not removed; the interface flags them for continued editing in Advanced mode."
      )
    ))
  )
}

bp_help_document_zh <- function() {
  htmltools::tags$article(
    class = "bp-help-document",
    `data-help-lang` = "zh",
    lang = "zh-CN",
    bp_help_hero("zh"),
    bp_help_visual_mode_intro("zh"),
    bp_help_section(
      "zh", "overview", "01", "认识工作台",
      "界面把模块、原生 R 参数、绘图结果和生成代码放在同一个可检查的工作区中。",
      bp_help_feature_grid(list(
        list(title = "图层构建器", text = "模块分类按钮与图层栈位于同一区域；悬停或单击按钮即可添加函数。"),
        list(title = "参数检查器", text = "按 General、Advanced 或 All arguments 编辑所选模块的原生参数。"),
        list(title = "Preview", text = "在本地 R 中执行当前代码并显示真实 ggplot2 图形。"),
        list(title = "Generated R", text = "实时显示由模块状态生成的 R 代码，并与所选模块联动。"),
        list(title = "状态栏", text = "显示错误、警告、语义保真度、Schema、模块数和执行环境。")
      )),
      bp_help_note(
        "产品边界",
        "BioPlotBlocks 是 R/ggplot2 代码编排器，并提供明确确认的 Raw count → PCA 最小分析配方；它不会静默修改原始数据。复杂差异设计、批次校正、通路分析等仍不在当前核心范围。"
      )
    ),
    bp_help_section(
      "zh", "quickstart", "02", "快速开始",
      "从内置火山图模板开始，通常几分钟即可完成第一次编辑和导出。",
      bp_help_steps(list(
        list(title = "选择起点", text = "打开 Templates 按钮并选择 Volcano plot (DEGs)，或从分类按钮依次添加 ggplot 与所需图层。"),
        list(title = "选择并编辑模块", text = "在图层栈中单击一个模块，然后在右侧设置映射、颜色、大小、透明度、标签或主题参数。"),
        list(title = "运行真实预览", text = "单击顶部 Run preview，等待 Preview 区域显示图形，并检查底部错误与警告。"),
        list(title = "保存或导出", text = "用 Save 保存可继续编辑的 JSON 项目；用 Export 或 Download .R 导出可运行的 R 脚本。")
      )),
      bp_help_note("提示", "Assign plot 打开时，生成代码会赋值给指定对象（默认 p）；关闭后则只生成表达式。"),
      bp_help_note(
        "Raw count → PCA 快速流程",
        "选择 RNA-seq 引导，先导入并仅注册 count 矩阵与可选样本信息表；确认数据语义和矩阵方向，再选择 PCA、检查 CPM 过滤及 TMM-logCPM 配方并点击“使用当前设置并生成”。标准化表达、PCA 得分和载荷会作为只读派生数据保留。"
      )
    ),
    bp_help_section(
      "zh", "library", "03", "模块按钮与模板",
      "每个下拉选项对应一个真实 R 函数或 BioPlotBlocks 核心表达式。",
      bp_help_bullets(list(
        htmltools::tagList(htmltools::tags$strong("打开："), "悬停或单击 All、Core、Geoms、Structure、Scales、Templates，即可展开对应选项。"),
        htmltools::tagList(htmltools::tags$strong("搜索："), "打开 All 后，可按函数名、标题、包名或摘要关键词搜索。"),
        htmltools::tagList(htmltools::tags$strong("添加："), "单击函数行即可把模块加入图层栈；ggplot() 根模块始终位于最前方。"),
        htmltools::tagList(htmltools::tags$strong("状态："), "绿色检查表示 beta/已验证运行环境；警告三角表示 experimental，需要额外检查结果。"),
        htmltools::tagList(htmltools::tags$strong("模板："), "模板会展开为可见、可编辑的真实 ggplot2 模块，不会隐藏额外绘图逻辑。")
      )),
      bp_help_note("当前模板", "Volcano plot (DEGs) 使用内置示例数据框 df 及其预制列，展示点图、阈值线、颜色标尺、标签和经典主题。")
    ),
    bp_help_section(
      "zh", "layers", "04", "图层栈",
      "图层栈体现生成代码中模块的实际顺序。",
      bp_help_feature_grid(list(
        list(title = "选择", text = "单击模块正文，参数检查器和代码高亮会同步到该模块。"),
        list(title = "排序", text = "拖动左侧点状手柄，或使用上移/下移按钮改变 + 链顺序。"),
        list(title = "复制", text = "复制按钮会在当前模块后创建一个带相同参数的新实例。"),
        list(title = "折叠", text = "折叠仅减少界面占用，不改变参数或生成代码。"),
        list(title = "删除", text = "红色删除按钮移除模块；必要时可立即使用 Undo。"),
        list(title = "撤销/重做", text = "顶部 Undo 与 Redo 可恢复模块、参数和顺序的历史状态。")
      )),
      bp_help_note("代码顺序", "图层顺序会影响 ggplot2 的覆盖关系和最终结果。调整后请重新运行预览。", "warning")
    ),
    bp_help_section(
      "zh", "parameters", "05", "参数与映射",
      "参数名称和状态尽量保持目标 ggplot2 版本的原生语义。",
      bp_help_table(
        c("状态", "生成代码中的含义"),
        list(
          list(htmltools::tags$code("Unset"), "省略该参数，让 R/ggplot2 使用自身行为。"),
          list(htmltools::tags$code("Explicit"), "把当前值作为显式参数写入函数调用。"),
          list(htmltools::tags$code("Explicit default"), "即使等于形式默认值，也显式保留在代码中。"),
          list(htmltools::tags$code("NULL"), "生成显式 NULL。"),
          list(htmltools::tags$code("NA"), "生成带类型语义的 NA。"),
          list(htmltools::tags$code("Raw expression"), "保留无法用普通控件表达的 R 表达式。")
        )
      ),
      bp_help_feature_grid(list(
        list(title = "General", text = "最常用、最适合日常绘图的参数。"),
        list(title = "Advanced", text = "较少使用但仍已建模的高级参数。"),
        list(title = "All arguments", text = "显示当前模块已声明的全部参数。"),
        list(title = "Mapped aesthetics", text = "aes() 内的变量映射，例如 color = status。"),
        list(title = "列名建议", text = "x、y、color 等映射框可从当前数据列中选择，也可手动输入列名或 R 表达式。"),
        list(title = "数据源联动", text = "ggplot() 的 data 值可从已注册数据源中下拉选择。选择 ready 数据源会同步活动数据、预览和列名建议，保留有效映射并清除缺失列映射。"),
        list(title = "自定义 data 表达式", text = "手动输入 subset() 或 transform() 等 R 表达式时，软件不推断输出列、不切换活动数据源，并保留现有映射；请使用 Run preview 验证。"),
        list(title = "需重新链接", text = "relink required 数据源会显示在列表中，但在重新导入或链接原文件前不能切换为绘图数据。"),
        list(title = "Fixed values", text = "aes() 外的固定值，例如 color = \"red\"。"),
        list(title = "R expr", text = "打开表达式编辑器，适合函数调用、向量或其他原生 R 语法。")
      )),
      bp_help_note("映射与固定值不能混淆", "aes(color = status) 表示按数据列映射颜色；color = \"red\" 表示所有对象使用同一颜色。")
    ),
    bp_help_section(
      "zh", "preview", "06", "预览与 R 代码",
      "模块状态、生成代码和本地执行结果使用同一语义来源。",
      bp_help_bullets(list(
        htmltools::tagList(htmltools::tags$strong("Run preview："), "在独立本地 R 进程中执行当前项目并刷新图像。"),
        htmltools::tagList(htmltools::tags$strong("图形 / 数据："), "使用预览区标题栏按钮切换图形和数据。数据视图可选择当前数据源或内置 df 示例，显示前 30 行并支持横向、纵向滚动；切换只影响查看，不修改绘图映射。"),
        htmltools::tagList(htmltools::tags$strong("Cancel："), "终止正在运行的预览，不改变模块或生成代码。"),
        htmltools::tagList(htmltools::tags$strong("Generated R："), "参数修改后自动更新；单击代码行可反向选择对应模块。"),
        htmltools::tagList(htmltools::tags$strong("Copy："), "把当前生成代码复制到剪贴板。"),
        htmltools::tagList(htmltools::tags$strong("Download .R："), "下载包含必要设置的 R 脚本。"),
        htmltools::tagList(htmltools::tags$strong("诊断："), "底部状态栏报告错误和警告；预览失败时会显示来自 R 的信息。")
      )),
      bp_help_note("预览与代码", "代码生成会随参数立即更新；图像只有在 Run preview 后才会重新执行。")
    ),
    bp_help_section(
      "zh", "projects", "07", "项目与导入导出",
      "JSON 项目用于继续编辑，R 脚本用于运行、复核和分享。",
      bp_help_table(
        c("顶部操作", "用途"),
        list(
          list(htmltools::tags$strong("New"), "创建新的示例散点项目，并替换当前工作区。"),
          list(htmltools::tags$strong("Import R"), "粘贴支持的 R/ggplot2 代码并解析为模块；未知结构保留为 Raw R。"),
          list(htmltools::tags$strong("Import Data"), "导入 CSV、TSV、TXT、RDS 或 RData/rda。RData 在隔离环境中浏览多个对象；矩阵可转换为数据框并配置行名。"),
          list(htmltools::tags$strong("顶部 Data 标签"), "打开多数据源管理：预览、重命名、重新链接或移除数据。Use in plot 作为兼容入口，与右侧 data 参数使用同一套联动规则。"),
          list(htmltools::tags$strong("Save"), "下载版本化的 .bioplotblocks.json 项目文件。"),
          list(htmltools::tags$strong("Open folder"), "打开此前保存的 JSON 项目并恢复模块状态。"),
          list(htmltools::tags$strong("Export"), "下载当前项目对应的 .R 脚本。")
        )
      ),
      bp_help_note("保存前建议", "项目会保存数据源元数据和列映射，但不会嵌入完整原始表；重新打开后请按提示重新链接原文件。", "warning")
    ),
    bp_help_section(
      "zh", "layout", "08", "布局与快捷键",
      "各主要区域可以独立滚动；桌面端还可拖动分隔线调整空间。",
      bp_help_table(
        c("操作", "结果"),
        list(
          list(htmltools::tags$kbd("Ctrl / Cmd + Enter"), "运行预览。"),
          list(htmltools::tags$kbd("Ctrl / Cmd + Z"), "撤销。"),
          list(htmltools::tags$kbd("Ctrl / Cmd + Y"), "重做。"),
          list(htmltools::tags$kbd("Ctrl / Cmd + Shift + Z"), "重做。"),
          list("拖动分隔线", "调整图层构建器、检查器、预览或代码区域大小。"),
          list("分隔线 + 方向键", "每次微调 16 像素。"),
          list("双击分隔线", "恢复该分隔线的默认位置。"),
          list(htmltools::tags$kbd("Esc"), "关闭本手册并返回工作台。")
        )
      ),
      bp_help_note("窄屏行为", "在较窄窗口中，工作区会改为纵向排列并隐藏分隔线，以避免触控误操作；页面仍可正常滚动。")
    ),
    bp_help_section(
      "zh", "scope", "09", "范围、安全与常见问题",
      "本版本聚焦 R 与 ggplot2，并明确保留无法完整建模的表达式。",
      bp_help_bullets(list(
        "当前一等支持范围为 R 4.5.1 与 ggplot2 4.0.1；并非所有 ggplot2 函数和参数都已建模。",
        "支持的外层语法会恢复为模块；复杂或未知表达式会降级为 Raw R，而不是被静默删除。",
        "预览在本机 R 中运行，并拥有当前用户的系统权限；不要执行来源不可信的导入代码。",
        "精确空白、格式和源代码注释不保证往返保持，但目标 R 语义应尽量保持。"
      )),
      bp_help_faq("为什么预览没有随参数立即变化？", "参数会立即更新生成代码，但图像需要单击 Run preview 才会重新执行。"),
      bp_help_faq("为什么导入代码后出现 Raw R？", "该结构超出当前解析子集。BioPlotBlocks 会优先保留原始表达式，避免丢失语义。"),
      bp_help_faq("为什么某些函数带警告三角？", "它们处于 experimental 状态。可以使用，但应仔细检查生成代码、预览和目标 ggplot2 版本。"),
      bp_help_faq("预览失败时怎么办？", "先查看 Preview 错误文本和底部诊断，再检查数据列名、Raw expression 语法、参数类型以及模块顺序。"),
      bp_help_faq("RData 中哪些对象可以导入？", "本阶段支持 data.frame、tibble、matrix 和简单表格列表。函数、环境、连接、公式、外部指针会被禁止，其他复杂类会标记为暂不支持。"),
      bp_help_faq("如何重新开始？", "先保存需要保留的 JSON 或 R 文件，然后单击 New；也可以重新载入 Volcano plot 模板。")
    )
  )
}

bp_help_document_en <- function() {
  htmltools::tags$article(
    class = "bp-help-document",
    `data-help-lang` = "en",
    lang = "en",
    hidden = "hidden",
    bp_help_hero("en"),
    bp_help_visual_mode_intro("en"),
    bp_help_section(
      "en", "overview", "01", "Workspace tour",
      "The workspace keeps modules, native R parameters, the rendered plot, and generated code visible in one inspectable surface.",
      bp_help_feature_grid(list(
        list(title = "Layer builder", text = "Module category buttons and the layer stack share one region; hover or click a button to add functions."),
        list(title = "Parameter inspector", text = "Edit native arguments through General, Advanced, or All arguments."),
        list(title = "Preview", text = "Execute the current code in local R and display the real ggplot2 result."),
        list(title = "Generated R", text = "See deterministic R code generated from module state and linked to selection."),
        list(title = "Status bar", text = "Review errors, warnings, semantic fidelity, schema, module count, and runtime.")
      )),
      bp_help_note("Product boundary", "BioPlotBlocks composes R/ggplot2 code and provides an explicitly confirmed raw-count-to-PCA minimal recipe. It never silently changes source data. Complex differential designs, batch correction, and pathway analysis remain outside the current core scope.")
    ),
    bp_help_section(
      "en", "quickstart", "02", "Quick start",
      "Start from the built-in volcano plot and complete a first edit and export in a few minutes.",
      bp_help_steps(list(
        list(title = "Choose a starting point", text = "Open Templates and choose Volcano plot (DEGs), or add ggplot and the required layers from the category buttons."),
        list(title = "Select and edit a module", text = "Click a layer card, then set mappings, color, size, alpha, labels, or theme arguments in the inspector."),
        list(title = "Run a real preview", text = "Click Run preview, wait for the plot, and check the error and warning indicators in the status bar."),
        list(title = "Save or export", text = "Use Save for an editable JSON project; use Export or Download .R for a runnable R script.")
      )),
      bp_help_note("Tip", "When Assign plot is enabled, the generated code assigns the result to the chosen symbol (p by default). Disable it to emit only the expression."),
      bp_help_note(
        "Raw count → PCA quick path",
        "Choose RNA-seq Guided, import and register the count matrix plus optional sample metadata, confirm semantics and orientation, then select PCA, review the CPM filter and TMM-logCPM recipe, and click Use current settings and generate. Normalized expression, scores, and loadings remain available as read-only derived data."
      )
    ),
    bp_help_section(
      "en", "library", "03", "Module buttons and templates",
      "Every menu option represents a real R function or a BioPlotBlocks core expression.",
      bp_help_bullets(list(
        htmltools::tagList(htmltools::tags$strong("Open: "), "Hover or click All, Core, Geoms, Structure, Scales, or Templates to reveal its options."),
        htmltools::tagList(htmltools::tags$strong("Search: "), "Open All, then use a function name, title, package, or summary keyword."),
        htmltools::tagList(htmltools::tags$strong("Add: "), "Click a row to append a module; the ggplot() root remains first."),
        htmltools::tagList(htmltools::tags$strong("Status: "), "A green check marks beta/verified runtime status; a warning triangle marks experimental support."),
        htmltools::tagList(htmltools::tags$strong("Templates: "), "A template expands into visible, editable ggplot2 modules without hidden plotting logic.")
      )),
      bp_help_note("Current template", "Volcano plot (DEGs) uses the included example data frame df and prepared columns to demonstrate points, thresholds, a color scale, labels, and a classic theme.")
    ),
    bp_help_section(
      "en", "layers", "04", "Layer stack",
      "The stack is the actual order used by the generated ggplot2 + chain.",
      bp_help_feature_grid(list(
        list(title = "Select", text = "Click a card body to synchronize the inspector and code highlight."),
        list(title = "Reorder", text = "Drag the dotted handle or use the up/down controls."),
        list(title = "Duplicate", text = "Create a new instance with the same arguments immediately after the source."),
        list(title = "Collapse", text = "Reduce visual space without changing arguments or code."),
        list(title = "Delete", text = "Remove a module with the red control; use Undo if needed."),
        list(title = "Undo/redo", text = "Restore prior module, argument, and ordering states.")
      )),
      bp_help_note("Code order", "Layer order affects ggplot2 overpainting and the final result. Run the preview again after reordering.", "warning")
    ),
    bp_help_section(
      "en", "parameters", "05", "Parameters and mappings",
      "Argument names and states preserve the native semantics of the targeted ggplot2 version whenever possible.",
      bp_help_table(
        c("State", "Meaning in generated code"),
        list(
          list(htmltools::tags$code("Unset"), "Omit the argument and let R/ggplot2 apply its own behavior."),
          list(htmltools::tags$code("Explicit"), "Write the current value as an explicit argument."),
          list(htmltools::tags$code("Explicit default"), "Keep the formal default explicitly in the call."),
          list(htmltools::tags$code("NULL"), "Generate explicit NULL."),
          list(htmltools::tags$code("NA"), "Generate an NA value with its R type semantics."),
          list(htmltools::tags$code("Raw expression"), "Preserve R syntax that a standard control cannot represent.")
        )
      ),
      bp_help_feature_grid(list(
        list(title = "General", text = "Common arguments for routine plotting."),
        list(title = "Advanced", text = "Less common but modeled advanced arguments."),
        list(title = "All arguments", text = "Every argument declared by the current module."),
        list(title = "Mapped aesthetics", text = "Variables inside aes(), such as color = status."),
        list(title = "Column suggestions", text = "Mapping fields such as x, y, and color offer columns from the active data while still accepting typed column names or R expressions."),
        list(title = "Linked data-source switching", text = "Selecting a ready source in ggplot() data synchronizes the active data, preview, and column suggestions; compatible mappings are kept and missing direct-column mappings are cleared."),
        list(title = "Custom data expressions", text = "Typed subset(), transform(), and other R expressions keep the current active source and mappings because their output columns cannot be inferred safely. Verify them with Run preview."),
        list(title = "Relink required", text = "Unavailable sources remain visible but cannot become plot data until their original file is re-imported or relinked."),
        list(title = "Fixed values", text = "Constants outside aes(), such as color = \"red\"."),
        list(title = "R expr", text = "Open the expression editor for calls, vectors, or other native R syntax.")
      )),
      bp_help_note("Mappings are not fixed values", "aes(color = status) maps a data column; color = \"red\" assigns one color to every object.")
    ),
    bp_help_section(
      "en", "preview", "06", "Preview and R code",
      "Module state, generated code, and local execution share the same semantic source.",
      bp_help_bullets(list(
        htmltools::tagList(htmltools::tags$strong("Run preview: "), "Execute the project in a separate local R process and refresh the image."),
        htmltools::tagList(htmltools::tags$strong("Plot / Data: "), "Switch between the plot and data. The data view can show the active source or the built-in df example, keeps plot mappings unchanged, and scrolls the first 30 rows in both directions."),
        htmltools::tagList(htmltools::tags$strong("Cancel: "), "Stop an active preview without changing modules or code."),
        htmltools::tagList(htmltools::tags$strong("Generated R: "), "Updates after argument changes; click a code line to select its module."),
        htmltools::tagList(htmltools::tags$strong("Copy: "), "Copy the current generated code to the clipboard."),
        htmltools::tagList(htmltools::tags$strong("Download .R: "), "Download an R script with the required setup."),
        htmltools::tagList(htmltools::tags$strong("Diagnostics: "), "The status bar reports errors and warnings; preview failures show R output.")
      )),
      bp_help_note("Preview versus code", "Argument edits update code immediately. The image is re-executed only when you run the preview.")
    ),
    bp_help_section(
      "en", "projects", "07", "Projects and import/export",
      "Use JSON projects for continued editing and R scripts for execution, review, and sharing.",
      bp_help_table(
        c("Top command", "Purpose"),
        list(
          list(htmltools::tags$strong("New"), "Create a fresh example scatter project and replace the current workspace."),
          list(htmltools::tags$strong("Import R"), "Parse supported R/ggplot2 code into modules; preserve unknown structures as Raw R."),
          list(htmltools::tags$strong("Import Data"), "Import CSV, TSV, TXT, RDS, or RData/rda. Browse RData objects in isolation; convert matrices to data frames with explicit row-name handling."),
          list(htmltools::tags$strong("Top Data badge"), "Open multi-source management to preview, rename, relink, or remove data. Use in plot remains a compatibility entry and follows the same switching rules as the inspector data parameter."),
          list(htmltools::tags$strong("Save"), "Download a versioned .bioplotblocks.json project."),
          list(htmltools::tags$strong("Open folder"), "Restore a previously saved JSON project and its module state."),
          list(htmltools::tags$strong("Export"), "Download the current project as an .R script.")
        )
      ),
      bp_help_note("Before saving", "Projects retain data-source metadata and mappings, not the complete source table. Re-link the original file when prompted after reopening.", "warning")
    ),
    bp_help_section(
      "en", "layout", "08", "Layout and shortcuts",
      "Major regions scroll independently. On desktop, drag dividers to allocate space.",
      bp_help_table(
        c("Action", "Result"),
        list(
          list(htmltools::tags$kbd("Ctrl / Cmd + Enter"), "Run preview."),
          list(htmltools::tags$kbd("Ctrl / Cmd + Z"), "Undo."),
          list(htmltools::tags$kbd("Ctrl / Cmd + Y"), "Redo."),
          list(htmltools::tags$kbd("Ctrl / Cmd + Shift + Z"), "Redo."),
          list("Drag a divider", "Resize the layer builder, inspector, preview, or code region."),
          list("Divider + arrow key", "Adjust by 16 pixels."),
          list("Double-click a divider", "Restore that divider to its default position."),
          list(htmltools::tags$kbd("Esc"), "Close this manual and return to the workspace.")
        )
      ),
      bp_help_note("Narrow screens", "The workspace changes to a vertical layout and hides dividers to prevent accidental touch resizing. The page remains scrollable.")
    ),
    bp_help_section(
      "en", "scope", "09", "Scope, safety, and FAQ",
      "This release focuses on R and ggplot2 and explicitly preserves expressions it cannot fully model.",
      bp_help_bullets(list(
        "The first-class target is R 4.5.1 with ggplot2 4.0.1. Not every ggplot2 function or argument is modeled.",
        "Supported outer syntax becomes modules. Complex or unknown expressions degrade to Raw R instead of being silently removed.",
        "Preview code runs in local R with the current user's system permissions. Do not execute imported code from untrusted sources.",
        "Exact whitespace, formatting, and source comments are not guaranteed to round-trip, although target R semantics are preserved where possible."
      )),
      bp_help_faq("Why did the plot not change immediately?", "Argument edits update generated code immediately, but the plot is re-executed only after Run preview."),
      bp_help_faq("Why did imported code become Raw R?", "The structure is outside the current parser subset. Preserving the expression prevents semantic loss."),
      bp_help_faq("Why does a function show a warning triangle?", "It is experimental. You can use it, but inspect the generated code, preview, and target ggplot2 version carefully."),
      bp_help_faq("What should I do when preview fails?", "Read the Preview error and status diagnostics, then check data column names, Raw expression syntax, argument types, and layer order."),
      bp_help_faq("Which RData objects can I import?", "This stage supports data.frame, tibble, matrix, and simple table-like lists. Functions, environments, connections, formulas, and external pointers are forbidden; other complex classes are shown as unsupported."),
      bp_help_faq("How do I start over?", "Save any JSON or R file you need, then click New or reload the Volcano plot template.")
    )
  )
}

bp_help_manual_ui <- function() {
  htmltools::tags$section(
    id = "bp-help-view",
    class = "bp-help-view",
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = "bp-help-title",
    `aria-hidden` = "true",
    hidden = "hidden",
    htmltools::tags$header(
      class = "bp-help-topbar",
      htmltools::tags$div(
        class = "bp-help-brand",
        bp_brand_mark(31),
        htmltools::tags$div(
          htmltools::tags$strong(id = "bp-help-title", "使用手册"),
          htmltools::tags$span("BioPlotBlocks Help")
        )
      ),
      htmltools::tags$div(
        class = "bp-help-top-actions",
        htmltools::tags$div(
          class = "bp-help-language-switch",
          role = "group",
          `aria-label` = "Language / 语言",
          htmltools::tags$button(
            type = "button", class = "bp-help-language is-active", `data-help-language` = "zh",
            `aria-pressed` = "true", "中文"
          ),
          htmltools::tags$button(
            type = "button", class = "bp-help-language", `data-help-language` = "en",
            `aria-pressed` = "false", "English"
          )
        ),
        htmltools::tags$button(
          type = "button",
          class = "bp-command-button bp-help-close",
          `aria-label` = "关闭手册并返回工作台",
          title = "关闭手册并返回工作台 (Esc)",
          bp_icon("close", 17),
          htmltools::tags$span(class = "bp-help-close-label", "返回工作台")
        )
      )
    ),
    htmltools::tags$div(
      class = "bp-help-layout",
      htmltools::tags$aside(
        class = "bp-help-sidebar",
        htmltools::tags$label(
          class = "bp-help-search",
          htmltools::tags$span(class = "bp-search-icon", bp_icon("search", 17)),
          htmltools::tags$input(
            id = "bp-help-search-input",
            type = "search",
            placeholder = "搜索手册",
            `aria-label` = "搜索手册",
            autocomplete = "off"
          )
        ),
        htmltools::tags$p(class = "bp-help-sidebar-title", "CONTENTS · 目录"),
        bp_help_nav("zh"),
        bp_help_nav("en")
      ),
      htmltools::tags$main(
        class = "bp-help-main",
        tabindex = "-1",
        bp_help_document_zh(),
        bp_help_document_en(),
        htmltools::tags$div(
          id = "bp-help-no-results",
          class = "bp-help-no-results",
          hidden = "hidden",
          bp_icon("search", 30),
          htmltools::tags$strong("没有找到相关内容"),
          htmltools::tags$p("请尝试其他关键词。")
        )
      )
    )
  )
}
