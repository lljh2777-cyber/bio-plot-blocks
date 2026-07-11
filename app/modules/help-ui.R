bp_help_nav_entries <- function(language) {
  if (identical(language, "zh")) {
    return(c(
      overview = "认识工作台",
      quickstart = "快速开始",
      library = "函数库与模板",
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
    library = "Library and templates",
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

bp_help_document_zh <- function() {
  htmltools::tags$article(
    class = "bp-help-document",
    `data-help-lang` = "zh",
    lang = "zh-CN",
    bp_help_hero("zh"),
    bp_help_section(
      "zh", "overview", "01", "认识工作台",
      "界面把模块、原生 R 参数、绘图结果和生成代码放在同一个可检查的工作区中。",
      bp_help_feature_grid(list(
        list(title = "函数库", text = "搜索并筛选 ggplot2 函数，单击即可加入项目。"),
        list(title = "图层栈", text = "查看 ggplot() 根模块以及按 + 连接的图层、标尺和主题。"),
        list(title = "参数检查器", text = "按 General、Advanced 或 All arguments 编辑所选模块的原生参数。"),
        list(title = "Preview", text = "在本地 R 中执行当前代码并显示真实 ggplot2 图形。"),
        list(title = "Generated R", text = "实时显示由模块状态生成的 R 代码，并与所选模块联动。"),
        list(title = "状态栏", text = "显示错误、警告、语义保真度、Schema、模块数和执行环境。")
      )),
      bp_help_note(
        "产品边界",
        "BioPlotBlocks 是 R/ggplot2 代码编排器，不负责差异分析、数据清洗或其他生物信息学计算，也不会静默修改你的数据或优化代码。"
      )
    ),
    bp_help_section(
      "zh", "quickstart", "02", "快速开始",
      "从内置火山图模板开始，通常几分钟即可完成第一次编辑和导出。",
      bp_help_steps(list(
        list(title = "选择起点", text = "单击左下方 Volcano plot (DEGs) 模板，或从函数库依次添加 ggplot 与所需图层。"),
        list(title = "选择并编辑模块", text = "在图层栈中单击一个模块，然后在右侧设置映射、颜色、大小、透明度、标签或主题参数。"),
        list(title = "运行真实预览", text = "单击顶部 Run preview，等待 Preview 区域显示图形，并检查底部错误与警告。"),
        list(title = "保存或导出", text = "用 Save 保存可继续编辑的 JSON 项目；用 Export 或 Download .R 导出可运行的 R 脚本。")
      )),
      bp_help_note("提示", "Assign plot 打开时，生成代码会赋值给指定对象（默认 p）；关闭后则只生成表达式。")
    ),
    bp_help_section(
      "zh", "library", "03", "函数库与模板",
      "函数库中的每一项对应一个真实 R 函数或 BioPlotBlocks 核心表达式。",
      bp_help_bullets(list(
        htmltools::tagList(htmltools::tags$strong("搜索："), "在 Search functions 中输入函数名、标题、包名或摘要关键词。"),
        htmltools::tagList(htmltools::tags$strong("筛选："), "All、Core、Geoms、Structure、Scales 和 Templates 可快速缩小范围。"),
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
          list(htmltools::tags$strong("Import Data"), "导入 CSV、TSV 或 TXT，检查列类型和质量，并将确认的列映射应用到绘图。"),
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
          list("拖动分隔线", "调整函数库、图层栈、检查器、预览或代码区域大小。"),
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
    bp_help_section(
      "en", "overview", "01", "Workspace tour",
      "The workspace keeps modules, native R parameters, the rendered plot, and generated code visible in one inspectable surface.",
      bp_help_feature_grid(list(
        list(title = "Function library", text = "Search and filter ggplot2 functions, then click to add one."),
        list(title = "Layer stack", text = "Inspect the ggplot() root and the layers, scales, and themes joined with +."),
        list(title = "Parameter inspector", text = "Edit native arguments through General, Advanced, or All arguments."),
        list(title = "Preview", text = "Execute the current code in local R and display the real ggplot2 result."),
        list(title = "Generated R", text = "See deterministic R code generated from module state and linked to selection."),
        list(title = "Status bar", text = "Review errors, warnings, semantic fidelity, schema, module count, and runtime.")
      )),
      bp_help_note("Product boundary", "BioPlotBlocks composes R/ggplot2 code. It does not perform differential analysis, data cleaning, or other bioinformatics computations, and it never silently changes your data or optimizes your code.")
    ),
    bp_help_section(
      "en", "quickstart", "02", "Quick start",
      "Start from the built-in volcano plot and complete a first edit and export in a few minutes.",
      bp_help_steps(list(
        list(title = "Choose a starting point", text = "Click Volcano plot (DEGs), or add ggplot and the required layers from the function library."),
        list(title = "Select and edit a module", text = "Click a layer card, then set mappings, color, size, alpha, labels, or theme arguments in the inspector."),
        list(title = "Run a real preview", text = "Click Run preview, wait for the plot, and check the error and warning indicators in the status bar."),
        list(title = "Save or export", text = "Use Save for an editable JSON project; use Export or Download .R for a runnable R script.")
      )),
      bp_help_note("Tip", "When Assign plot is enabled, the generated code assigns the result to the chosen symbol (p by default). Disable it to emit only the expression.")
    ),
    bp_help_section(
      "en", "library", "03", "Library and templates",
      "Every library entry represents a real R function or a BioPlotBlocks core expression.",
      bp_help_bullets(list(
        htmltools::tagList(htmltools::tags$strong("Search: "), "Use a function name, title, package, or summary keyword."),
        htmltools::tagList(htmltools::tags$strong("Filter: "), "Use All, Core, Geoms, Structure, Scales, or Templates."),
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
          list(htmltools::tags$strong("Import Data"), "Import CSV, TSV, or TXT data, review types and quality, then apply confirmed column mappings to the plot."),
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
          list("Drag a divider", "Resize the library, stack, inspector, preview, or code region."),
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
