bp_clone_project <- function(project) {
  unserialize(serialize(project, NULL))
}

bp_find_instance_index <- function(project, instance_id) {
  ids <- vapply(project$modules %||% list(), `[[`, character(1), "instance_id")
  match(instance_id, ids)
}

bp_new_scatter_project <- function(registry) {
  bp_basic_scatter_project(registry)
}

bp_category_filter <- function(category) {
  switch(
    category,
    core = "core",
    geom = "geoms",
    structure = "structure",
    theme = "structure",
    scale = "scales",
    raw = "core",
    category
  )
}

bp_status_icon <- function(status) {
  if (status %in% c("stable", "beta")) {
    htmltools::tags$span(class = "bp-module-status bp-module-status-verified", title = status, bp_icon("check", 15))
  } else {
    htmltools::tags$span(class = "bp-module-status bp-module-status-experimental", title = status, bp_icon("warning", 15))
  }
}

bp_library_row <- function(spec) {
  nested <- identical(spec$composition$required_context %||% spec$compatibility$required_context, "mapping_argument")
  htmltools::tags$button(
    type = "button",
    class = paste("bp-library-row bp-add-module", if (nested) "bp-library-row-nested"),
    `data-module-id` = spec$id,
    title = if (nested) "Use this nested module through a mapping argument" else paste("Add", spec$symbol),
    htmltools::tags$span(class = paste("bp-category-rail", paste0("bp-category-", bp_category_filter(spec$presentation$category)))),
    htmltools::tags$span(class = "bp-library-icon", bp_icon(spec$presentation$icon %||% "code", 17)),
    htmltools::tags$span(
      class = "bp-library-copy",
      htmltools::tags$strong(spec$symbol),
      htmltools::tags$small(if (nested) "nested mapping" else spec$presentation$summary)
    ),
    htmltools::tags$span(class = "bp-library-package", if (identical(spec$package, "ggplot2")) "ggplot2" else "core"),
    bp_status_icon(spec$status)
  )
}

bp_template_row <- function(template) {
  htmltools::tags$button(
    type = "button",
    class = "bp-library-row bp-template-row bp-load-template",
    `data-template-id` = template$id,
    title = template$description,
    htmltools::tags$span(class = "bp-category-rail bp-category-templates"),
    htmltools::tags$span(class = "bp-library-icon", bp_icon("template", 18)),
    htmltools::tags$span(
      class = "bp-library-copy",
      htmltools::tags$strong(template$display_title %||% template$title),
      htmltools::tags$small("Expands to visible ggplot2 modules")
    ),
    htmltools::tags$span(class = "bp-library-package", "ggplot2"),
    htmltools::tags$span(class = "bp-module-status bp-module-status-verified", bp_icon("check", 15))
  )
}

bp_argument_summary <- function(instance) {
  set <- Filter(function(argument) !bp_is_unset(argument), instance$arguments %||% list())
  if (!length(set)) return("No explicit arguments")
  pieces <- vapply(names(set), function(name) {
    paste0(name, " = ", bp_value_to_source(set[[name]]$value))
  }, character(1))
  result <- paste(pieces, collapse = ", ")
  if (nchar(result) > 116L) paste0(substr(result, 1L, 115L), "…") else result
}

bp_layer_action <- function(action, instance_id, icon, label) {
  htmltools::tags$button(
    type = "button",
    class = paste("bp-icon-button bp-layer-action", if (action == "delete") "bp-layer-delete"),
    `data-action` = action,
    `data-instance-id` = instance_id,
    title = label,
    `aria-label` = label,
    bp_icon(icon, 16)
  )
}

bp_layer_row <- function(instance, spec, selected = FALSE, position = 1L, total = 1L) {
  category <- bp_category_filter(spec$presentation$category)
  full_name <- if (identical(spec$package, "ggplot2")) paste0("ggplot2::", spec$symbol) else "Raw R expression"
  htmltools::tags$div(
    class = "bp-layer-node",
    htmltools::tags$span(class = "bp-plus-node", if (position == 1L) "" else "+"),
    htmltools::tags$div(
      class = paste("bp-layer-card", paste0("bp-layer-category-", category), if (selected) "is-selected", if (isTRUE(instance$collapsed)) "is-collapsed"),
      draggable = "true",
      `data-instance-id` = instance$instance_id,
      htmltools::tags$span(class = paste("bp-layer-category-rail", paste0("bp-category-", category))),
      htmltools::tags$button(
        type = "button",
        class = "bp-drag-handle bp-layer-action",
        `data-action` = "select",
        `data-instance-id` = instance$instance_id,
        title = "Select and drag module",
        bp_icon("grip", 17)
      ),
      htmltools::tags$button(
        type = "button",
        class = "bp-layer-main bp-layer-action",
        `data-action` = "select",
        `data-instance-id` = instance$instance_id,
        htmltools::tags$span(
          class = "bp-layer-title-row",
          htmltools::tags$strong(spec$symbol),
          htmltools::tags$span(class = "bp-layer-full-name", full_name)
        ),
        htmltools::tags$span(class = "bp-layer-summary", bp_argument_summary(instance))
      ),
      htmltools::tags$div(
        class = "bp-layer-controls",
        if (position > 1L) bp_layer_action("move_up", instance$instance_id, "move_up", "Move up"),
        if (position < total) bp_layer_action("move_down", instance$instance_id, "move_down", "Move down"),
        bp_layer_action("duplicate", instance$instance_id, "duplicate", "Duplicate module"),
        bp_layer_action("collapse", instance$instance_id, if (isTRUE(instance$collapsed)) "chevron_down" else "chevron_up", if (isTRUE(instance$collapsed)) "Expand module" else "Collapse module"),
        bp_layer_action("delete", instance$instance_id, "trash", "Delete module")
      )
    )
  )
}

bp_argument_value <- function(argument, parameter) {
  value <- argument$value
  if (is.null(value)) value <- parameter$formal_default
  bp_value_to_source(value)
}

bp_state_select <- function(instance_id, name, state) {
  options <- bp_state_options()
  htmltools::tags$select(
    class = paste("bp-param-state", paste0("state-", state)),
    `data-instance-id` = instance_id,
    `data-param` = name,
    `aria-label` = paste("State for", name),
    lapply(names(options), function(label) {
      value <- options[[label]]
      htmltools::tags$option(value = value, selected = if (identical(value, state)) "selected" else NULL, label)
    })
  )
}

bp_value_control <- function(instance_id, name, argument, parameter) {
  control <- parameter$ui_control %||% "expression"
  value <- bp_argument_value(argument, parameter)
  common_attrs <- list(
    class = "bp-param-value",
    `data-instance-id` = instance_id,
    `data-param` = name,
    `data-control` = control,
    `aria-label` = paste("Value for", name)
  )

  if (identical(control, "logical_state")) {
    choices <- c("TRUE", "FALSE", "NA")
    return(do.call(htmltools::tags$select, c(common_attrs, list(
      lapply(choices, function(choice) htmltools::tags$option(
        value = choice,
        selected = if (identical(value, choice)) "selected" else NULL,
        choice
      ))
    ))))
  }
  if (identical(control, "enum")) {
    choices <- unique(c(unlist(parameter$ui_options %||% list()), value))
    return(do.call(htmltools::tags$select, c(common_attrs, list(
      lapply(choices, function(choice) htmltools::tags$option(
        value = choice,
        selected = if (identical(value, choice)) "selected" else NULL,
        choice
      ))
    ))))
  }
  type <- if (control %in% c("numeric", "number", "numeric_or_expression", "integer") && grepl("^-?[0-9.]+L?$", value)) "number" else "text"
  if (identical(type, "number")) value <- sub("L$", "", value)
  do.call(htmltools::tags$input, c(common_attrs, list(
    type = type,
    value = value,
    step = if (type == "number") "any" else NULL,
    placeholder = if (identical(argument$state, "unset")) "Unset" else NULL,
    spellcheck = "false"
  )))
}

bp_aes_editor <- function(instance_id, name, argument) {
  mapping <- argument$value
  mappings <- if (!is.null(mapping) && identical(bp_value_type(mapping), "RAesMapping")) mapping$mappings %||% list() else list()
  keys <- c("x", "y", "color", "fill", "shape", "group", "label")
  htmltools::tags$details(
    class = "bp-aes-editor",
    htmltools::tags$summary(
      class = "bp-aes-editor-label",
      bp_icon("mapping", 14),
      htmltools::tags$span("Edit mapped aesthetics"),
      htmltools::tags$code(bp_value_to_source(mapping %||% bp_aes_mapping()))
    ),
    htmltools::tags$div(
      class = "bp-aes-grid",
      lapply(keys, function(key) {
        htmltools::tags$label(
          class = "bp-aes-field",
          htmltools::tags$span(key),
          htmltools::tags$input(
            type = "text",
            class = "bp-aes-value",
            `data-instance-id` = instance_id,
            `data-param` = name,
            `data-aes-key` = key,
            value = if (!is.null(mappings[[key]])) bp_value_to_source(mappings[[key]]) else "",
            placeholder = "column or R expression",
            spellcheck = "false"
          )
        )
      })
    )
  )
}

bp_expression_editor <- function(instance_id, name, value) {
  htmltools::tags$div(
    class = "bp-expression-editor",
    htmltools::tags$div(
      class = "bp-expression-editor-title",
      htmltools::tags$span("R expression"),
      htmltools::tags$span(class = "bp-expression-param", name)
    ),
    shiny::textAreaInput("raw_expression_value", label = NULL, value = value, rows = 4, resize = "vertical"),
    htmltools::tags$div(
      class = "bp-inline-warning",
      bp_icon("warning", 18),
      htmltools::tags$span("This value will be preserved as Raw R Expression and will not be coerced or silently corrected.")
    ),
    htmltools::tags$div(
      class = "bp-fragment-preview",
      htmltools::tags$span("Generated R fragment preview"),
      htmltools::tags$code(name, " = ", shiny::textOutput("expression_fragment", inline = TRUE))
    ),
    htmltools::tags$div(
      class = "bp-expression-actions",
      shiny::actionButton("cancel_expression", "Cancel", class = "bp-command-button"),
      shiny::actionButton("apply_expression", "Use expression", class = "bp-command-button bp-command-primary")
    ),
    `data-instance-id` = instance_id,
    `data-param` = name
  )
}

bp_parameter_row <- function(instance, parameter, argument, effective_mapping, expression_edit) {
  name <- parameter$name
  state <- argument$state %||% "unset"
  mapping_key <- if (identical(name, "colour")) "color" else name
  mapped <- state == "unset" && mapping_key %in% names(effective_mapping)
  inherited <- state == "unset" && name %in% c("data", "mapping") && instance$module_id != "r.ggplot2.ggplot"

  row <- htmltools::tags$div(
    class = paste("bp-parameter-row", if (identical(parameter$ui_control, "aes_editor")) "bp-parameter-row-aes"),
    htmltools::tags$div(class = "bp-parameter-name", htmltools::tags$strong(name)),
    htmltools::tags$div(class = "bp-parameter-origin", gsub("_", " ", parameter$source)),
    htmltools::tags$div(
      class = "bp-parameter-state",
      if (mapped) htmltools::tags$span(class = "bp-effective-badge mapped", "Mapped in aes()"),
      if (inherited) htmltools::tags$span(class = "bp-effective-badge inherited", "Inherited"),
      bp_state_select(instance$instance_id, name, state)
    ),
    htmltools::tags$div(
      class = "bp-parameter-value",
      if (identical(parameter$ui_control, "aes_editor")) {
        htmltools::tags$code(class = "bp-aes-summary", bp_argument_value(argument, parameter))
      } else {
        bp_value_control(instance$instance_id, name, argument, parameter)
      }
    ),
    htmltools::tags$div(
      class = "bp-parameter-expression",
      htmltools::tags$button(
        type = "button",
        class = "bp-expression-button",
        `data-instance-id` = instance$instance_id,
        `data-param` = name,
        htmltools::tagList(htmltools::tags$span("R expr"), bp_icon("code", 14))
      )
    )
  )

  details <- if (identical(parameter$ui_control, "aes_editor")) {
    bp_aes_editor(instance$instance_id, name, argument)
  }
  editor <- if (!is.null(expression_edit) &&
      identical(expression_edit$instance_id, instance$instance_id) &&
      identical(expression_edit$parameter, name)) {
    bp_expression_editor(instance$instance_id, name, expression_edit$value)
  }
  htmltools::tagList(row, details, editor)
}

bp_default_explicit_value <- function(parameter) {
  default <- parameter$formal_default
  if (!is.null(default)) return(default)
  control <- parameter$ui_control %||% "expression"
  if (control %in% c("numeric", "number", "numeric_or_expression", "integer")) return(bp_double(1))
  if (identical(control, "logical_state")) return(bp_logical(TRUE))
  if (identical(control, "aes_editor")) return(bp_aes_mapping())
  if (identical(control, "symbol_or_expression")) return(bp_symbol("df"))
  if (identical(control, "formula_editor")) return(bp_formula("~ group"))
  if (identical(control, "enum")) return(bp_character(unlist(parameter$ui_options %||% list("identity"))[[1]]))
  if (control %in% c("text", "string", "color_or_expression")) return(bp_character(""))
  bp_raw_expression("NULL")
}

bp_seed_added_module <- function(instance, spec) {
  symbol <- spec$symbol
  if (identical(instance$module_id, "core.raw_r")) {
    instance$arguments$expression <- bp_argument("raw_expression", bp_raw_expression("theme()"), "nested_expression")
  }
  if (identical(symbol, "geom_hline")) {
    instance$arguments$yintercept <- bp_argument("explicit", bp_double(0), "formal")
  }
  if (identical(symbol, "geom_vline")) {
    instance$arguments$xintercept <- bp_argument("explicit", bp_double(0), "formal")
  }
  if (identical(symbol, "facet_wrap")) {
    instance$arguments$facets <- bp_argument("explicit", bp_formula("~ group"), "formal")
  }
  if (symbol %in% c("scale_color_manual", "scale_fill_manual")) {
    instance$arguments$values <- bp_argument(
      "raw_expression",
      bp_raw_expression('c(A = "#2C7FB8", B = "#D73027")'),
      "formal"
    )
  }
  instance
}

bp_workspace_server <- function(input, output, session, registry, templates, root) {
  initial <- bp_project_from_template("bio.volcano.basic", registry)
  selected_initial <- initial$modules[[2]]$instance_id
  state <- shiny::reactiveValues(
    project = initial,
    selected = selected_initial,
    library_filter = "all",
    parameter_tab = "common",
    history = list(),
    future = list(),
    expression_edit = NULL,
    preview_process = NULL,
    preview_status = "initial",
    preview_result = NULL,
    preview_image = NULL,
    preview_status_file = NULL
  )

  commit <- function(project, selected = state$selected, record_history = TRUE) {
    current <- shiny::isolate(state$project)
    if (isTRUE(record_history)) {
      history <- c(shiny::isolate(state$history), list(bp_clone_project(current)))
      if (length(history) > 60L) history <- tail(history, 60L)
      state$history <- history
      state$future <- list()
    }
    project$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    state$project <- project
    ids <- vapply(project$modules %||% list(), `[[`, character(1), "instance_id")
    state$selected <- if (length(ids) && selected %in% ids) selected else if (length(ids)) ids[[1]] else NULL
  }

  start_preview <- function() {
    process <- shiny::isolate(state$preview_process)
    if (!is.null(process) && process$is_alive()) process$kill()
    status_path <- tempfile("bioplotblocks-preview-", fileext = ".json")
    image_path <- tempfile("bioplotblocks-preview-", fileext = ".png")
    state$preview_status <- "running"
    state$preview_result <- NULL
    state$preview_status_file <- status_path
    state$preview_image <- image_path
    process <- tryCatch(
      bp_start_preview_process(shiny::isolate(state$project), root, status_path, image_path),
      error = identity
    )
    if (inherits(process, "error")) {
      state$preview_status <- "error"
      state$preview_result <- list(ok = FALSE, error = conditionMessage(process), warnings = list(), messages = list())
      state$preview_process <- NULL
    } else {
      state$preview_process <- process
    }
  }

  output$library_filters <- shiny::renderUI({
    filters <- c(All = "all", Core = "core", Geoms = "geoms", Structure = "structure", Scales = "scales", Templates = "templates")
    htmltools::tags$div(
      class = "bp-library-filters",
      lapply(names(filters), function(label) {
        value <- filters[[label]]
        htmltools::tags$button(
          type = "button",
          class = paste("bp-filter-button", if (identical(state$library_filter, value)) "is-active"),
          `data-filter` = value,
          label
        )
      })
    )
  })

  output$module_library <- shiny::renderUI({
    query <- tolower(trimws(input$module_search %||% ""))
    filter <- state$library_filter
    specs <- registry
    specs <- Filter(function(spec) {
      category <- bp_category_filter(spec$presentation$category)
      filter_ok <- filter %in% c("all", "templates") || identical(category, filter)
      text <- tolower(paste(spec$symbol, spec$presentation$title, spec$presentation$summary, spec$package))
      query_ok <- !nzchar(query) || grepl(query, text, fixed = TRUE)
      filter_ok && query_ok && !identical(filter, "templates")
    }, specs)
    if (!length(specs)) {
      return(htmltools::tags$div(class = "bp-library-empty", "No matching functions"))
    }
    htmltools::tags$div(class = "bp-library-list", lapply(specs, bp_library_row))
  })

  output$template_library <- shiny::renderUI({
    filter <- state$library_filter
    query <- tolower(trimws(input$module_search %||% ""))
    visible <- Filter(function(template) {
      filter %in% c("all", "templates") && (!nzchar(query) || grepl(query, tolower(paste(template$title, template$description)), fixed = TRUE))
    }, templates)
    if (!length(visible)) return(NULL)
    htmltools::tagList(
      htmltools::tags$div(class = "bp-library-heading bp-template-heading", "Templates"),
      htmltools::tags$div(class = "bp-library-list bp-template-list", lapply(visible, bp_template_row))
    )
  })

  output$assignment_editor <- shiny::renderUI({
    assignment <- state$project$assignment
    htmltools::tags$div(
      class = "bp-assignment-row",
      htmltools::tags$label(
        class = "bp-assignment-toggle",
        htmltools::tags$input(
          type = "checkbox",
          class = "bp-assignment-enabled",
          checked = if (isTRUE(assignment$enabled)) "checked" else NULL
        ),
        htmltools::tags$span("Assign plot")
      ),
      htmltools::tags$input(
        type = "text",
        class = "bp-assignment-target",
        value = assignment$target %||% "p",
        disabled = if (!isTRUE(assignment$enabled)) "disabled" else NULL,
        `aria-label` = "Assignment target",
        spellcheck = "false"
      ),
      htmltools::tags$code(class = "bp-assignment-operator", assignment$operator %||% "<-"),
      htmltools::tags$span(class = "bp-assignment-hint", "optional · assign the plot to a name")
    )
  })

  output$layer_stack <- shiny::renderUI({
    modules <- state$project$modules %||% list()
    if (!length(modules)) {
      return(htmltools::tags$div(
        class = "bp-stack-empty",
        bp_icon("plot", 34),
        htmltools::tags$h3("Start with a ggplot module"),
        htmltools::tags$p("Choose a function from the library. Unsupported expressions can remain Raw R.")
      ))
    }
    htmltools::tags$div(
      class = "bp-layer-stack",
      lapply(seq_along(modules), function(index) {
        instance <- modules[[index]]
        spec <- bp_get_spec(registry, instance$module_id)
        bp_layer_row(instance, spec, identical(state$selected, instance$instance_id), index, length(modules))
      })
    )
  })

  output$parameter_inspector <- shiny::renderUI({
    project <- state$project
    index <- bp_find_instance_index(project, state$selected)
    if (is.na(index)) {
      return(htmltools::tags$div(class = "bp-inspector-empty", bp_icon("info", 28), htmltools::tags$p("Select a module to inspect native R arguments.")))
    }
    instance <- project$modules[[index]]
    spec <- bp_get_spec(registry, instance$module_id)
    parameters <- spec$parameters %||% list()
    tab <- state$parameter_tab
    if (identical(tab, "common")) parameters <- Filter(function(x) identical(x$ui_group, "common"), parameters)
    if (identical(tab, "advanced")) parameters <- Filter(function(x) identical(x$ui_group, "advanced"), parameters)
    effective <- bp_effective_mapping(project, index)
    set_count <- sum(vapply(instance$arguments %||% list(), function(x) !bp_is_unset(x), logical(1)))

    constraints <- Filter(function(parameter) {
      argument <- instance$arguments[[parameter$name]]
      constraint <- parameter$constraints
      if (is.null(argument) || is.null(constraint) || bp_is_unset(argument) || !identical(bp_value_type(argument$value), "RDouble")) return(FALSE)
      value <- argument$value$value
      (!is.null(constraint$recommended_minimum) && value < constraint$recommended_minimum) ||
        (!is.null(constraint$recommended_maximum) && value > constraint$recommended_maximum)
    }, spec$parameters %||% list())

    htmltools::tagList(
      htmltools::tags$div(
        class = "bp-inspector-header",
        htmltools::tags$div(
          htmltools::tags$h2(paste0(if (identical(spec$package, "ggplot2")) "ggplot2::" else "", spec$symbol)),
          htmltools::tags$div(
            class = "bp-inspector-meta",
            htmltools::tags$span(class = "bp-package-mark", bp_icon(if (identical(spec$package, "ggplot2")) "plot" else "code", 15), if (identical(spec$package, "ggplot2")) "ggplot2 4.0.1" else "BioPlotBlocks core"),
            htmltools::tags$span(class = "bp-verified-mark", bp_icon("check", 15), spec$status)
          )
        ),
        htmltools::tags$button(type = "button", class = "bp-icon-button bp-close-inspector", title = "Collapse inspector", bp_icon("close", 17))
      ),
      htmltools::tags$div(
        class = "bp-inspector-tabs",
        htmltools::tags$button(type = "button", class = paste("bp-inspector-tab", if (tab == "common") "is-active"), `data-param-tab` = "common", "General"),
        htmltools::tags$button(type = "button", class = paste("bp-inspector-tab", if (tab == "advanced") "is-active"), `data-param-tab` = "advanced", "Advanced"),
        htmltools::tags$button(type = "button", class = paste("bp-inspector-tab", if (tab == "all") "is-active"), `data-param-tab` = "all", "All arguments", htmltools::tags$span(class = "bp-tab-count", length(spec$parameters)))
      ),
      htmltools::tags$div(class = "bp-inspector-section-title", if (tab == "all") paste0("All native arguments · ", set_count, " set") else "Native settings mapping"),
      htmltools::tags$div(
        class = "bp-parameter-table",
        htmltools::tags$div(
          class = "bp-parameter-header",
          htmltools::tags$span("Argument"), htmltools::tags$span("Origin"), htmltools::tags$span("State"), htmltools::tags$span("Value"), htmltools::tags$span("Expression")
        ),
        lapply(parameters, function(parameter) {
          argument <- instance$arguments[[parameter$name]] %||% bp_argument(origin = parameter$source)
          bp_parameter_row(instance, parameter, argument, effective, state$expression_edit)
        })
      ),
      if (length(constraints)) htmltools::tags$div(
        class = "bp-inspector-warning",
        bp_icon("warning", 18),
        htmltools::tags$span("A value is outside its recommended range. It was preserved unchanged.")
      ),
      htmltools::tags$div(
        class = "bp-inspector-docs",
        htmltools::tags$h3("Documentation & provenance"),
        htmltools::tags$div(
          class = "bp-doc-grid",
          htmltools::tags$span(bp_icon("open", 16), "Installed help"),
          htmltools::tags$span(bp_icon("plot", 16), if (identical(spec$package, "ggplot2")) "ggplot2 4.0.1" else "core 0.2.0"),
          htmltools::tags$span(class = "bp-doc-verified", bp_icon("check", 16), spec$provenance$confidence),
          htmltools::tags$a(
            href = if (identical(spec$package, "ggplot2")) paste0("https://ggplot2.tidyverse.org/reference/", spec$documentation$reference_topic, ".html") else "#",
            target = "_blank",
            rel = "noopener noreferrer",
            "Open help",
            bp_icon("export", 13)
          )
        )
      )
    )
  })

  output$expression_fragment <- shiny::renderText({
    input$raw_expression_value %||% state$expression_edit$value %||% ""
  })

  output$code_view <- shiny::renderUI({
    lines <- tryCatch(bp_generate_lines(state$project, registry), error = identity)
    if (inherits(lines, "error")) {
      return(htmltools::tags$div(class = "bp-code-error", bp_icon("warning", 20), conditionMessage(lines)))
    }
    if (!length(lines)) return(htmltools::tags$div(class = "bp-code-empty", "Add a module to generate R code."))
    htmltools::tags$div(
      class = "bp-code-editor",
      lapply(lines, function(line) {
        htmltools::tags$button(
          type = "button",
          class = paste("bp-code-line", if (identical(line$instance_id, state$selected)) "is-selected"),
          `data-instance-id` = line$instance_id,
          htmltools::tags$span(class = "bp-line-number", line$line_number),
          htmltools::tags$code(htmltools::HTML(bp_highlight_r_line(line$text)))
        )
      })
    )
  })

  output$code_line_count <- shiny::renderUI({
    count <- length(state$project$modules %||% list())
    htmltools::tags$span(class = "bp-line-count", paste(count, if (count == 1L) "line" else "lines"))
  })

  output$generated_code_transport <- shiny::renderUI({
    code <- tryCatch(bp_generate_code(state$project, registry), error = function(error) "")
    htmltools::tags$textarea(id = "generated_code_raw", class = "bp-code-transport", code)
  })

  output$preview_image <- shiny::renderUI({
    path <- state$preview_image
    status <- state$preview_status
    shiny::req(!is.null(path), file.exists(path), identical(status, "success"))
    htmltools::tags$img(
      src = base64enc::dataURI(file = path, mime = "image/png"),
      alt = "ggplot2 preview generated from the current module stack"
    )
  })

  output$preview_overlay <- shiny::renderUI({
    status <- state$preview_status
    if (identical(status, "success")) return(NULL)
    if (identical(status, "running")) {
      return(htmltools::tags$div(class = "bp-preview-overlay", htmltools::tags$span(class = "bp-spinner"), htmltools::tags$strong("Running real ggplot2"), htmltools::tags$p("The editor remains responsive; cancel is available.")))
    }
    if (identical(status, "error")) {
      return(htmltools::tags$div(class = "bp-preview-overlay bp-preview-error", bp_icon("warning", 28), htmltools::tags$strong("Preview failed"), htmltools::tags$p(state$preview_result$error %||% "Unknown R error")))
    }
    if (identical(status, "cancelled")) {
      return(htmltools::tags$div(class = "bp-preview-overlay", bp_icon("info", 28), htmltools::tags$strong("Preview cancelled"), htmltools::tags$p("Your module state and code were not changed.")))
    }
    htmltools::tags$div(class = "bp-preview-overlay", bp_icon("plot", 30), htmltools::tags$strong("Preview ready"), htmltools::tags$p("Run the current module stack with local R."))
  })

  output$status_bar <- shiny::renderUI({
    diagnostics <- bp_project_diagnostics(state$project, registry)
    errors <- unique(c(
      vapply(Filter(function(x) identical(x$level, "error"), diagnostics), `[[`, character(1), "message"),
      if (identical(state$preview_status, "error")) state$preview_result$error %||% character() else character()
    ))
    warnings <- unique(c(
      vapply(Filter(function(x) identical(x$level, "warning"), diagnostics), `[[`, character(1), "message"),
      unlist(state$preview_result$warnings %||% list(), use.names = FALSE)
    ))
    ready_label <- switch(state$preview_status, running = "Running", error = "Needs attention", cancelled = "Cancelled", "Ready")
    htmltools::tagList(
      htmltools::tags$div(class = paste("bp-status-item bp-ready-status", paste0("status-", state$preview_status)), htmltools::tags$span(class = "bp-status-dot"), ready_label),
      htmltools::tags$div(class = "bp-status-item bp-error-status", bp_icon(if (length(errors)) "warning" else "check", 16), paste(length(errors), if (length(errors) == 1L) "error" else "errors")),
      htmltools::tags$div(class = "bp-status-item bp-warning-status", bp_icon("warning", 16), paste(length(warnings), if (length(warnings) == 1L) "warning" else "warnings")),
      htmltools::tags$div(class = "bp-status-item bp-fidelity-status", "Semantic fidelity", htmltools::tags$span(class = "bp-fidelity-meter", lapply(seq_len(5), function(x) htmltools::tags$i())), "100%"),
      htmltools::tags$div(class = "bp-status-item", "Schema 0.2.0"),
      htmltools::tags$div(class = "bp-status-item", paste("Modules", length(registry))),
      htmltools::tags$div(class = "bp-status-item bp-local-status", htmltools::tags$span(class = "bp-live-dot"), "Execution: Local R")
    )
  })

  shiny::observeEvent(input$library_filter, {
    state$library_filter <- input$library_filter$value %||% input$library_filter
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$parameter_tab, {
    state$parameter_tab <- input$parameter_tab$value %||% input$parameter_tab
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$add_module, {
    module_id <- input$add_module$module_id %||% input$add_module$value
    spec <- registry[[module_id]]
    if (is.null(spec)) return()
    if (identical(spec$compatibility$required_context, "mapping_argument")) {
      shiny::showNotification("aes() is edited through a mapping argument rather than added to the + chain.", type = "message")
      return()
    }
    project <- bp_clone_project(state$project)
    if (identical(spec$composition$output_type, "ggplot_object") && any(vapply(project$modules, function(x) identical(x$module_id, module_id), logical(1)))) {
      shiny::showNotification("This project already has a ggplot() root module.", type = "warning")
      return()
    }
    instance <- bp_seed_added_module(bp_instantiate_module(module_id, registry), spec)
    if (identical(spec$composition$output_type, "ggplot_object")) {
      project$modules <- c(list(instance), project$modules)
    } else {
      project$modules <- c(project$modules, list(instance))
    }
    commit(project, instance$instance_id)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$load_template, {
    template_id <- input$load_template$template_id %||% input$load_template$value
    project <- bp_project_from_template(template_id, registry)
    selected <- if (length(project$modules) >= 2L) project$modules[[2]]$instance_id else project$modules[[1]]$instance_id
    commit(project, selected)
    shiny::updateTextInput(session, "project_name", value = project$name)
    start_preview()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$module_action, {
    action <- input$module_action$action
    instance_id <- input$module_action$instance_id
    project <- bp_clone_project(state$project)
    index <- bp_find_instance_index(project, instance_id)
    if (is.na(index)) return()
    if (identical(action, "select")) {
      state$selected <- instance_id
      return()
    }
    if (identical(action, "delete")) {
      project$modules <- project$modules[-index]
      next_selected <- if (length(project$modules)) project$modules[[min(index, length(project$modules))]]$instance_id else NULL
      commit(project, next_selected)
    }
    if (identical(action, "duplicate")) {
      clone <- bp_clone_instance(project$modules[[index]])
      project$modules <- append(project$modules, list(clone), after = index)
      commit(project, clone$instance_id)
    }
    if (identical(action, "collapse")) {
      project$modules[[index]]$collapsed <- !isTRUE(project$modules[[index]]$collapsed)
      commit(project, instance_id)
    }
    if (identical(action, "move_up") && index > 1L) {
      project$modules[c(index - 1L, index)] <- project$modules[c(index, index - 1L)]
      commit(project, instance_id)
    }
    if (identical(action, "move_down") && index < length(project$modules)) {
      project$modules[c(index, index + 1L)] <- project$modules[c(index + 1L, index)]
      commit(project, instance_id)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$reorder_modules, {
    ids <- unlist(input$reorder_modules$ids %||% input$reorder_modules, use.names = FALSE)
    project <- bp_clone_project(state$project)
    existing <- vapply(project$modules, `[[`, character(1), "instance_id")
    if (length(ids) != length(existing) || !setequal(ids, existing)) return()
    project$modules <- project$modules[match(ids, existing)]
    commit(project)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$select_from_code, {
    id <- input$select_from_code$instance_id %||% input$select_from_code$value
    if (!is.na(bp_find_instance_index(state$project, id))) state$selected <- id
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$assignment_change, {
    project <- bp_clone_project(state$project)
    if (identical(input$assignment_change$kind, "enabled")) {
      project$assignment$enabled <- isTRUE(input$assignment_change$value)
    } else {
      target <- trimws(input$assignment_change$value %||% "")
      if (!grepl("^[.A-Za-z][.A-Za-z0-9_]*$", target)) {
        shiny::showNotification("Assignment target must be a valid R symbol.", type = "warning")
        return()
      }
      project$assignment$target <- target
    }
    commit(project)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$param_change, {
    payload <- input$param_change
    project <- bp_clone_project(state$project)
    index <- bp_find_instance_index(project, payload$instance_id)
    if (is.na(index)) return()
    instance <- project$modules[[index]]
    spec <- bp_get_spec(registry, instance$module_id)
    parameter <- bp_parameter_spec(spec, payload$param)
    if (is.null(parameter)) return()
    argument <- instance$arguments[[payload$param]] %||% bp_argument(origin = parameter$source)

    if (identical(payload$kind, "state")) {
      new_state <- payload$value
      argument$state <- new_state
      if (identical(new_state, "explicit")) argument$value <- argument$value %||% bp_default_explicit_value(parameter)
      if (identical(new_state, "explicit_default")) argument$value <- parameter$formal_default
      if (identical(new_state, "explicit_null")) argument$value <- bp_null()
      if (identical(new_state, "explicit_na")) argument$value <- bp_na()
      if (identical(new_state, "raw_expression")) {
        source <- bp_value_to_source(argument$value %||% parameter$formal_default)
        if (!nzchar(source)) source <- "NULL"
        argument$value <- bp_raw_expression(source)
      }
    }
    if (identical(payload$kind, "value")) {
      if ((payload$control %||% parameter$ui_control) %in% c("numeric", "number", "numeric_or_expression", "integer") &&
          !nzchar(trimws(payload$value %||% ""))) {
        return()
      }
      argument$state <- "explicit"
      argument$value <- bp_value_from_text(payload$value, payload$control %||% parameter$ui_control, "explicit")
    }
    if (identical(payload$kind, "aes")) {
      mapping <- argument$value
      if (is.null(mapping) || !identical(bp_value_type(mapping), "RAesMapping")) mapping <- bp_aes_mapping()
      key <- payload$aes_key
      value <- trimws(payload$value %||% "")
      if (!nzchar(value)) {
        mapping$mappings[[key]] <- NULL
      } else {
        mapping$mappings[[key]] <- bp_value_from_text(value, "symbol_or_expression", "explicit")
      }
      argument$state <- "explicit"
      argument$value <- mapping
    }
    instance$arguments[[payload$param]] <- argument
    project$modules[[index]] <- instance
    commit(project, instance$instance_id)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$expression_open, {
    payload <- input$expression_open
    index <- bp_find_instance_index(state$project, payload$instance_id)
    if (is.na(index)) return()
    argument <- state$project$modules[[index]]$arguments[[payload$param]]
    value <- bp_value_to_source(argument$value) %||% ""
    state$expression_edit <- list(instance_id = payload$instance_id, parameter = payload$param, value = value)
    session$onFlushed(function() shiny::updateTextAreaInput(session, "raw_expression_value", value = value), once = TRUE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$cancel_expression, {
    state$expression_edit <- NULL
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$apply_expression, {
    edit <- state$expression_edit
    if (is.null(edit)) return()
    value <- trimws(input$raw_expression_value %||% "")
    if (!nzchar(value)) {
      shiny::showNotification("A Raw R Expression cannot be empty.", type = "warning")
      return()
    }
    parsed <- tryCatch(bp_parse_single_expression(value), error = identity)
    if (inherits(parsed, "error")) {
      shiny::showNotification(conditionMessage(parsed), type = "error", duration = NULL)
      return()
    }
    project <- bp_clone_project(state$project)
    index <- bp_find_instance_index(project, edit$instance_id)
    argument <- project$modules[[index]]$arguments[[edit$parameter]]
    argument$state <- "raw_expression"
    argument$value <- bp_raw_expression(value)
    project$modules[[index]]$arguments[[edit$parameter]] <- argument
    state$expression_edit <- NULL
    commit(project, edit$instance_id)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$undo, {
    history <- state$history
    if (!length(history)) return()
    previous <- history[[length(history)]]
    state$future <- c(list(bp_clone_project(state$project)), state$future)
    state$history <- history[-length(history)]
    commit(previous, state$selected, record_history = FALSE)
    shiny::updateTextInput(session, "project_name", value = previous$name)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$redo, {
    future <- state$future
    if (!length(future)) return()
    next_project <- future[[1]]
    state$history <- c(state$history, list(bp_clone_project(state$project)))
    state$future <- future[-1]
    commit(next_project, state$selected, record_history = FALSE)
    shiny::updateTextInput(session, "project_name", value = next_project$name)
  }, ignoreInit = TRUE)

  project_name_debounced <- shiny::debounce(shiny::reactive(input$project_name), 450)
  shiny::observeEvent(project_name_debounced(), {
    name <- trimws(project_name_debounced() %||% "")
    if (!nzchar(name) || identical(name, state$project$name)) return()
    project <- bp_clone_project(state$project)
    project$name <- name
    commit(project, record_history = FALSE)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$new_project, {
    project <- bp_new_scatter_project(registry)
    commit(project, project$modules[[2]]$instance_id)
    shiny::updateTextInput(session, "project_name", value = project$name)
    state$expression_edit <- NULL
    state$preview_status <- "initial"
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$import_r, {
    current <- tryCatch(bp_generate_code(state$project, registry), error = function(error) "")
    shiny::showModal(shiny::modalDialog(
      title = "Import supported R / ggplot2 code",
      size = "l",
      easyClose = TRUE,
      htmltools::tags$p(class = "bp-modal-note", "Supported outer calls become modules. Complex inner expressions remain Raw R and are never silently removed."),
      shiny::textAreaInput("import_source", label = NULL, value = current, rows = 14, width = "100%", resize = "vertical"),
      footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("parse_import", "Parse into modules", class = "bp-command-primary"))
    ))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$parse_import, {
    parsed <- bp_parse_code(input$import_source %||% "", registry)
    commit(parsed, if (length(parsed$modules)) parsed$modules[[1]]$instance_id else NULL)
    shiny::updateTextInput(session, "project_name", value = parsed$name)
    shiny::removeModal()
    if (identical(parsed$parse_support, "D")) {
      shiny::showNotification("The source was preserved as Raw R because its outer structure is unsupported.", type = "warning", duration = NULL)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_file, {
    shiny::req(input$project_file$datapath)
    project <- tryCatch(bp_load_project(input$project_file$datapath), error = identity)
    if (inherits(project, "error")) {
      shiny::showNotification(conditionMessage(project), type = "error", duration = NULL)
      return()
    }
    commit(project, if (length(project$modules)) project$modules[[1]]$instance_id else NULL)
    shiny::updateTextInput(session, "project_name", value = project$name)
    shiny::showNotification("Project restored with versioned module state.", type = "message")
  }, ignoreInit = TRUE)

  output$download_project <- shiny::downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "-", state$project$name), ".bioplotblocks.json"),
    content = function(file) bp_save_project(shiny::isolate(state$project), file)
  )

  make_r_download <- function() shiny::downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "-", state$project$name), ".R"),
    content = function(file) writeLines(bp_generate_code(shiny::isolate(state$project), registry, include_setup = TRUE), file, useBytes = TRUE)
  )
  output$download_r <- make_r_download()
  output$download_r_secondary <- make_r_download()

  shiny::observeEvent(input$run_preview, start_preview(), ignoreInit = TRUE)

  shiny::observeEvent(input$cancel_preview, {
    process <- state$preview_process
    if (!is.null(process) && process$is_alive()) {
      process$kill()
      state$preview_process <- NULL
      state$preview_status <- "cancelled"
      state$preview_result <- list(ok = FALSE, error = NULL, warnings = list(), messages = list())
    }
  }, ignoreInit = TRUE)

  shiny::observe({
    shiny::invalidateLater(180, session)
    process <- state$preview_process
    if (is.null(process) || process$is_alive()) return()
    status_path <- state$preview_status_file
    if (!is.null(status_path) && file.exists(status_path)) {
      result <- jsonlite::fromJSON(status_path, simplifyVector = FALSE)
      state$preview_result <- result
      state$preview_status <- if (isTRUE(result$ok) && file.exists(state$preview_image)) "success" else "error"
    } else {
      stderr <- tryCatch(process$read_all_error(), error = function(error) "")
      state$preview_result <- list(ok = FALSE, error = if (nzchar(stderr)) stderr else "The R preview process exited without a result.", warnings = list(), messages = list())
      state$preview_status <- "error"
    }
    state$preview_process <- NULL
  })

  session$onFlushed(function() start_preview(), once = TRUE)
  session$onSessionEnded(function() {
    process <- shiny::isolate(state$preview_process)
    if (!is.null(process) && process$is_alive()) process$kill()
  })
}
