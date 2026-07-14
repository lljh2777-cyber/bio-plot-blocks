bp_clone_project <- function(project) {
  unserialize(serialize(project, NULL))
}

bp_find_instance_index <- function(project, instance_id) {
  ids <- vapply(project$modules %||% list(), `[[`, character(1), "instance_id")
  match(instance_id, ids)
}

bp_new_scatter_project <- function(registry) {
  bp_ggplot_only_project(registry)
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
  search_text <- tolower(paste(spec$symbol, spec$presentation$title, spec$presentation$summary, spec$package))
  htmltools::tags$button(
    type = "button",
    role = "menuitem",
    class = paste("bp-library-row bp-add-module", if (nested) "bp-library-row-nested"),
    `data-module-id` = spec$id,
    `data-search-text` = search_text,
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
    role = "menuitem",
    class = "bp-library-row bp-template-row bp-load-template",
    `data-template-id` = template$id,
    `data-search-text` = tolower(paste(template$title, template$display_title %||% template$title, template$description, "ggplot2")),
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

bp_module_picker_group <- function(label, value, specs = list(), templates = list()) {
  trigger_id <- paste0("bp-picker-trigger-", value)
  menu_id <- paste0("bp-picker-menu-", value)
  items <- if (identical(value, "templates")) templates else specs
  option_label <- paste(length(items), if (length(items) == 1L) "option" else "options")

  htmltools::tags$div(
    class = "bp-picker-group",
    `data-picker-group` = value,
    htmltools::tags$button(
      id = trigger_id,
      type = "button",
      class = paste("bp-picker-trigger", paste0("bp-picker-trigger-", value)),
      `data-picker-target` = value,
      `aria-controls` = menu_id,
      `aria-expanded` = "false",
      `aria-haspopup` = "menu",
      htmltools::tags$span(class = paste("bp-picker-dot", paste0("bp-category-", value))),
      htmltools::tags$span(label),
      bp_icon("chevron_down", 13)
    ),
    htmltools::tags$div(
      id = menu_id,
      class = "bp-picker-menu",
      role = "menu",
      `aria-labelledby` = trigger_id,
      hidden = "hidden",
      htmltools::tags$div(
        class = "bp-picker-menu-header",
        htmltools::tags$div(
          htmltools::tags$strong(if (identical(value, "all")) "All modules" else label),
          htmltools::tags$span(option_label)
        ),
        if (identical(value, "all")) {
          htmltools::tags$label(
            class = "bp-picker-search",
            htmltools::tags$span(class = "bp-search-icon", bp_icon("search", 15)),
            htmltools::tags$input(
              id = "module_search",
              type = "search",
              placeholder = "Search functions",
              autocomplete = "off",
              `aria-label` = "Search all modules"
            )
          )
        }
      ),
      htmltools::tags$div(
        class = "bp-library-list bp-picker-list",
        if (identical(value, "templates")) lapply(templates, bp_template_row) else lapply(specs, bp_library_row),
        htmltools::tags$div(class = "bp-picker-empty", hidden = "hidden", "No matching modules")
      )
    )
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

bp_argument_input_value <- function(argument, parameter) {
  value <- argument$value
  if (is.null(value)) value <- parameter$formal_default
  bp_value_to_input_text(value, parameter$ui_control %||% "expression")
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

bp_value_control <- function(instance_id, name, argument, parameter, data_source_suggestions = character()) {
  control <- parameter$ui_control %||% "expression"
  value <- bp_argument_input_value(argument, parameter)
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
  if (identical(control, "data_reference")) {
    list_id <- paste("bp-data-sources", instance_id, name, sep = "-")
    input_id <- paste0(list_id, "-input")
    return(htmltools::tags$div(
      class = "bp-param-hybrid-control",
      do.call(htmltools::tags$input, c(common_attrs, list(
        id = input_id,
        type = "text",
        value = value,
        list = list_id,
        placeholder = if (identical(argument$state, "unset")) "data object or R expression" else NULL,
        autocomplete = "off",
        title = "Choose a registered data source or type an R expression",
        spellcheck = "false"
      ))),
      htmltools::tags$button(
        type = "button",
        class = "bp-aes-suggestion-button bp-data-suggestion-button",
        `data-aes-input-id` = input_id,
        title = "Choose from registered data sources",
        `aria-label` = "Choose data from registered data sources",
        bp_icon("chevron_down", 12)
      ),
      htmltools::tags$datalist(
        id = list_id,
        lapply(seq_along(data_source_suggestions), function(index) htmltools::tags$option(
          value = unname(data_source_suggestions[[index]]),
          label = names(data_source_suggestions)[[index]]
        ))
      )
    ))
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

bp_aes_editor <- function(instance_id, name, argument, column_suggestions = character()) {
  mapping <- argument$value
  mappings <- if (!is.null(mapping) && identical(bp_value_type(mapping), "RAesMapping")) mapping$mappings %||% list() else list()
  keys <- c("x", "y", "color", "fill", "shape", "size", "alpha", "label", "group")
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
        list_id <- paste("bp-aes-columns", instance_id, name, key, sep = "-")
        htmltools::tags$label(
          class = "bp-aes-field",
          htmltools::tags$span(key),
          htmltools::tags$div(
            class = "bp-aes-hybrid-control",
            htmltools::tags$input(
              id = paste0(list_id, "-input"),
              type = "text",
              class = "bp-aes-value",
              `data-instance-id` = instance_id,
              `data-param` = name,
              `data-aes-key` = key,
              value = if (!is.null(mappings[[key]])) bp_value_to_source(mappings[[key]]) else "",
              list = list_id,
              placeholder = "column or R expression",
              autocomplete = "off",
              title = "Choose a data column or type a column name / R expression",
              spellcheck = "false"
            ),
            htmltools::tags$button(
              type = "button",
              class = "bp-aes-suggestion-button",
              `data-aes-input-id` = paste0(list_id, "-input"),
              title = paste("Choose", key, "from data columns"),
              `aria-label` = paste("Choose", key, "from data columns"),
              bp_icon("chevron_down", 12)
            ),
            htmltools::tags$datalist(
              id = list_id,
              lapply(seq_along(column_suggestions), function(index) htmltools::tags$option(
                value = unname(column_suggestions[[index]]),
                label = names(column_suggestions)[[index]]
              ))
            )
          )
        )
      }),
      htmltools::tags$p(
        class = "bp-aes-suggestion-hint",
        if (length(column_suggestions)) {
          paste0(length(column_suggestions), " columns available · choose a suggestion or type an R expression")
        } else {
          "Type a column name or R expression · column suggestions appear when data is available"
        }
      )
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

bp_parameter_row <- function(instance, parameter, argument, effective_mapping, expression_edit, column_suggestions = character(), data_source_suggestions = character()) {
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
        bp_value_control(instance$instance_id, name, argument, parameter, data_source_suggestions)
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
    bp_aes_editor(instance$instance_id, name, argument, column_suggestions)
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

bp_data_import_modal <- function() {
  shiny::modalDialog(
    title = "Import Data / 导入数据",
    size = "l",
    easyClose = FALSE,
    htmltools::tags$p(class = "bp-modal-note", "Supported: CSV, TSV, TXT, RDS, RData, and rda. Files are read-only; R objects are inspected against a safe type whitelist."),
    htmltools::tags$div(
      class = "bp-data-import-layout",
      htmltools::tags$section(
        class = "bp-import-settings",
        htmltools::tags$h3("1. Select and parse"),
        shiny::fileInput("data_file", "Data file", accept = c("text/csv", "text/tab-separated-values", ".csv", ".tsv", ".txt", ".rds", ".RData", ".rda")),
        htmltools::tags$details(
          class = "bp-import-options",
          htmltools::tags$summary("Text parsing options"),
          shiny::selectInput("data_delimiter", "Delimiter", choices = c("Auto" = "auto", "Comma" = "comma", "Tab" = "tab", "Semicolon" = "semicolon", "Pipe" = "pipe")),
          shiny::selectInput("data_encoding", "Encoding", choices = c("UTF-8", "UTF-8-BOM" = "UTF-8-BOM", "GB18030", "Latin-1" = "latin1")),
          shiny::checkboxInput("data_header", "First row contains column names", value = TRUE),
          shiny::textInput("data_na_values", "Missing values (comma separated)", value = ",NA,N/A,null,NULL"),
          shiny::selectInput("data_quote", "Quote character", choices = c('Double quote (")' = '"', "Single quote (')" = "'", "None" = "")),
          shiny::selectInput("data_decimal", "Decimal mark", choices = c("Dot" = ".", "Comma" = ",")),
          shiny::numericInput("data_skip", "Rows to skip", value = 0, min = 0, step = 1),
          shiny::actionButton("analyze_data_file", "Re-analyze file", class = "bp-command-button")
        )
      ),
      htmltools::tags$section(class = "bp-import-results", shiny::uiOutput("data_import_results"))
    ),
    footer = htmltools::tagList(
      shiny::actionButton("cancel_data_import", "Cancel", class = "bp-command-button"),
      shiny::actionButton("register_data_source", "Register / use data", class = "bp-command-primary")
    )
  )
}

bp_data_preview_table <- function(data, rows = 30L, columns = 12L, row_numbers = FALSE) {
  shown <- utils::head(data, rows)
  row_labels <- row.names(shown)
  shown <- shown[, seq_len(min(ncol(shown), columns)), drop = FALSE]
  header_cells <- lapply(names(shown), htmltools::tags$th)
  if (isTRUE(row_numbers)) {
    header_cells <- c(list(htmltools::tags$th(class = "bp-data-row-number", scope = "col", "#")), header_cells)
  }
  htmltools::tags$div(
    class = "bp-data-preview-scroll",
    htmltools::tags$table(
      class = "bp-data-preview-table",
      htmltools::tags$thead(htmltools::tags$tr(header_cells)),
      htmltools::tags$tbody(lapply(seq_len(nrow(shown)), function(row) {
        cells <- lapply(shown[row, , drop = FALSE], function(value) {
          text <- if (length(value) == 0L || is.na(value[[1]])) "NA" else as.character(value[[1]])
          htmltools::tags$td(title = text, text)
        })
        if (isTRUE(row_numbers)) {
          cells <- c(list(htmltools::tags$th(class = "bp-data-row-number", scope = "row", row_labels[[row]])), cells)
        }
        htmltools::tags$tr(cells)
      }))
    )
  )
}

bp_data_column_table <- function(profile) {
  type_choices <- c("numeric", "integer", "character", "logical", "factor", "date", "datetime")
  htmltools::tags$div(
    class = "bp-column-profile-scroll",
    htmltools::tags$table(
      class = "bp-column-profile-table",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Column"), htmltools::tags$th("Detected / override"),
        htmltools::tags$th("Missing"), htmltools::tags$th("Unique"), htmltools::tags$th("Suggested use")
      )),
      htmltools::tags$tbody(lapply(seq_along(profile$column_metadata), function(index) {
        column <- profile$column_metadata[[index]]
        htmltools::tags$tr(
          htmltools::tags$td(htmltools::tags$code(column$name)),
          htmltools::tags$td(shiny::selectInput(
            paste0("data_type_", index), label = NULL, choices = type_choices,
            selected = column$recommended_type, width = "132px"
          )),
          htmltools::tags$td(format(column$missing_count, big.mark = ",")),
          htmltools::tags$td(format(column$unique_count, big.mark = ",")),
          htmltools::tags$td(paste(column$valid_for, collapse = ", "))
        )
      }))
    )
  )
}

bp_data_mapping_controls <- function(data_import, project) {
  columns <- names(data_import$data)
  choices <- c("Not mapped" = "", stats::setNames(columns, columns))
  metadata <- data_import$profile$column_metadata
  numeric <- vapply(metadata, function(column) column$recommended_type %in% c("numeric", "integer"), logical(1))
  categorical <- vapply(metadata, function(column) "color" %in% column$valid_for, logical(1))
  defaults <- list(
    x = if (any(numeric)) columns[which(numeric)[1]] else columns[[1]],
    y = if (sum(numeric) >= 2L) columns[which(numeric)[2]] else if (length(columns) >= 2L) columns[[2]] else columns[[1]],
    color = if (any(categorical)) columns[which(categorical)[1]] else "",
    fill = "", shape = "", size = "", alpha = "", label = "", group = ""
  )
  relink_id <- data_import$relink_id %||% NULL
  existing <- project$mapping_config %||% list()
  if (!is.null(relink_id) && identical(existing$dataset_id, relink_id)) {
    defaults[names(existing$mapping %||% list())] <- existing$mapping
  }
  htmltools::tags$div(
    class = "bp-mapping-grid",
    lapply(names(defaults), function(role) shiny::selectizeInput(
      paste0("data_map_", role), toupper(sub("^.", substr(role, 1, 1), role)),
      choices = choices, selected = defaults[[role]] %||% "",
      options = list(
        dropdownParent = "body",
        dropdownClass = "selectize-dropdown bp-mapping-dropdown",
        plugins = list("auto_position")
      )
    ))
  )
}

bp_r_object_browser <- function(imported) {
  metadata <- imported$metadata %||% list()
  supported <- Filter(function(item) isTRUE(item$supported), metadata)
  unsupported <- Filter(function(item) !isTRUE(item$supported), metadata)
  object_label <- function(item) htmltools::tags$div(
    class = paste("bp-r-object-card", paste0("is-", item$status)),
    htmltools::tags$div(
      htmltools::tags$strong(item$name),
      htmltools::tags$span(class = "bp-r-object-status", if (identical(item$status, "ready")) "Ready" else "Convert to data.frame")
    ),
    htmltools::tags$div(
      class = "bp-r-object-meta",
      htmltools::tags$code(item$r_class),
      htmltools::tags$span(if (is.finite(item$rows)) paste0(format(item$rows, big.mark = ","), " × ", format(item$columns, big.mark = ",")) else "No table dimensions"),
      if (isTRUE(item$has_row_names)) htmltools::tags$span("Has row names")
    ),
    htmltools::tags$p(item$message)
  )
  htmltools::tagList(
    htmltools::tags$h3("2. Browse R objects"),
    htmltools::tags$div(
      class = "bp-data-summary-grid",
      htmltools::tags$div(htmltools::tags$span("Format"), htmltools::tags$strong(toupper(imported$format))),
      htmltools::tags$div(htmltools::tags$span("Objects"), htmltools::tags$strong(length(metadata))),
      htmltools::tags$div(htmltools::tags$span("Supported"), htmltools::tags$strong(length(supported))),
      htmltools::tags$div(htmltools::tags$span("File size"), htmltools::tags$strong(sprintf("%.1f MB", imported$file_size / 1024^2)))
    ),
    if (length(supported)) shiny::checkboxGroupInput(
      "r_object_selection", "Select one or more objects to register",
      choiceNames = lapply(supported, object_label),
      choiceValues = vapply(supported, `[[`, character(1), "name"),
      selected = vapply(supported, `[[`, character(1), "name")
    ) else htmltools::tags$div(class = "bp-import-error", "No supported table objects were found."),
    if (length(unsupported)) htmltools::tags$details(
      class = "bp-import-section bp-r-unsupported",
      htmltools::tags$summary(paste(length(unsupported), "unsupported or forbidden objects")),
      lapply(unsupported, object_label)
    ),
    if (any(vapply(supported, function(item) isTRUE(item$requires_conversion) || isTRUE(item$has_row_names), logical(1)))) htmltools::tags$div(
      class = "bp-r-conversion-options",
      htmltools::tags$strong("Matrix / row-name handling"),
      shiny::radioButtons(
        "r_row_names", label = NULL,
        choices = c("Preserve as row names" = "preserve", "Convert to a column" = "column", "Ignore row names" = "ignore"),
        selected = "preserve", inline = TRUE
      ),
      shiny::textInput("r_row_name_column", "Column name when converting row names", value = "RowName")
    ),
    if (length(supported)) htmltools::tags$section(
      class = "bp-r-object-preview",
      htmltools::tags$div(
        class = "bp-r-object-preview-heading",
        htmltools::tags$strong("Preview data object"),
        htmltools::tags$span("Click any available object to inspect its first 30 rows and all columns.")
      ),
      htmltools::tags$div(
        class = "bp-r-preview-picker",
        shiny::radioButtons(
          "r_preview_object", label = NULL,
          choiceNames = lapply(supported, function(item) htmltools::tagList(
            htmltools::tags$code(item$name),
            htmltools::tags$span(paste0(format(item$rows, big.mark = ","), " × ", format(item$columns, big.mark = ",")))
          )),
          choiceValues = vapply(supported, `[[`, character(1), "name"),
          selected = supported[[1]]$name,
          inline = TRUE
        )
      ),
      shiny::uiOutput("r_object_preview")
    ),
    htmltools::tags$div(class = "bp-inline-warning", bp_icon("info", 18), htmltools::tags$span("RData/rda is loaded into an isolated environment. Functions, environments, connections, formulas, external pointers, and unsupported objects are never registered."))
  )
}

bp_data_source_manager_modal <- function() {
  shiny::modalDialog(
    title = "Data Sources / 数据源",
    size = "l",
    easyClose = TRUE,
    htmltools::tags$p(class = "bp-modal-note", "Preview, rename, map, relink, or remove registered sources. Project files store metadata and mappings, not full imported datasets."),
    shiny::uiOutput("data_source_manager_list"),
    footer = htmltools::tagList(
      shiny::modalButton("Close"),
      shiny::actionButton("manager_import_data", "Import another data source", class = "bp-command-primary")
    )
  )
}

bp_data_source_mapping_modal <- function(source, data, project) {
  profile <- bp_profile_dataset(data)
  mapping_import <- list(data = data, profile = profile, relink_id = source$id)
  shiny::modalDialog(
    title = paste0("Map data source: ", source$name),
    size = "l",
    easyClose = FALSE,
    htmltools::tags$p(class = "bp-modal-note", "Choose mappings for this plot. Applying them changes the active plot data source; it does not modify the dataset."),
    htmltools::tags$div(
      class = "bp-data-summary-grid",
      htmltools::tags$div(htmltools::tags$span("Object"), htmltools::tags$strong(source$object_type %||% "data.frame")),
      htmltools::tags$div(htmltools::tags$span("Rows"), htmltools::tags$strong(format(nrow(data), big.mark = ","))),
      htmltools::tags$div(htmltools::tags$span("Columns"), htmltools::tags$strong(ncol(data))),
      htmltools::tags$div(htmltools::tags$span("Source"), htmltools::tags$strong(toupper(source$source_type %||% "data")))
    ),
    htmltools::tags$details(class = "bp-import-section", htmltools::tags$summary("Preview first 12 rows"), bp_data_preview_table(data, rows = 12L)),
    htmltools::tags$div(class = "bp-mapping-title", htmltools::tags$strong("Column mapping"), htmltools::tags$span("X and Y are required; other mappings are optional.")),
    bp_data_mapping_controls(mapping_import, project),
    footer = htmltools::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("apply_data_source_mapping", "Use in plot", class = "bp-command-primary")
    )
  )
}

bp_workspace_server <- function(input, output, session, registry, templates, root) {
  initial <- bp_ggplot_only_project(registry)
  initial$visual_config <- initial$visual_config %||% list()
  initial$visual_config$active_chart_type <- "scatter"
  initial$visual_config$scatter <- bp_visual_scatter_config_from_project(initial)
  initial_volcano_recommendation <- bp_visual_recommend_volcano_fields(bp_example_data_source(), bp_default_environment()$df)
  initial$visual_config$volcano <- bp_normalize_visual_volcano_config(utils::modifyList(
    bp_visual_volcano_defaults(initial),
    initial_volcano_recommendation[c("x_field", "y_field", "color_field", "x_scale", "y_scale")]
  ), initial)
  initial_boxplot_recommendation <- bp_visual_recommend_boxplot_fields(bp_example_data_source(), bp_default_environment()$df)
  initial$visual_config$boxplot <- bp_normalize_visual_boxplot_config(utils::modifyList(
    bp_visual_boxplot_defaults(initial),
    initial_boxplot_recommendation[c("x_field", "y_field", "color_field", "x_scale", "y_scale")]
  ), initial)
  initial$visual_config$pca <- bp_normalize_pca_config(utils::modifyList(
    bp_pca_defaults(initial),
    list(
      expression_source_id = "dataset_example",
      expression_orientation = "genes_by_samples",
      feature_id_location = "column",
      feature_id_field = "gene",
      transform = "none",
      variable_feature_count = "all"
    )
  ), initial)
  selected_initial <- initial$modules[[1]]$instance_id
  state <- shiny::reactiveValues(
    project = initial,
    selected = selected_initial,
    parameter_tab = "common",
    history = list(),
    future = list(),
    expression_edit = NULL,
    preview_process = NULL,
    preview_status = "initial",
    preview_result = NULL,
    preview_image = NULL,
    preview_pending_image = NULL,
    preview_status_file = NULL,
    data_objects = list(),
    data_import = NULL,
    data_preview_source_id = "dataset_example",
    pending_mapping_source_id = NULL,
    pending_rename_source_id = NULL,
    last_data_switch = NULL,
    interface_mode = "visual",
    visual_syncing = FALSE,
    visual_sync_generation = 0L,
    visual_suppress_debounced = FALSE,
    visual_input_error = NULL,
    last_commit_origin = NULL
  )

  commit <- function(project, selected = state$selected, record_history = TRUE, origin = "advanced") {
    current <- shiny::isolate(state$project)
    if (isTRUE(record_history)) {
      history <- c(shiny::isolate(state$history), list(bp_clone_project(current)))
      if (length(history) > 60L) history <- tail(history, 60L)
      state$history <- history
      state$future <- list()
    }
    project$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    state$last_commit_origin <- origin
    state$project <- project
    ids <- vapply(project$modules %||% list(), `[[`, character(1), "instance_id")
    state$selected <- if (length(ids) && selected %in% ids) selected else if (length(ids)) ids[[1]] else NULL
  }

  data_source_object <- function(source) {
    if (isTRUE(source$example)) return(bp_default_environment()$df)
    shiny::isolate(state$data_objects[[source$id]])
  }

  switch_registered_data_source <- function(project, source, mapping_override = NULL) {
    data <- data_source_object(source)
    if (identical(source$status, "relink_required") || isTRUE(source$relink_required) || is.null(data)) {
      stop("This data source must be relinked before it can be used in the plot.", call. = FALSE)
    }
    bp_switch_project_data_source(project, source, data = data, mapping_override = mapping_override)
  }

  notify_data_source_switch <- function(result) {
    cleared <- result$cleared %||% list()
    summary <- if (length(cleared)) {
      paste0(
        result$preserved_count, " valid mapping", if (result$preserved_count == 1L) " was" else "s were",
        " preserved; ", result$cleared_count, " unavailable mapping", if (result$cleared_count == 1L) " was" else "s were", " cleared."
      )
    } else NULL
    detail <- if (length(cleared)) {
      item <- cleared[[1]]
      paste0(toupper(item$key), " mapping \"", item$column %||% item$source, "\" was cleared because the selected data source does not contain this column.")
    } else NULL
    shiny::showNotification(
      htmltools::tagList(
        htmltools::tags$div(paste0("Data source changed to ", result$source$name, ".")),
        if (!is.null(summary)) htmltools::tags$div(summary),
        if (!is.null(detail)) htmltools::tags$div(class = "bp-notification-detail", detail),
        if (length(cleared)) htmltools::tags$div(paste0("数据源已切换为 ", result$source$name, "；保留 ", result$preserved_count, " 项映射，清除 ", result$cleared_count, " 项不可用映射。"))
      ),
      type = if (length(cleared)) "warning" else "message",
      duration = 8
    )
  }

  notify_custom_data_expression <- function() {
    shiny::showNotification(
      htmltools::tagList(
        htmltools::tags$div("Custom R data expression detected. Existing mappings were preserved because the resulting columns cannot be determined safely."),
        htmltools::tags$div("已检测到自定义 R 数据表达式；由于无法安全确定输出列，已保留现有映射，请使用 Run preview 验证。")
      ),
      type = "warning", duration = 8
    )
  }

  pca_source_data <- function(source_id, project = state$project) {
    if (!nzchar(source_id %||% "")) return(NULL)
    source <- bp_pca_source(project, source_id)
    if (is.null(source) || identical(source$status, "relink_required") || isTRUE(source$relink_required)) return(NULL)
    if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source_id]]
  }

  compute_pca_for_config <- function(config = bp_pca_config_from_project(state$project), project = state$project) {
    config <- bp_normalize_pca_config(config, project)
    expression_source <- bp_pca_source(project, config$expression_source_id)
    if (is.null(expression_source)) {
      return(list(ok = FALSE, error = "PCA 表达数据源未注册。", diagnostics = list(), warnings = character()))
    }
    effective_semantic <- bp_data_source_effective_semantic(expression_source, pca_source_data(config$expression_source_id, project))
    config$input_semantic_type <- if (!isTRUE(expression_source$semantic_confirmed) && identical(effective_semantic, "raw_counts")) {
      "unconfirmed_raw_counts"
    } else {
      effective_semantic
    }
    expression_data <- pca_source_data(config$expression_source_id, project)
    if (is.null(expression_data)) {
      return(list(ok = FALSE, error = "PCA 表达数据源不可用；请重新导入或链接该数据。", diagnostics = list(), warnings = character()))
    }
    metadata_data <- if (nzchar(config$metadata_source_id)) pca_source_data(config$metadata_source_id, project) else NULL
    if (nzchar(config$metadata_source_id) && is.null(metadata_data)) {
      return(list(ok = FALSE, error = "PCA 样本信息数据源不可用；请重新导入或链接该数据。", diagnostics = list(), warnings = character()))
    }
    bp_compute_pca(expression_data, metadata_data, config)
  }

  prepare_pca_config <- function(config, project = state$project) {
    config <- bp_normalize_pca_config(config, project)
    result <- compute_pca_for_config(config, project)
    if (!isTRUE(result$ok)) return(list(config = config, result = result, cleared = character()))
    columns <- names(result$scores)
    components <- columns[grepl("^PC[0-9]+$", columns)]
    first_or <- function(values, fallback) if (length(values)) values[[1]] else fallback
    if (!config$x_component %in% components) config$x_component <- first_or(components, "PC1")
    if (!config$y_component %in% components || identical(config$y_component, config$x_component)) {
      config$y_component <- first_or(setdiff(components, config$x_component), config$x_component)
    }
    cleared <- character()
    for (field in c("color_field", "shape_field", "label_field")) {
      if (nzchar(config[[field]]) && !config[[field]] %in% columns) {
        cleared <- c(cleared, config[[field]])
        config[[field]] <- ""
      }
    }
    list(config = config, result = result, cleared = cleared)
  }

  pca_result <- shiny::reactive({
    if (!identical(bp_visual_chart_type(state$project), "pca")) return(NULL)
    compute_pca_for_config(bp_pca_config_from_project(state$project), state$project)
  })

  cache_pca_result <- function(result, project = NULL) {
    if (is.null(result) || !isTRUE(result$ok)) return(FALSE)
    objects <- shiny::isolate(state$data_objects)
    objects[["dataset_pca_scores"]] <- result$scores
    objects[["dataset_pca_loadings"]] <- result$loadings
    if (is.data.frame(result$normalized_expression)) {
      objects[["dataset_normalized_expression"]] <- result$normalized_expression
    } else {
      objects[["dataset_normalized_expression"]] <- NULL
    }
    state$data_objects <- objects
    state$data_preview_source_id <- "dataset_pca_scores"
    TRUE
  }

  start_preview <- function() {
    process <- shiny::isolate(state$preview_process)
    if (!is.null(process) && process$is_alive()) process$kill()
    status_path <- tempfile("bioplotblocks-preview-", fileext = ".json")
    image_path <- tempfile("bioplotblocks-preview-", fileext = ".png")
    state$preview_status <- "running"
    state$preview_result <- NULL
    state$preview_status_file <- status_path
    state$preview_pending_image <- image_path
    if (identical(bp_visual_chart_type(shiny::isolate(state$project)), "pca")) {
      result <- shiny::isolate(compute_pca_for_config(bp_pca_config_from_project(state$project), state$project))
      if (!isTRUE(result$ok)) {
        state$preview_status <- "error"
        state$preview_result <- list(ok = FALSE, error = result$error %||% "PCA 计算失败。", warnings = result$warnings %||% list(), messages = list())
        state$preview_pending_image <- NULL
        return()
      }
      cache_pca_result(result)
    }
    active_id <- shiny::isolate(state$project$active_data_source_id %||% "dataset_example")
    active_sources <- Filter(function(source) identical(source$id, active_id), shiny::isolate(state$project$data_sources %||% list()))
    if (length(active_sources) && !isTRUE(active_sources[[1]]$example) && is.null(shiny::isolate(state$data_objects[[active_id]]))) {
      state$preview_status <- "error"
      state$preview_result <- list(
        ok = FALSE,
        error = paste0("Data source '", active_sources[[1]]$name, "' must be re-linked by importing ", active_sources[[1]]$original_file_name, " again."),
        warnings = list(), messages = list()
      )
      state$preview_pending_image <- NULL
      return()
    }
    datasets <- bp_runtime_dataset_values(shiny::isolate(state$project), shiny::isolate(state$data_objects))
    process <- tryCatch(
      bp_start_preview_process(shiny::isolate(state$project), root, status_path, image_path, datasets),
      error = identity
    )
    if (inherits(process, "error")) {
      state$preview_status <- "error"
      state$preview_result <- list(ok = FALSE, error = conditionMessage(process), warnings = list(), messages = list())
      state$preview_process <- NULL
      state$preview_pending_image <- NULL
    } else {
      state$preview_process <- process
    }
  }

  visual_source <- function(project = shiny::isolate(state$project)) {
    active_id <- project$active_data_source_id %||% "dataset_example"
    sources <- Filter(function(source) identical(source$id, active_id), project$data_sources %||% list())
    if (length(sources)) sources[[1]] else if (identical(active_id, "dataset_example")) bp_example_data_source() else NULL
  }

  visual_source_data <- function(source = visual_source()) {
    if (is.null(source)) return(NULL)
    if (isTRUE(source$derived) && identical(source$source_type, "derived_pca")) {
      result <- pca_result()
      if (is.null(result) || !isTRUE(result$ok)) return(NULL)
      return(result[[source$derived_kind %||% "scores"]])
    }
    if (isTRUE(source$example)) bp_default_environment()$df else shiny::isolate(state$data_objects[[source$id]])
  }

  semantic_focus_source <- function(project = state$project) {
    if (identical(bp_visual_chart_type(project), "pca")) {
      config <- bp_pca_config_from_project(project)
      return(bp_pca_source(project, config$expression_source_id))
    }
    visual_source(project)
  }

  semantic_focus_data <- function(source = semantic_focus_source()) {
    if (is.null(source)) return(NULL)
    if (isTRUE(source$example)) return(bp_default_environment()$df)
    state$data_objects[[source$id]]
  }

  semantic_label <- function(value) {
    bp_data_semantic_label(value)
  }

  semantic_display_value <- function(source) {
    if (isTRUE(source$semantic_confirmed) || isTRUE(source$derived) || isTRUE(source$example)) {
      source$semantic_type %||% "generic_table"
    } else {
      source$semantic_suggestion %||% source$semantic_type %||% "generic_table"
    }
  }

  visual_current_columns <- function(project = state$project) {
    if (identical(bp_visual_chart_type(project), "pca")) {
      result <- pca_result()
      return(if (!is.null(result) && isTRUE(result$ok)) names(result$scores) else character())
    }
    source <- visual_source(project)
    if (is.null(source)) return(character())
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source$id]]
    bp_data_source_columns(source, data)
  }

  visual_active_config <- function(project = shiny::isolate(state$project)) {
    bp_visual_config_from_project(project)
  }

  visual_matrix_source_config_from_inputs <- function(project = state$project, expression_source_id = NULL) {
    current <- bp_pca_config_from_project(project)
    feature_field <- input$visual_pca_feature_id_field %||% current$feature_id_field
    sample_field <- input$visual_pca_expression_sample_id_field %||% current$expression_sample_id_field
    current$expression_source_id <- expression_source_id %||% input$visual_pca_expression_source %||% current$expression_source_id
    current$metadata_source_id <- input$visual_pca_metadata_source %||% current$metadata_source_id
    current$expression_orientation <- input$visual_pca_orientation %||% current$expression_orientation
    current$feature_id_location <- if (nzchar(feature_field %||% "")) "column" else "auto"
    current$feature_id_field <- feature_field %||% ""
    current$expression_sample_id_location <- if (nzchar(sample_field %||% "")) "column" else "auto"
    current$expression_sample_id_field <- sample_field %||% ""
    current$metadata_sample_id_field <- input$visual_pca_metadata_id_field %||% current$metadata_sample_id_field
    current$unmatched_sample_policy <- input$visual_pca_unmatched_policy %||% current$unmatched_sample_policy
    bp_normalize_pca_config(current, project)
  }

  visual_label_signature <- function(config) {
    fields <- c("title", "x_label", "y_label", "legend_title")
    vapply(fields, function(field) bp_visual_scalar_character(config[[field]], ""), character(1))
  }

  visual_normalize_config <- function(config, project = shiny::isolate(state$project)) {
    if (identical(config$chart_type %||% "scatter", "volcano")) {
      bp_normalize_visual_volcano_config(config, project)
    } else if (identical(config$chart_type %||% "scatter", "boxplot")) {
      bp_normalize_visual_boxplot_config(config, project)
    } else if (identical(config$chart_type %||% "scatter", "pca")) {
      bp_normalize_pca_config(config, project)
    } else {
      bp_normalize_visual_scatter_config(config, project)
    }
  }

  visual_config_from_inputs <- function() {
    current <- visual_active_config(state$project)
    if (identical(current$chart_type %||% "scatter", "pca")) {
      source_config <- visual_matrix_source_config_from_inputs(state$project)
      feature_choice <- input$visual_pca_feature_count %||% if (identical(current$variable_feature_count, "all")) "all" else as.character(current$variable_feature_count)
      feature_count <- if (identical(feature_choice, "custom")) {
        input$visual_pca_custom_feature_count %||% current$custom_feature_count
      } else if (identical(feature_choice, "all")) {
        "all"
      } else {
        suppressWarnings(as.integer(feature_choice))
      }
      return(list(
        chart_type = "pca",
        expression_source_id = source_config$expression_source_id,
        metadata_source_id = source_config$metadata_source_id,
        expression_orientation = source_config$expression_orientation,
        feature_id_location = source_config$feature_id_location,
        feature_id_field = source_config$feature_id_field,
        expression_sample_id_location = source_config$expression_sample_id_location,
        expression_sample_id_field = source_config$expression_sample_id_field,
        metadata_sample_id_field = source_config$metadata_sample_id_field,
        unmatched_sample_policy = source_config$unmatched_sample_policy,
        input_semantic_type = current$input_semantic_type %||% "generic_table",
        raw_count_filter_cpm = input$visual_pca_filter_cpm %||% current$raw_count_filter_cpm,
        raw_count_filter_min_samples = input$visual_pca_filter_min_samples %||% current$raw_count_filter_min_samples,
        raw_count_normalization = input$visual_pca_normalization %||% current$raw_count_normalization,
        raw_count_prior_count = input$visual_pca_prior_count %||% current$raw_count_prior_count,
        raw_count_recipe_confirmed_signature = current$raw_count_recipe_confirmed_signature %||% "",
        transform = input$visual_pca_transform %||% current$transform,
        variable_feature_count = feature_count,
        custom_feature_count = input$visual_pca_custom_feature_count %||% current$custom_feature_count,
        remove_zero_variance = if (is.null(input$visual_pca_remove_zero_variance)) isTRUE(current$remove_zero_variance) else isTRUE(input$visual_pca_remove_zero_variance),
        missing_value_policy = input$visual_pca_missing_policy %||% current$missing_value_policy,
        center = if (is.null(input$visual_pca_center)) isTRUE(current$center) else isTRUE(input$visual_pca_center),
        scale = if (is.null(input$visual_pca_scale)) isTRUE(current$scale) else isTRUE(input$visual_pca_scale),
        x_component = input$visual_pca_x_component %||% current$x_component,
        y_component = input$visual_pca_y_component %||% current$y_component,
        color_field = input$visual_pca_color %||% current$color_field,
        shape_field = input$visual_pca_shape %||% current$shape_field,
        label_field = input$visual_pca_label %||% current$label_field,
        point_color = input$visual_point_color %||% current$point_color,
        point_size = input$visual_point_size %||% current$point_size,
        alpha = input$visual_alpha %||% current$alpha,
        shape = input$visual_shape %||% current$shape,
        palette = input$visual_palette %||% current$palette,
        show_ellipse = if (is.null(input$visual_pca_show_ellipse)) isTRUE(current$show_ellipse) else isTRUE(input$visual_pca_show_ellipse),
        ellipse_level = input$visual_pca_ellipse_level %||% current$ellipse_level,
        title = input$visual_title %||% current$title,
        legend_title = input$visual_legend_title %||% current$legend_title,
        theme = input$visual_theme %||% current$theme,
        base_size = input$visual_base_size %||% current$base_size,
        advanced_preserved = isTRUE(current$advanced_preserved)
      ))
    }
    list(
      chart_type = current$chart_type %||% "scatter",
      data_source_id = state$project$active_data_source_id %||% "dataset_example",
      x_field = input$visual_x %||% current$x_field,
      y_field = input$visual_y %||% current$y_field,
      color_field = input$visual_color %||% current$color_field,
      size_field = input$visual_size %||% current$size_field,
      label_field = input$visual_label %||% current$label_field,
      point_color = input$visual_point_color %||% current$point_color,
      point_size = input$visual_point_size %||% current$point_size,
      alpha = input$visual_alpha %||% current$alpha,
      shape = input$visual_shape %||% current$shape,
      palette = input$visual_palette %||% current$palette,
      trend_line = input$visual_trend %||% current$trend_line,
      title = input$visual_title %||% current$title,
      x_label = input$visual_x_label %||% current$x_label,
      y_label = input$visual_y_label %||% current$y_label,
      legend_title = input$visual_legend_title %||% current$legend_title,
      x_scale = input$visual_x_scale %||% current$x_scale,
      y_scale = input$visual_y_scale %||% current$y_scale,
      theme = input$visual_theme %||% current$theme,
      base_size = input$visual_base_size %||% current$base_size,
      vertical_reference_lines = input$visual_vlines %||% current$vertical_reference_lines %||% "",
      horizontal_reference_lines = input$visual_hlines %||% current$horizontal_reference_lines %||% "",
      reference_line_color = input$visual_reference_color %||% current$reference_line_color %||% "#6B7280",
      reference_line_width = input$visual_reference_width %||% current$reference_line_width %||% 0.6,
      fold_change_cutoff = input$visual_fc_cutoff %||% current$fold_change_cutoff %||% 1,
      significance_cutoff = input$visual_p_cutoff %||% current$significance_cutoff %||% 0.05,
      auto_status = if (is.null(input$visual_auto_status)) isTRUE(current$auto_status %||% TRUE) else isTRUE(input$visual_auto_status),
      box_border_color = input$visual_box_border_color %||% current$box_border_color %||% "#334155",
      box_show_outliers = if (is.null(input$visual_box_show_outliers)) isTRUE(current$box_show_outliers %||% TRUE) else isTRUE(input$visual_box_show_outliers),
      box_outlier_restore = isTRUE(current$box_outlier_restore %||% current$box_show_outliers %||% TRUE),
      box_outlier_size = input$visual_box_outlier_size %||% current$box_outlier_size %||% 1.5,
      box_jitter = if (is.null(input$visual_box_jitter)) isTRUE(current$box_jitter) else isTRUE(input$visual_box_jitter),
      box_jitter_color = input$visual_box_jitter_color %||% current$box_jitter_color %||% "#334155",
      box_jitter_size = input$visual_box_jitter_size %||% current$box_jitter_size %||% 1.4,
      box_jitter_alpha = input$visual_box_jitter_alpha %||% current$box_jitter_alpha %||% 0.55,
      box_jitter_width = input$visual_box_jitter_width %||% current$box_jitter_width %||% 0.16,
      advanced_preserved = isTRUE(current$advanced_preserved)
    )
  }

  visual_validation <- function(config = visual_config_from_inputs()) {
    validation <- bp_validate_visual_config(config, visual_current_columns())
    if (identical(config$chart_type %||% "scatter", "pca")) {
      result <- compute_pca_for_config(config, state$project)
      if (!isTRUE(result$ok)) {
        validation$valid <- FALSE
        validation$errors <- unique(c(validation$errors, result$error %||% "PCA 计算失败。"))
      }
      return(validation)
    }
    source <- visual_source()
    if (is.null(source) || identical(source$status, "relink_required") || isTRUE(source$relink_required)) {
      validation$valid <- FALSE
      validation$errors <- unique(c(validation$errors, "当前数据源需要重新链接后才能生成预览。"))
    }
    color <- trimws(config$point_color %||% "")
    if (!grepl("^#[0-9A-Fa-f]{6}$", color)) {
      validation$valid <- FALSE
      validation$errors <- unique(c(validation$errors, "点颜色需使用 6 位十六进制颜色，例如 #2C7FB8。"))
    }
    validation
  }

  sync_visual_inputs <- function(project = shiny::isolate(state$project), config = NULL) {
    config <- visual_normalize_config(config %||% bp_visual_config_from_project(project), project)
    state$visual_syncing <- TRUE
    generation <- shiny::isolate(state$visual_sync_generation) + 1L
    state$visual_sync_generation <- generation
    state$visual_suppress_debounced <- TRUE
    later::later(function() {
      if (identical(shiny::isolate(state$visual_sync_generation), generation)) state$visual_suppress_debounced <- FALSE
    }, 0.75)
    shiny::updateRadioButtons(
      session, "visual_workflow_mode",
      selected = project$analysis_workflow_mode %||% "generic"
    )
    session$sendCustomMessage(
      "bp_visual_workflow_mode",
      list(value = project$analysis_workflow_mode %||% "generic")
    )
    session$sendCustomMessage("bp_visual_chart_type", list(value = config$chart_type))
    sources <- project$data_sources %||% list(bp_example_data_source())
    is_pca <- identical(config$chart_type, "pca")
    source_config <- if (is_pca) config else bp_pca_config_from_project(project)
    if (!is_pca) source_config$expression_source_id <- project$active_data_source_id %||% config$data_source_id %||% "dataset_example"
    expression_sources <- if (is_pca) Filter(function(source) !isTRUE(source$derived), sources) else sources
    metadata_sources <- Filter(function(source) !isTRUE(source$derived), sources)
    source_choices <- function(items) {
      values <- vapply(items, function(source) source$id %||% "", character(1))
      labels <- vapply(items, function(source) paste0(
        source$name %||% "data", " · ", toupper(source$source_type %||% "DATA"), " · ",
        semantic_label(semantic_display_value(source)), " · ",
        if (identical(source$status, "relink_required") || isTRUE(source$relink_required)) "需要重新链接" else "可用"
      ), character(1))
      keep <- nzchar(values)
      stats::setNames(values[keep], labels[keep])
    }
    expression_choices <- source_choices(expression_sources)
    metadata_source_choices <- source_choices(metadata_sources)
    shiny::updateSelectInput(
      session, "visual_pca_expression_source",
      choices = expression_choices, selected = source_config$expression_source_id
    )
    shiny::updateSelectInput(
      session, "visual_pca_metadata_source",
      choices = c("不使用样本信息" = "", metadata_source_choices),
      selected = source_config$metadata_source_id
    )
    shiny::updateSelectInput(session, "visual_pca_orientation", selected = source_config$expression_orientation)
    shiny::updateSelectInput(session, "visual_pca_unmatched_policy", selected = source_config$unmatched_sample_policy)

    expression_data <- pca_source_data(source_config$expression_source_id, project)
    expression_columns <- if (is.data.frame(expression_data) || is.matrix(expression_data)) colnames(expression_data) %||% character() else character()
    expression_field_choices <- stats::setNames(c("", expression_columns), c("自动 / 行名", expression_columns))
    shiny::updateSelectizeInput(session, "visual_pca_feature_id_field", choices = expression_field_choices, selected = source_config$feature_id_field, server = TRUE)
    shiny::updateSelectizeInput(session, "visual_pca_expression_sample_id_field", choices = expression_field_choices, selected = source_config$expression_sample_id_field, server = TRUE)

    metadata_data <- if (nzchar(source_config$metadata_source_id)) pca_source_data(source_config$metadata_source_id, project) else NULL
    metadata_columns <- if (is.data.frame(metadata_data) || is.matrix(metadata_data)) colnames(metadata_data) %||% character() else character()
    metadata_choices <- stats::setNames(c("", metadata_columns), c("自动识别", metadata_columns))
    shiny::updateSelectizeInput(session, "visual_pca_metadata_id_field", choices = metadata_choices, selected = source_config$metadata_sample_id_field, server = TRUE)

    if (is_pca) {

      result <- compute_pca_for_config(config, project)
      score_columns <- if (isTRUE(result$ok)) names(result$scores) else character()
      components <- score_columns[grepl("^PC[0-9]+$", score_columns)]
      if (!length(components)) components <- c("PC1", "PC2")
      x_selected <- if (config$x_component %in% components) config$x_component else components[[1]]
      y_selected <- if (config$y_component %in% components) config$y_component else components[[min(2L, length(components))]]
      shiny::updateSelectInput(session, "visual_pca_x_component", choices = components, selected = x_selected)
      shiny::updateSelectInput(session, "visual_pca_y_component", choices = components, selected = y_selected)
      mapped_columns <- setdiff(score_columns, components)
      mapped_choices <- stats::setNames(c("", mapped_columns), c("不选择", mapped_columns))
      shiny::updateSelectizeInput(session, "visual_pca_color", choices = mapped_choices, selected = config$color_field, server = TRUE)
      shiny::updateSelectizeInput(session, "visual_pca_shape", choices = mapped_choices, selected = config$shape_field, server = TRUE)
      shiny::updateSelectizeInput(session, "visual_pca_label", choices = mapped_choices, selected = config$label_field, server = TRUE)

      feature_selected <- if (identical(config$variable_feature_count, "all")) {
        "all"
      } else if (as.character(config$variable_feature_count) %in% c("500", "1000", "2000")) {
        as.character(config$variable_feature_count)
      } else {
        "custom"
      }
      shiny::updateSelectInput(session, "visual_pca_transform", selected = config$transform)
      shiny::updateNumericInput(session, "visual_pca_filter_cpm", value = config$raw_count_filter_cpm)
      shiny::updateNumericInput(session, "visual_pca_filter_min_samples", value = config$raw_count_filter_min_samples)
      shiny::updateSelectInput(session, "visual_pca_normalization", selected = config$raw_count_normalization)
      shiny::updateNumericInput(session, "visual_pca_prior_count", value = config$raw_count_prior_count)
      shiny::updateSelectInput(session, "visual_pca_feature_count", selected = feature_selected)
      shiny::updateNumericInput(session, "visual_pca_custom_feature_count", value = config$custom_feature_count)
      shiny::updateSelectInput(session, "visual_pca_missing_policy", selected = config$missing_value_policy)
      shiny::updateCheckboxInput(session, "visual_pca_remove_zero_variance", value = isTRUE(config$remove_zero_variance))
      shiny::updateCheckboxInput(session, "visual_pca_center", value = isTRUE(config$center))
      shiny::updateCheckboxInput(session, "visual_pca_scale", value = isTRUE(config$scale))
      shiny::updateCheckboxInput(session, "visual_pca_show_ellipse", value = isTRUE(config$show_ellipse))
      shiny::updateNumericInput(session, "visual_pca_ellipse_level", value = config$ellipse_level)
      shiny::updateTextInput(session, "visual_point_color", value = config$point_color)
      shiny::updateNumericInput(session, "visual_point_size", value = config$point_size)
      shiny::updateNumericInput(session, "visual_alpha", value = config$alpha)
      shiny::updateSelectInput(session, "visual_shape", selected = config$shape)
      shiny::updateSelectInput(session, "visual_palette", selected = config$palette)
      shiny::updateSelectInput(session, "visual_theme", selected = config$theme)
      shiny::updateNumericInput(session, "visual_base_size", value = config$base_size)
      shiny::updateTextInput(session, "visual_title", value = config$title)
      shiny::updateTextInput(session, "visual_legend_title", value = config$legend_title)
      later::later(function() state$visual_syncing <- FALSE, 0.35)
      return(invisible(config))
    }
    source <- visual_source(project)
    data <- if (is.null(source)) NULL else if (isTRUE(source$example)) bp_default_environment()$df else shiny::isolate(state$data_objects[[source$id]])
    profile <- if (is.null(source)) data.frame(name = character(), type = character()) else bp_visual_column_profile(source, data)
    field_values <- c("", profile$name)
    field_labels <- c("不选择", paste0(profile$name, " · ", profile$type))
    field_choices <- stats::setNames(field_values, field_labels)
    update_field <- function(id, selected) {
      shiny::updateSelectizeInput(session, id, choices = field_choices, selected = selected %||% "", server = TRUE)
    }
    update_field("visual_x", config$x_field)
    update_field("visual_y", config$y_field)
    update_field("visual_color", config$color_field)
    update_field("visual_size", config$size_field)
    update_field("visual_label", config$label_field)
    shiny::updateTextInput(session, "visual_point_color", value = config$point_color)
    shiny::updateNumericInput(session, "visual_point_size", value = config$point_size)
    shiny::updateNumericInput(session, "visual_alpha", value = config$alpha)
    shiny::updateSelectInput(session, "visual_shape", selected = config$shape)
    shiny::updateSelectInput(session, "visual_palette", selected = config$palette)
    shiny::updateSelectInput(session, "visual_trend", selected = config$trend_line)
    shiny::updateSelectInput(session, "visual_theme", selected = config$theme)
    shiny::updateNumericInput(session, "visual_base_size", value = config$base_size)
    shiny::updateSelectInput(session, "visual_x_scale", selected = config$x_scale)
    shiny::updateSelectInput(session, "visual_y_scale", selected = config$y_scale)
    shiny::updateTextInput(session, "visual_title", value = config$title)
    shiny::updateTextInput(session, "visual_x_label", value = config$x_label)
    shiny::updateTextInput(session, "visual_y_label", value = config$y_label)
    shiny::updateTextInput(session, "visual_legend_title", value = config$legend_title)
    shiny::updateTextInput(session, "visual_vlines", value = config$vertical_reference_lines %||% "")
    shiny::updateTextInput(session, "visual_hlines", value = config$horizontal_reference_lines %||% "")
    shiny::updateTextInput(session, "visual_reference_color", value = config$reference_line_color %||% "#6B7280")
    shiny::updateNumericInput(session, "visual_reference_width", value = config$reference_line_width %||% 0.6)
    shiny::updateNumericInput(session, "visual_fc_cutoff", value = config$fold_change_cutoff %||% 1)
    shiny::updateNumericInput(session, "visual_p_cutoff", value = config$significance_cutoff %||% 0.05)
    shiny::updateCheckboxInput(session, "visual_auto_status", value = isTRUE(config$auto_status %||% TRUE))
    shiny::updateTextInput(session, "visual_box_border_color", value = config$box_border_color %||% "#334155")
    shiny::updateCheckboxInput(session, "visual_box_show_outliers", value = isTRUE(config$box_show_outliers %||% TRUE))
    shiny::updateNumericInput(session, "visual_box_outlier_size", value = config$box_outlier_size %||% 1.5)
    shiny::updateCheckboxInput(session, "visual_box_jitter", value = isTRUE(config$box_jitter))
    shiny::updateTextInput(session, "visual_box_jitter_color", value = config$box_jitter_color %||% "#334155")
    shiny::updateNumericInput(session, "visual_box_jitter_size", value = config$box_jitter_size %||% 1.4)
    shiny::updateNumericInput(session, "visual_box_jitter_alpha", value = config$box_jitter_alpha %||% 0.55)
    shiny::updateNumericInput(session, "visual_box_jitter_width", value = config$box_jitter_width %||% 0.16)
    later::later(function() state$visual_syncing <- FALSE, 0.35)
    invisible(config)
  }

  parse_data_file <- function() {
    file <- input$data_file
    if (is.null(file) || !nzchar(file$datapath %||% "") || !file.exists(file$datapath)) return()
    extension <- tolower(tools::file_ext(file$name))
    if (extension %in% c("rds", "rdata", "rda")) {
      parsed <- tryCatch(bp_read_r_data_objects(file$datapath, file$name), error = identity)
      if (inherits(parsed, "error")) {
        state$data_import <- list(error = conditionMessage(parsed), file = file, format = extension)
        return()
      }
      state$data_import <- c(list(file = file), parsed)
      return()
    }
    options <- list(
      delimiter = input$data_delimiter %||% "auto",
      encoding = input$data_encoding %||% "UTF-8",
      header = if (is.null(input$data_header)) TRUE else isTRUE(input$data_header),
      na_values = input$data_na_values %||% ",NA,N/A,null,NULL",
      quote = input$data_quote %||% '"',
      decimal = input$data_decimal %||% ".",
      skip = input$data_skip %||% 0L
    )
    parsed <- tryCatch(bp_read_delimited_data(file$datapath, file$name, options), error = identity)
    if (inherits(parsed, "error")) {
      state$data_import <- list(error = conditionMessage(parsed), file = file)
      return()
    }
    profile <- bp_profile_dataset(parsed$data)
    matches <- Filter(function(source) {
      !isTRUE(source$example) && identical(source$original_file_name %||% "", file$name)
    }, state$project$data_sources %||% list())
    state$data_import <- list(
      file = file,
      format = "delimited",
      data = parsed$data,
      parse_options = parsed$options,
      profile = profile,
      relink_id = if (length(matches)) matches[[1]]$id else NULL,
      suggested_name = if (length(matches)) matches[[1]]$name else bp_data_source_name(
        file$name,
        vapply(state$project$data_sources %||% list(), function(source) source$name %||% "", character(1))
      )
    )
  }

  output$active_data_source_badge <- shiny::renderUI({
    active_id <- state$project$active_data_source_id %||% "dataset_example"
    sources <- Filter(function(source) identical(source$id, active_id), state$project$data_sources %||% list())
    source <- if (length(sources)) sources[[1]] else bp_example_data_source()
    htmltools::tags$span(
      class = paste("bp-data-source-badge", if (identical(source$status, "relink_required")) "is-relink-required" else "is-ready"),
      title = if (isTRUE(source$example)) "Built-in demonstration data" else source$original_file_name,
      bp_icon(if (identical(source$status, "relink_required")) "warning" else "check", 14),
      paste0("Data: ", source$name, if (isTRUE(source$example)) " · example" else if (identical(source$status, "relink_required")) " · relink" else "")
    )
  })

  output$visual_data_profile <- shiny::renderUI({
    source <- semantic_focus_source(state$project)
    if (is.null(source)) return(htmltools::tags$div(class = "bp-visual-validation-card is-invalid", "当前项目没有可用数据源。"))
    data <- semantic_focus_data(source)
    rows <- if (is.data.frame(data)) nrow(data) else source$rows %||% "—"
    columns <- if (is.data.frame(data)) ncol(data) else source$columns %||% length(source$column_metadata %||% list())
    status <- if (identical(source$status, "relink_required") || isTRUE(source$relink_required)) "需要重新链接" else "可用于绘图"
    htmltools::tags$div(
      class = "bp-visual-data-profile",
      htmltools::tags$div(htmltools::tags$span("数据对象"), htmltools::tags$strong(source$name %||% "data")),
      htmltools::tags$div(htmltools::tags$span("行 × 列"), htmltools::tags$strong(paste0(format(rows, big.mark = ","), " × ", format(columns, big.mark = ",")))),
      htmltools::tags$div(htmltools::tags$span("状态"), htmltools::tags$strong(status))
    )
  })

  output$visual_active_data_preview <- shiny::renderUI({
    source <- semantic_focus_source(state$project)
    if (is.null(source)) {
      return(htmltools::tags$div(class = "bp-visual-data-preview-empty", bp_icon("warning", 18), "当前项目没有可预览的数据源。"))
    }
    data <- semantic_focus_data(source)
    if (is.null(data) || !is.data.frame(data)) {
      return(htmltools::tags$div(
        class = "bp-visual-data-preview-empty",
        bp_icon("warning", 18),
        htmltools::tags$span("该数据源需要重新导入或链接后才能预览。")
      ))
    }
    shown_rows <- min(30L, nrow(data))
    htmltools::tagList(
      htmltools::tags$div(
        class = "bp-visual-data-preview-meta",
        htmltools::tags$strong(source$name %||% "data"),
        htmltools::tags$span(paste0(
          "显示前 ", format(shown_rows, big.mark = ","), " 行，共 ",
          format(nrow(data), big.mark = ","), " 行 · ", format(ncol(data), big.mark = ","), " 列"
        ))
      ),
      bp_data_preview_table(data, rows = 30L, columns = ncol(data), row_numbers = TRUE)
    )
  })
  shiny::outputOptions(output, "visual_active_data_preview", suspendWhenHidden = FALSE)

  output$visual_data_semantics <- shiny::renderUI({
    source <- semantic_focus_source(state$project)
    if (is.null(source)) return(NULL)
    data <- semantic_focus_data(source)
    passport <- source$passport
    if (is.null(passport) && (is.data.frame(data) || is.matrix(data))) passport <- bp_build_data_passport(data, source)
    semantic <- bp_data_source_effective_semantic(source, data)
    confirmed <- isTRUE(source$semantic_confirmed) || isTRUE(source$derived) || isTRUE(source$example)
    orientation_label <- switch(
      passport$orientation_suggestion %||% "unknown",
      genes_by_samples = "基因 × 样本",
      samples_by_features = "样本 × 特征",
      ambiguous = "需要用户确认",
      "未知"
    )
    htmltools::tags$article(
      class = paste("bp-data-passport", if (confirmed) "is-confirmed" else "needs-confirmation"),
      htmltools::tags$div(
        class = "bp-data-passport-heading",
        htmltools::tags$div(
          htmltools::tags$strong("数据护照"),
          htmltools::tags$span(source$name %||% "data")
        ),
        htmltools::tags$span(
          class = "bp-data-passport-state",
          if (confirmed) paste0("已确认 · ", semantic_label(semantic)) else paste0("建议 · ", semantic_label(semantic))
        )
      ),
      htmltools::tags$div(
        class = "bp-data-passport-grid",
        htmltools::tags$div(htmltools::tags$span("结构"), htmltools::tags$strong(passport$structure %||% source$object_type %||% "data.frame")),
        htmltools::tags$div(htmltools::tags$span("行 × 列"), htmltools::tags$strong(paste0(format(passport$rows %||% source$rows %||% 0L, big.mark = ","), " × ", format(passport$columns %||% source$columns %||% 0L, big.mark = ",")))),
        htmltools::tags$div(htmltools::tags$span("矩阵方向"), htmltools::tags$strong(orientation_label)),
        htmltools::tags$div(htmltools::tags$span("特征 ID"), htmltools::tags$strong(passport$feature_id_suggestion %||% "未识别"))
      ),
      if (length(passport$evidence %||% character())) htmltools::tags$details(
        class = "bp-data-passport-evidence",
        htmltools::tags$summary("查看识别依据"),
        htmltools::tags$ul(lapply(passport$evidence, htmltools::tags$li))
      ),
      if (isTRUE(source$derived)) {
        lineage <- source$lineage %||% list()
        htmltools::tags$div(
          class = "bp-data-lineage-summary",
          bp_icon("open", 16),
          htmltools::tags$span(paste0(
            "只读派生数据 · ", lineage$normalization %||% lineage$analysis %||% "analysis",
            if (length(lineage$parent_source_ids %||% character())) paste0(" · 来源 ", paste(lineage$parent_source_ids, collapse = " + ")) else ""
          ))
        )
      } else if (!isTRUE(source$example)) {
        htmltools::tags$div(
          class = "bp-data-semantic-confirm",
          shiny::selectInput(
            "visual_semantic_type", "确认数据语义",
            choices = bp_data_semantic_choices(), selected = if (confirmed) source$semantic_type else semantic,
            width = "100%"
          ),
          shiny::actionButton(
            "visual_confirm_semantic", if (confirmed) "更新确认" else "确认数据类型",
            icon = shiny::icon("check"), class = "bp-command-button bp-command-primary"
          )
        )
      }
    )
  })
  shiny::outputOptions(output, "visual_data_semantics", suspendWhenHidden = FALSE)

  output$visual_chart_compatibility <- shiny::renderUI({
    source <- semantic_focus_source(state$project)
    data <- semantic_focus_data(source)
    chart_type <- bp_visual_chart_type(state$project)
    compatibility <- bp_chart_data_compatibility(source, chart_type, data)
    status_label <- switch(
      compatibility$status,
      direct = "可直接使用", transform = "需要转换", supplement = "需要补充信息",
      incompatible = "不可用", relink = "需要重新链接", failed = "分析失败", "待检查"
    )
    htmltools::tags$div(
      class = paste("bp-chart-compatibility", paste0("is-", compatibility$status)),
      htmltools::tags$strong(paste0(status_label, "：", source$name %||% "data")),
      htmltools::tags$span(compatibility$reason),
      if (nzchar(compatibility$action %||% "")) htmltools::tags$em(paste0("下一步：", compatibility$action))
    )
  })
  shiny::outputOptions(output, "visual_chart_compatibility", suspendWhenHidden = FALSE)

  output$visual_pca_recipe_panel <- shiny::renderUI({
    if (!identical(bp_visual_chart_type(state$project), "pca")) return(NULL)
    config <- bp_pca_config_from_project(state$project)
    source <- bp_pca_source(state$project, config$expression_source_id)
    data <- if (is.null(source)) NULL else pca_source_data(source$id, state$project)
    semantic <- bp_data_source_effective_semantic(source, data)
    if (!isTRUE(source$semantic_confirmed) && identical(semantic, "raw_counts")) {
      return(htmltools::tags$div(
        class = "bp-pca-recipe-card needs-confirmation",
        htmltools::tags$strong("先确认 Raw count 数据语义"),
        htmltools::tags$p("检测结果只是建议。确认后才会显示过滤和标准化配方，软件不会静默执行关键分析。")
      ))
    }
    if (!identical(semantic, "raw_counts")) {
      return(htmltools::tags$div(
        class = "bp-pca-recipe-card is-generic",
        htmltools::tags$strong("通用表达矩阵 PCA"),
        htmltools::tags$p("当前数据未确认为 Raw count；将使用下方的常规转换、零方差过滤和高变特征设置。")
      ))
    }
    expected <- bp_raw_count_recipe_signature(config)
    confirmed <- identical(config$raw_count_recipe_confirmed_signature, expected)
    edge_r_available <- requireNamespace("edgeR", quietly = TRUE)
    htmltools::tags$details(
      class = paste("bp-pca-recipe-card", if (confirmed) "is-confirmed" else "needs-confirmation"),
      open = "open",
      htmltools::tags$summary(if (confirmed) "Raw count PCA 配方已确认" else "为 PCA 确认 Raw count 分析配方"),
      htmltools::tags$ol(
        htmltools::tags$li("检查非负整数计数、特征 ID 和样本 ID"),
        htmltools::tags$li("按 CPM 阈值过滤低表达特征"),
        htmltools::tags$li(if (identical(config$raw_count_normalization, "tmm_logcpm")) "使用 edgeR TMM 校正并转换为 logCPM" else "使用 log2(count + 1) 快速探索转换"),
        htmltools::tags$li("移除无效/零方差特征并选择高变特征"),
        htmltools::tags$li("使用 stats::prcomp() 生成得分与载荷")
      ),
      htmltools::tags$div(
        class = "bp-pca-recipe-grid",
        shiny::numericInput("visual_pca_filter_cpm", "CPM 阈值", value = config$raw_count_filter_cpm, min = 0, step = 0.1, width = "100%"),
        shiny::numericInput("visual_pca_filter_min_samples", "至少满足的样本数", value = config$raw_count_filter_min_samples, min = 1, step = 1, width = "100%"),
        shiny::selectInput(
          "visual_pca_normalization", "标准化与转换",
          choices = stats::setNames(
            c("tmm_logcpm", "log2p1"),
            c(paste0("edgeR TMM + logCPM", if (!edge_r_available) "（当前不可用）" else ""), "log2(count + 1) 快速探索")
          ),
          selected = config$raw_count_normalization, width = "100%"
        ),
        shiny::numericInput("visual_pca_prior_count", "logCPM prior.count", value = config$raw_count_prior_count, min = 0.01, step = 0.5, width = "100%")
      ),
      htmltools::tags$div(
        class = "bp-pca-recipe-runtime",
        bp_icon(if (edge_r_available) "check" else "warning", 16),
        if (edge_r_available) paste0("edgeR ", as.character(utils::packageVersion("edgeR")), " 可用") else "edgeR 未安装；请选择基础快速探索配方"
      ),
      shiny::actionButton(
        "visual_pca_confirm_recipe",
        if (confirmed) "使用当前设置重新生成" else "使用当前设置并生成",
        icon = shiny::icon("play"), class = "bp-command-button bp-command-primary"
      )
    )
  })
  shiny::outputOptions(output, "visual_pca_recipe_panel", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$visual_workflow_mode, {
    requested <- input$visual_workflow_mode %||% "generic"
    if (!requested %in% c("generic", "rna_seq") || identical(requested, state$project$analysis_workflow_mode %||% "generic")) return()
    project <- bp_clone_project(state$project)
    project$analysis_workflow_mode <- requested
    if (identical(requested, "generic") && identical(bp_visual_chart_type(project), "volcano")) {
      switched <- switch_visual_chart(
        "scatter", project = project, record_history = TRUE,
        origin = "workflow_mode", allow_example_fallback = TRUE
      )
      if (isTRUE(switched)) return()
    }
    commit(project, record_history = TRUE, origin = "workflow_mode")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_confirm_semantic, {
    source <- semantic_focus_source(state$project)
    data <- semantic_focus_data(source)
    if (is.null(source) || is.null(data)) {
      shiny::showNotification("当前数据源不可用；请先重新链接。", type = "warning")
      return()
    }
    project <- tryCatch(
      bp_confirm_data_source_semantic(state$project, source$id, input$visual_semantic_type %||% "generic_table", data),
      error = identity
    )
    if (inherits(project, "error")) {
      shiny::showNotification(conditionMessage(project), type = "error")
      return()
    }
    if (identical(bp_visual_chart_type(project), "pca")) {
      config <- bp_pca_config_from_project(project)
      confirmed_source <- bp_pca_source(project, config$expression_source_id)
      config$input_semantic_type <- confirmed_source$semantic_type %||% "generic_table"
      config$raw_count_recipe_confirmed_signature <- ""
      project$visual_config$pca <- bp_normalize_pca_config(config, project)
      project$analysis_recipes <- project$analysis_recipes %||% list()
      project$analysis_recipes$pca <- NULL
    }
    commit(project, record_history = TRUE, origin = "data_semantic")
    sync_visual_inputs(project, bp_visual_config_from_project(project))
    if (identical(input$visual_semantic_type, "raw_counts")) {
      shiny::showNotification("已确认 Raw count。请检查并确认图表专属分析配方。", type = "message", duration = 8)
    } else {
      shiny::showNotification(paste0("数据语义已确认为：", semantic_label(input$visual_semantic_type), "。"), type = "message")
    }
  }, ignoreInit = TRUE)

  output$visual_pca_link_diagnostics <- shiny::renderUI({
    if (!identical(bp_visual_chart_type(state$project), "pca")) return(NULL)
    result <- pca_result()
    if (is.null(result)) return(NULL)
    if (!isTRUE(result$ok)) {
      return(htmltools::tags$div(
        class = "bp-pca-diagnostics is-error",
        htmltools::tags$strong("PCA 数据检查未通过"),
        result$error %||% "无法计算 PCA。"
      ))
    }
    diagnostics <- result$diagnostics %||% list()
    expression_only <- diagnostics$expression_only %||% character()
    metadata_only <- diagnostics$metadata_only %||% character()
    has_warning <- length(expression_only) || length(metadata_only) || length(result$warnings %||% character())
    htmltools::tags$div(
      class = paste("bp-pca-diagnostics", if (has_warning) "is-warning"),
      htmltools::tags$strong("样本关联检查通过"),
      htmltools::tags$div(paste0(
        "表达矩阵样本：", diagnostics$expression_samples %||% nrow(result$scores),
        if ((diagnostics$metadata_samples %||% 0L) > 0L) paste0(" · 样本信息：", diagnostics$metadata_samples, " · 匹配：", diagnostics$matched_samples) else " · 未使用样本信息"
      )),
      if (length(expression_only)) htmltools::tags$div(paste0("仅表达矩阵：", paste(head(expression_only, 8L), collapse = "、"))),
      if (length(metadata_only)) htmltools::tags$div(paste0("仅样本信息：", paste(head(metadata_only, 8L), collapse = "、"))),
      lapply(result$warnings %||% character(), htmltools::tags$div)
    )
  })

  output$visual_pca_result_summary <- shiny::renderUI({
    if (!identical(bp_visual_chart_type(state$project), "pca")) return(NULL)
    result <- pca_result()
    if (is.null(result) || !isTRUE(result$ok)) return(NULL)
    variance <- result$explained_variance %||% numeric()
    htmltools::tagList(
      htmltools::tags$div(
        class = "bp-pca-result-summary",
        htmltools::tags$strong("PCA 结果"),
        htmltools::tags$div(paste0(
          nrow(result$scores), " 个样本 · ", result$selected_feature_count, " 个特征 · ",
          if (identical(result$orientation, "genes_by_samples")) "基因 × 样本" else "样本 × 特征",
          " · 转换：", result$transform_applied
        )),
        htmltools::tags$div(paste0(
          "PC1：", sprintf("%.1f%%", variance[["PC1"]] %||% NA_real_),
          if ("PC2" %in% names(variance)) paste0(" · PC2：", sprintf("%.1f%%", variance[["PC2"]])) else ""
        )),
        if (!is.null(result$preparation)) htmltools::tags$div(paste0(
          "Raw count 配方：", result$preparation$normalization_label,
          " · 低表达过滤保留 ", result$preparation$retained_feature_count, " / ", result$preparation$original_feature_count, " 个特征"
        )),
        if ((result$zero_variance_removed %||% 0L) > 0L) htmltools::tags$div(paste0("已移除 ", result$zero_variance_removed, " 个零方差特征。"))
      ),
      if (is.data.frame(result$normalized_expression)) htmltools::tags$details(
        class = "bp-visual-data-preview bp-pca-score-preview",
        htmltools::tags$summary("预览标准化表达矩阵（前 12 行 · 全部列）"),
        bp_data_preview_table(result$normalized_expression, rows = 12L, columns = ncol(result$normalized_expression), row_numbers = TRUE)
      ),
      htmltools::tags$details(
        class = "bp-visual-data-preview bp-pca-score-preview",
        htmltools::tags$summary("预览 PCA 得分（前 12 行 · 全部列）"),
        bp_data_preview_table(result$scores, rows = 12L, columns = ncol(result$scores), row_numbers = TRUE)
      )
    )
  })
  shiny::outputOptions(output, "visual_pca_link_diagnostics", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "visual_pca_result_summary", suspendWhenHidden = FALSE)

  output$visual_field_recommendation <- shiny::renderUI({
    source <- visual_source(state$project)
    if (is.null(source)) return(NULL)
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source$id]]
    chart_type <- bp_visual_chart_type(state$project)
    recommendation <- if (identical(chart_type, "volcano")) {
      bp_visual_recommend_volcano_fields(source, data)
    } else if (identical(chart_type, "boxplot")) {
      bp_visual_recommend_boxplot_fields(source, data)
    } else {
      bp_visual_recommend_scatter_fields(source, data)
    }
    if (!nzchar(recommendation$x_field) || !nzchar(recommendation$y_field)) return(NULL)
    recommendation_text <- if (identical(chart_type, "volcano")) {
      paste0(
        "倍数变化 = ", recommendation$x_field, "，显著性 = ", recommendation$y_field,
        if (nzchar(recommendation$color_field)) paste0("，状态 = ", recommendation$color_field) else "，自动按阈值分组",
        "。"
      )
    } else if (identical(chart_type, "boxplot")) {
      paste0(
        "分组 = ", recommendation$x_field, "，数值 = ", recommendation$y_field,
        if (nzchar(recommendation$color_field)) paste0("，填充 = ", recommendation$color_field) else "",
        "。"
      )
    } else {
      paste0("X = ", recommendation$x_field, "，Y = ", recommendation$y_field,
        if (nzchar(recommendation$color_field)) paste0("，颜色 = ", recommendation$color_field) else "", "。")
    }
    htmltools::tags$div(
      class = "bp-visual-recommendation",
      bp_icon("info", 17),
      htmltools::tags$div(
        htmltools::tags$strong("字段建议："),
        recommendation_text
      )
    )
  })

  output$visual_advanced_state <- shiny::renderUI({
    config <- bp_visual_config_from_project(state$project)
    if (!isTRUE(config$advanced_preserved)) return(NULL)
    htmltools::tags$div(
      class = "bp-visual-advanced-note",
      bp_icon("check", 17),
      htmltools::tags$div(
        htmltools::tags$strong("高级设置已保留"),
        htmltools::tags$div("当前项目含有可视化面板未展开的图层、映射或参数。它们会继续参与代码生成和预览，可在 R / 高级模式中查看。")
      )
    )
  })

  output$visual_preview_status <- shiny::renderUI({
    status <- state$preview_status
    label <- switch(status, running = "正在生成", error = "预览有错误", cancelled = "已取消", success = "预览已同步", "等待预览")
    htmltools::tags$span(
      class = paste("bp-visual-preview-status", paste0("is-", status)),
      htmltools::tags$span(class = "bp-status-dot"),
      label
    )
  })

  output$visual_validation <- shiny::renderUI({
    validation <- visual_validation()
    if (isTRUE(validation$valid)) {
      return(htmltools::tags$div(
        class = "bp-visual-validation-card",
        bp_icon("check", 16),
        htmltools::tags$span("配置有效。可视化设置、模块状态、R 代码和预览保持同步。")
      ))
    }
    htmltools::tags$div(
      class = "bp-visual-validation-card is-invalid",
      bp_icon("warning", 16),
      htmltools::tags$span(paste(validation$errors, collapse = " "))
    )
  })

  output$visual_action_status <- shiny::renderUI({
    config <- bp_visual_config_from_project(state$project)
    status <- switch(state$preview_status, running = "正在使用本地 R 生成预览…", error = "本次预览失败；已保留上一次成功图片。", success = "已同步到 ggplot2 项目并完成预览。", cancelled = "预览已取消，项目设置未改变。", "更改字段或样式后将自动预览。")
    htmltools::tags$span(
      paste0(status, if (isTRUE(config$advanced_preserved)) " 高级设置仍已保留。" else "")
    )
  })

  output$data_source_manager_list <- shiny::renderUI({
    sources <- state$project$data_sources %||% list(bp_example_data_source())
    active_id <- state$project$active_data_source_id %||% "dataset_example"
    action_button <- function(source, action, label, disabled = FALSE, danger = FALSE) htmltools::tags$button(
      type = "button",
      class = paste("bp-command-button bp-data-source-action", if (danger) "is-danger"),
      `data-source-id` = source$id,
      `data-action` = action,
      disabled = if (disabled) "disabled" else NULL,
      label
    )
    htmltools::tags$div(
      class = "bp-data-source-manager",
      lapply(sources, function(source) {
        is_active <- identical(source$id, active_id)
        stale <- identical(source$status, "derived_stale")
        ready <- !stale && !identical(source$status, "relink_required") && !isTRUE(source$relink_required) && (isTRUE(source$example) || !is.null(state$data_objects[[source$id]]))
        htmltools::tags$article(
          class = paste("bp-data-source-card", if (is_active) "is-active", if (!ready && !stale) "needs-relink", if (stale) "is-stale"),
          htmltools::tags$div(
            class = "bp-data-source-card-main",
            htmltools::tags$div(
              htmltools::tags$strong(source$name),
              if (is_active) htmltools::tags$span(class = "bp-active-source-label", "Active plot data"),
              if (isTRUE(source$example)) htmltools::tags$span(class = "bp-example-source-label", "Example"),
              if (isTRUE(source$derived)) htmltools::tags$span(class = "bp-example-source-label", if (stale) "Derived · stale" else "Derived · read only"),
              htmltools::tags$span(
                class = paste("bp-semantic-source-label", if (isTRUE(source$semantic_confirmed)) "is-confirmed"),
                semantic_label(semantic_display_value(source))
              )
            ),
            htmltools::tags$p(paste0(
              toupper(source$source_type %||% "data"), " · ", format(source$rows %||% 0L, big.mark = ","), " rows × ", source$columns %||% 0L, " columns",
              if (!is.null(source$object_name) && !identical(source$object_name, source$name)) paste0(" · object ", source$object_name) else ""
            )),
            htmltools::tags$small(if (stale) {
              "Input data or analysis parameters changed; regenerate this derived result."
            } else if (ready) {
              source$original_file_name %||% "Built-in deterministic dataset"
            } else {
              paste0("Relink required: ", source$original_file_name %||% "original file")
            }),
            if (isTRUE(source$derived) && length(source$lineage$parent_source_ids %||% character())) htmltools::tags$small(
              class = "bp-data-source-lineage",
              paste0("Lineage: ", paste(source$lineage$parent_source_ids, collapse = " + "), " → ", source$name)
            )
          ),
          htmltools::tags$div(
            class = "bp-data-source-card-actions",
            action_button(source, "preview", "Preview", disabled = !ready),
            if (!isTRUE(source$derived)) action_button(source, "map", if (is_active) "Remap" else "Use in plot", disabled = !ready),
            if (!isTRUE(source$example) && !isTRUE(source$derived)) action_button(source, "rename", "Rename"),
            if (!ready && !isTRUE(source$example) && !isTRUE(source$derived)) action_button(source, "relink", "Relink"),
            if (!isTRUE(source$example) && !isTRUE(source$derived)) action_button(source, "remove", "Remove", disabled = is_active, danger = TRUE)
          )
        )
      })
    )
  })

  shiny::observeEvent(input$manage_data_sources, shiny::showModal(bp_data_source_manager_modal()), ignoreInit = TRUE)

  shiny::observeEvent(input$manager_import_data, {
    state$data_import <- NULL
    shiny::removeModal()
    shiny::showModal(bp_data_import_modal())
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$data_source_action, {
    payload <- input$data_source_action
    source_id <- payload$source_id %||% ""
    action <- payload$action %||% ""
    sources <- Filter(function(source) identical(source$id, source_id), state$project$data_sources %||% list())
    if (!length(sources)) return()
    source <- sources[[1]]
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source_id]]
    if (identical(action, "preview")) {
      if (identical(source$status, "relink_required") || isTRUE(source$relink_required) || is.null(data)) return()
      state$data_preview_source_id <- source_id
      shiny::removeModal()
      session$sendCustomMessage("bp_set_preview_view", list(view = "data"))
      return()
    }
    if (identical(action, "map")) {
      if (identical(source$status, "relink_required") || isTRUE(source$relink_required) || is.null(data)) {
        shiny::showNotification("Relink the original file before mapping this source.", type = "warning")
        return()
      }
      state$pending_mapping_source_id <- source_id
      shiny::showModal(bp_data_source_mapping_modal(source, data, state$project))
      return()
    }
    if (identical(action, "rename") && !isTRUE(source$example)) {
      state$pending_rename_source_id <- source_id
      shiny::showModal(shiny::modalDialog(
        title = paste0("Rename data source: ", source$name),
        shiny::textInput("renamed_data_source", "R object name", value = source$name),
        htmltools::tags$p(class = "bp-modal-note", "This changes the generated R object name, not the original file or RData object name."),
        footer = htmltools::tagList(shiny::modalButton("Cancel"), shiny::actionButton("apply_data_source_rename", "Rename", class = "bp-command-primary"))
      ))
      return()
    }
    if (identical(action, "relink") && !isTRUE(source$example)) {
      state$data_import <- NULL
      shiny::removeModal()
      shiny::showModal(bp_data_import_modal())
      shiny::showNotification(paste0("Choose ", source$original_file_name, "; matching objects will keep their data-source IDs and mappings."), type = "message", duration = 7)
      return()
    }
    if (identical(action, "remove") && !isTRUE(source$example)) {
      project <- tryCatch(bp_remove_data_source(state$project, source_id), error = identity)
      if (inherits(project, "error")) {
        shiny::showNotification(conditionMessage(project), type = "error")
        return()
      }
      state$data_objects[[source_id]] <- NULL
      if (identical(state$data_preview_source_id, source_id)) state$data_preview_source_id <- state$project$active_data_source_id %||% "dataset_example"
      commit(project)
      shiny::showNotification(paste0("Removed data source '", source$name, "'."), type = "message")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$apply_data_source_rename, {
    source_id <- state$pending_rename_source_id %||% ""
    new_name <- trimws(input$renamed_data_source %||% "")
    if (!grepl("^[.A-Za-z][.A-Za-z0-9_]*$", new_name)) {
      shiny::showNotification("Data source name must be a valid R identifier.", type = "error")
      return()
    }
    other_names <- vapply(Filter(function(source) !identical(source$id, source_id), state$project$data_sources %||% list()), function(source) source$name %||% "", character(1))
    if (new_name %in% other_names) {
      shiny::showNotification("Another data source already uses that name.", type = "error")
      return()
    }
    project <- tryCatch(bp_rename_data_source(state$project, source_id, new_name), error = identity)
    if (inherits(project, "error")) {
      shiny::showNotification(conditionMessage(project), type = "error")
      return()
    }
    commit(project)
    state$pending_rename_source_id <- NULL
    shiny::removeModal()
    shiny::showNotification(paste0("Data source renamed to '", new_name, "'."), type = "message")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$apply_data_source_mapping, {
    source_id <- state$pending_mapping_source_id %||% ""
    sources <- Filter(function(source) identical(source$id, source_id), state$project$data_sources %||% list())
    if (!length(sources)) return()
    source <- sources[[1]]
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source_id]]
    if (is.null(data)) {
      shiny::showNotification("Relink the data source before mapping it.", type = "error")
      return()
    }
    roles <- c("x", "y", "color", "fill", "shape", "size", "alpha", "label", "group")
    mapping <- stats::setNames(lapply(roles, function(role) input[[paste0("data_map_", role)]] %||% ""), roles)
    if (!nzchar(mapping$x) || !nzchar(mapping$y)) {
      shiny::showNotification("Choose both X and Y columns.", type = "error")
      return()
    }
    profile <- bp_profile_dataset(data)
    metadata_by_name <- stats::setNames(profile$column_metadata, names(data))
    for (role in c("size", "alpha")) {
      column <- mapping[[role]]
      if (nzchar(column) && !metadata_by_name[[column]]$recommended_type %in% c("numeric", "integer")) {
        shiny::showNotification(paste(toupper(role), "usually requires a numeric column."), type = "warning")
      }
    }
    result <- tryCatch(switch_registered_data_source(state$project, source, mapping), error = identity)
    if (inherits(result, "error")) {
      shiny::showNotification(conditionMessage(result), type = "warning")
      return()
    }
    commit(result$project, result$root_instance_id)
    state$data_preview_source_id <- source_id
    state$last_data_switch <- result
    state$pending_mapping_source_id <- NULL
    shiny::removeModal()
    notify_data_source_switch(result)
    start_preview()
  }, ignoreInit = TRUE)

  output$data_import_results <- shiny::renderUI({
    imported <- state$data_import
    if (is.null(imported)) {
      return(htmltools::tags$div(class = "bp-import-empty", bp_icon("import", 30), htmltools::tags$p("Choose a CSV, TSV, TXT, RDS, RData, or rda file to analyze.")))
    }
    if (!is.null(imported$error)) {
      return(htmltools::tags$div(class = "bp-import-error", bp_icon("warning", 22), htmltools::tags$strong("Import failed"), htmltools::tags$p(imported$error)))
    }
    if (imported$format %in% c("rds", "rdata", "rda")) return(bp_r_object_browser(imported))
    profile <- imported$profile
    warnings <- profile$warnings %||% list()
    passport <- bp_build_data_passport(imported$data)
    semantic_labels <- stats::setNames(names(bp_data_semantic_choices(include_auto = TRUE)), unname(bp_data_semantic_choices(include_auto = TRUE)))
    suggested_label <- semantic_labels[[passport$suggested_semantic_type]] %||% passport$suggested_semantic_type
    htmltools::tagList(
      htmltools::tags$h3("2. Review and map"),
      htmltools::tags$div(
        class = "bp-data-summary-grid",
        htmltools::tags$div(htmltools::tags$span("Rows"), htmltools::tags$strong(format(profile$rows, big.mark = ","))),
        htmltools::tags$div(htmltools::tags$span("Columns"), htmltools::tags$strong(profile$columns)),
        htmltools::tags$div(htmltools::tags$span("Numeric"), htmltools::tags$strong(profile$numeric_columns)),
        htmltools::tags$div(htmltools::tags$span("Categorical"), htmltools::tags$strong(profile$categorical_columns)),
        htmltools::tags$div(htmltools::tags$span("Missing"), htmltools::tags$strong(format(profile$missing_values, big.mark = ","))),
        htmltools::tags$div(htmltools::tags$span("Duplicate rows"), htmltools::tags$strong(format(profile$duplicate_rows, big.mark = ",")))
      ),
      shiny::textInput("data_source_name", "Data object name", value = imported$suggested_name),
      htmltools::tags$div(
        class = "bp-import-semantic-suggestion",
        htmltools::tags$strong(paste0("数据语义建议：可能是 ", suggested_label)),
        htmltools::tags$span(paste0(" · 置信度 ", sprintf("%.0f%%", 100 * passport$confidence))),
        htmltools::tags$ul(lapply(passport$evidence, htmltools::tags$li))
      ),
      shiny::selectInput(
        "data_semantic_type", "确认数据语义",
        choices = bp_data_semantic_choices(include_auto = TRUE), selected = "auto", width = "100%"
      ),
      shiny::checkboxInput(
        "data_register_only", "只注册数据源，不修改当前通用图表映射",
        value = identical(state$project$analysis_workflow_mode %||% "generic", "rna_seq")
      ),
      htmltools::tags$details(class = "bp-import-section", open = "open", htmltools::tags$summary("Data preview · first 30 rows"), bp_data_preview_table(imported$data)),
      htmltools::tags$details(class = "bp-import-section", htmltools::tags$summary("Column types and quality"), bp_data_column_table(profile)),
      if (length(warnings)) htmltools::tags$div(
        class = "bp-data-quality-warnings",
        htmltools::tags$strong(paste("Quality checks ·", length(warnings))),
        htmltools::tags$ul(lapply(warnings, function(warning) htmltools::tags$li(class = paste0("is-", warning$level), warning$message)))
      ),
      htmltools::tags$details(
        class = "bp-import-section",
        htmltools::tags$summary("通用绘图字段映射（取消“只注册”时使用）"),
        htmltools::tags$div(class = "bp-mapping-title", htmltools::tags$strong("Manual column mapping"), htmltools::tags$span("Raw count 引导模式无需在导入时设置 X/Y。")),
        bp_data_mapping_controls(imported, state$project)
      )
    )
  })

  output$r_object_preview <- shiny::renderUI({
    imported <- state$data_import
    shiny::req(!is.null(imported), is.null(imported$error), imported$format %in% c("rds", "rdata", "rda"))
    supported <- Filter(function(item) isTRUE(item$supported), imported$metadata %||% list())
    shiny::req(length(supported) > 0L)
    supported_names <- vapply(supported, `[[`, character(1), "name")
    object_name <- input$r_preview_object %||% supported_names[[1]]
    if (!object_name %in% supported_names) object_name <- supported_names[[1]]
    metadata <- supported[[match(object_name, supported_names)]]
    row_names <- input$r_row_names %||% "preserve"
    row_name_column <- input$r_row_name_column %||% "RowName"
    data <- tryCatch(
      bp_convert_r_object(imported$objects[[object_name]], row_names = row_names, row_name_column = row_name_column),
      error = identity
    )
    if (inherits(data, "error")) {
      return(htmltools::tags$div(class = "bp-import-error", conditionMessage(data)))
    }
    htmltools::tagList(
      htmltools::tags$div(
        class = "bp-r-preview-summary",
        htmltools::tags$strong(paste0("Preview: ", object_name)),
        htmltools::tags$span(paste0(
          "Showing ", min(30L, nrow(data)), " of ", format(nrow(data), big.mark = ","),
          " rows · all ", format(ncol(data), big.mark = ","), " columns",
          if (isTRUE(metadata$requires_conversion)) " · converted to data.frame for preview" else ""
        ))
      ),
      bp_data_preview_table(data, rows = 30L, columns = ncol(data), row_numbers = TRUE)
    )
  })
  shiny::outputOptions(output, "r_object_preview", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$import_data, {
    state$data_import <- NULL
    shiny::showModal(bp_data_import_modal())
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$data_file, parse_data_file(), ignoreInit = TRUE)
  shiny::observeEvent(input$analyze_data_file, parse_data_file(), ignoreInit = TRUE)

  shiny::observeEvent(input$cancel_data_import, {
    state$data_import <- NULL
    shiny::removeModal()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$register_data_source, {
    imported <- state$data_import
    if (!is.null(imported) && is.null(imported$error) && imported$format %in% c("rds", "rdata", "rda")) {
      selected_objects <- intersect(input$r_object_selection %||% character(), names(imported$objects %||% list()))
      if (!length(selected_objects)) {
        shiny::showNotification("Select at least one supported R object.", type = "warning")
        return()
      }
      row_names <- input$r_row_names %||% "preserve"
      row_name_column <- input$r_row_name_column %||% "RowName"
      project <- bp_clone_project(state$project)
      registered_ids <- character()
      existing_names <- vapply(project$data_sources %||% list(), function(source) source$name %||% "", character(1))
      for (object_name in selected_objects) {
        metadata <- Filter(function(item) identical(item$name, object_name), imported$metadata %||% list())[[1]]
        data <- tryCatch(bp_convert_r_object(imported$objects[[object_name]], row_names = row_names, row_name_column = row_name_column), error = identity)
        if (inherits(data, "error")) {
          shiny::showNotification(paste0(object_name, ": ", conditionMessage(data)), type = "error", duration = NULL)
          return()
        }
        matches <- Filter(function(source) {
          !isTRUE(source$example) && identical(source$original_file_name %||% "", imported$file$name) && identical(source$object_name %||% "", object_name)
        }, project$data_sources %||% list())
        source_id <- if (length(matches)) matches[[1]]$id else bp_data_source_id(project)
        source_name <- if (length(matches)) matches[[1]]$name else bp_data_source_name(object_name, existing_names)
        profile <- bp_profile_dataset(data)
        conversion <- attr(data, "bp_conversion") %||% list(from = metadata$kind, to = "data.frame", row_names = row_names, row_name_column = if (identical(row_names, "column")) row_name_column else NULL)
        attr(data, "bp_conversion") <- NULL
        source <- list(
          id = source_id, name = source_name, source_type = imported$format,
          original_file_name = imported$file$name, object_name = object_name,
          object_type = "data.frame", original_object_type = metadata$kind, r_class = metadata$r_class,
          rows = nrow(data), columns = ncol(data), status = "ready", example = FALSE, relink_required = FALSE,
          column_metadata = profile$column_metadata,
          quality = list(missing_values = profile$missing_values, duplicate_rows = profile$duplicate_rows, duplicate_column_names = profile$duplicate_column_names, warnings = profile$warnings),
          conversion = conversion, parse_options = list()
        )
        source <- bp_enrich_data_source(source, data, if (length(matches)) matches[[1]] else NULL)
        project <- bp_register_data_source(project, source)
        state$data_objects[[source_id]] <- data
        registered_ids <- c(registered_ids, source_id)
        existing_names <- c(existing_names, source_name)
      }
      commit(project, record_history = TRUE)
      state$data_preview_source_id <- registered_ids[[1]]
      state$data_import <- NULL
      shiny::removeModal()
      shiny::showNotification(paste(length(registered_ids), "R object data source(s) registered. Open Data Sources to preview or map one to the plot."), type = "message", duration = 7)
      return()
    }
    if (is.null(imported) || !is.null(imported$error) || is.null(imported$data)) {
      shiny::showNotification("Choose and analyze a supported data file first.", type = "warning")
      return()
    }
    name <- trimws(input$data_source_name %||% "")
    if (!grepl("^[.A-Za-z][.A-Za-z0-9_]*$", name)) {
      shiny::showNotification("Data object name must be a valid R identifier.", type = "error")
      return()
    }
    data <- imported$data
    conversions <- list()
    for (index in seq_along(data)) {
      target <- input[[paste0("data_type_", index)]] %||% imported$profile$column_metadata[[index]]$recommended_type
      current <- bp_column_type(data[[index]])
      if (!identical(target, current)) {
        converted <- tryCatch(bp_convert_column(data[[index]], target), error = identity)
        if (inherits(converted, "error")) {
          shiny::showNotification(paste0("Column ", names(data)[[index]], ": ", conditionMessage(converted)), type = "error", duration = NULL)
          return()
        }
        data[[index]] <- converted
        conversions[[length(conversions) + 1L]] <- list(column = names(data)[[index]], from = current, to = target)
      }
    }
    roles <- c("x", "y", "color", "fill", "shape", "size", "alpha", "label", "group")
    mapping <- stats::setNames(lapply(roles, function(role) input[[paste0("data_map_", role)]] %||% ""), roles)
    register_only <- isTRUE(input$data_register_only)
    if (!register_only && (!nzchar(mapping$x) || !nzchar(mapping$y))) {
      shiny::showNotification("Choose both X and Y columns.", type = "error")
      return()
    }
    updated_profile <- bp_profile_dataset(data)
    metadata_by_name <- stats::setNames(updated_profile$column_metadata, names(data))
    for (role in if (register_only) character() else c("size", "alpha")) {
      column <- mapping[[role]]
      if (nzchar(column) && !metadata_by_name[[column]]$recommended_type %in% c("numeric", "integer")) {
        shiny::showNotification(paste(toupper(role), "requires a numeric column in the first-stage mapper."), type = "error")
        return()
      }
    }
    extension <- tolower(tools::file_ext(imported$file$name))
    source_id <- imported$relink_id %||% bp_data_source_id(state$project)
    source <- list(
      id = source_id,
      name = name,
      source_type = if (extension %in% c("csv", "tsv", "txt")) extension else "txt",
      original_file_name = imported$file$name,
      object_type = "data.frame",
      rows = nrow(data), columns = ncol(data), status = "ready", example = FALSE,
      relink_required = FALSE,
      column_metadata = updated_profile$column_metadata,
      quality = list(
        missing_values = updated_profile$missing_values,
        duplicate_rows = updated_profile$duplicate_rows,
        duplicate_column_names = updated_profile$duplicate_column_names,
        warnings = updated_profile$warnings
      ),
      parse_options = imported$parse_options,
      conversions = conversions
    )
    previous <- Filter(function(item) identical(item$id, source_id), state$project$data_sources %||% list())
    source <- bp_enrich_data_source(source, data, if (length(previous)) previous[[1]] else NULL)
    requested_semantic <- input$data_semantic_type %||% "auto"
    if (!identical(requested_semantic, "auto")) {
      source$semantic_type <- bp_normalize_semantic_type(requested_semantic)
      source$semantic_confirmed <- TRUE
      source$semantic_confirmed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      source$semantic_user_override <- !identical(source$semantic_type, source$semantic_suggestion)
    }
    if (register_only) {
      project <- bp_register_data_source(state$project, source)
      if (identical(source$semantic_type, "raw_counts") && isTRUE(source$semantic_confirmed)) project$analysis_workflow_mode <- "rna_seq"
      state$data_objects[[source_id]] <- data
      state$data_preview_source_id <- source_id
      commit(project, record_history = TRUE, origin = "data_import")
      state$data_import <- NULL
      shiny::removeModal()
      shiny::showNotification(paste0("Data source '", name, "' was registered without changing the current plot mapping."), type = "message")
      return()
    }
    project <- bp_apply_dataset_mapping(state$project, source, mapping)
    state$data_objects[[source_id]] <- data
    state$data_preview_source_id <- source_id
    root_index <- which(vapply(project$modules, function(module) identical(module$module_id, "r.ggplot2.ggplot"), logical(1)))[1]
    commit(project, project$modules[[root_index]]$instance_id)
    state$data_import <- NULL
    shiny::removeModal()
    shiny::showNotification(paste0("Data source '", name, "' is ready and mapped to the plot."), type = "message")
    start_preview()
  }, ignoreInit = TRUE)

  output$module_picker <- shiny::renderUI({
    groups <- c(All = "all", Core = "core", Geoms = "geoms", Structure = "structure", Scales = "scales", Templates = "templates")
    htmltools::tags$div(
      class = "bp-picker-groups",
      lapply(names(groups), function(label) {
        value <- groups[[label]]
        specs <- if (identical(value, "all")) {
          registry
        } else if (identical(value, "templates")) {
          list()
        } else {
          Filter(function(spec) identical(bp_category_filter(spec$presentation$category), value), registry)
        }
        bp_module_picker_group(
          label,
          value,
          specs = specs,
          templates = if (identical(value, "templates")) templates else list()
        )
      })
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
      tabindex = "0",
      `aria-label` = "Scrollable layer stack",
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
    if (length(index) != 1L || is.na(index)) {
      return(htmltools::tags$div(class = "bp-inspector-empty", bp_icon("info", 28), htmltools::tags$p("Select a module to inspect native R arguments.")))
    }
    instance <- project$modules[[index]]
    spec <- bp_get_spec(registry, instance$module_id)
    parameters <- spec$parameters %||% list()
    tab <- state$parameter_tab
    if (identical(tab, "common")) parameters <- Filter(function(x) identical(x$ui_group, "common"), parameters)
    if (identical(tab, "advanced")) parameters <- Filter(function(x) identical(x$ui_group, "advanced"), parameters)
    effective <- bp_effective_mapping(project, index)
    column_suggestions <- bp_active_data_column_suggestions(project, state$data_objects)
    data_source_suggestions <- bp_data_source_reference_suggestions(project)
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
          bp_parameter_row(instance, parameter, argument, effective, state$expression_edit, column_suggestions, data_source_suggestions)
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

  output$analysis_context_view <- shiny::renderUI({
    if (!identical(bp_visual_chart_type(state$project), "pca")) return(NULL)
    config <- bp_pca_config_from_project(state$project)
    expression_source <- bp_pca_source(state$project, config$expression_source_id)
    metadata_source <- if (nzchar(config$metadata_source_id)) bp_pca_source(state$project, config$metadata_source_id) else NULL
    raw_counts <- identical(config$input_semantic_type, "raw_counts")
    packages <- c(
      paste0("R ", getRversion()),
      paste0("stats ", getRversion()),
      paste0("ggplot2 ", as.character(utils::packageVersion("ggplot2"))),
      if (raw_counts && requireNamespace("edgeR", quietly = TRUE)) paste0("edgeR ", as.character(utils::packageVersion("edgeR")))
    )
    htmltools::tags$div(
      class = "bp-analysis-context",
      htmltools::tags$div(htmltools::tags$span("分析输入"), htmltools::tags$strong(paste0(expression_source$name %||% "data", if (!is.null(metadata_source)) paste0(" + ", metadata_source$name) else ""))),
      htmltools::tags$div(htmltools::tags$span("数据语义"), htmltools::tags$strong(semantic_label(config$input_semantic_type))),
      htmltools::tags$div(htmltools::tags$span("分析配方"), htmltools::tags$strong(if (raw_counts) paste0(config$raw_count_normalization, " → prcomp") else paste0(config$transform, " → prcomp"))),
      htmltools::tags$div(htmltools::tags$span("运行依赖"), htmltools::tags$strong(paste(packages, collapse = " · ")))
    )
  })

  output$analysis_code_view <- shiny::renderUI({
    if (!identical(bp_visual_chart_type(state$project), "pca")) return(NULL)
    code <- tryCatch(bp_generate_pca_analysis_code(state$project), error = identity)
    if (inherits(code, "error")) {
      return(htmltools::tags$div(class = "bp-code-error", bp_icon("warning", 20), conditionMessage(code)))
    }
    lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
    htmltools::tags$details(
      class = "bp-analysis-code",
      open = "open",
      htmltools::tags$summary(paste0("Generated analysis R · ", length(lines), " lines")),
      htmltools::tags$div(
        class = "bp-code-editor",
        lapply(seq_along(lines), function(index) {
          htmltools::tags$div(
            class = "bp-code-line",
            htmltools::tags$span(class = "bp-line-number", index),
            htmltools::tags$code(htmltools::HTML(bp_highlight_r_line(lines[[index]])))
          )
        })
      )
    )
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
    if (identical(bp_visual_chart_type(state$project), "pca")) {
      analysis <- tryCatch(bp_generate_pca_analysis_code(state$project), error = function(error) "")
      if (nzchar(analysis)) count <- count + length(strsplit(analysis, "\n", fixed = TRUE)[[1]])
    }
    htmltools::tags$span(class = "bp-line-count", paste(count, if (count == 1L) "line" else "lines"))
  })

  output$generated_code_transport <- shiny::renderUI({
    code <- tryCatch(bp_generate_code(state$project, registry), error = function(error) "")
    htmltools::tags$textarea(id = "generated_code_raw", class = "bp-code-transport", code)
  })

  output$project_state_transport <- shiny::renderUI({
    payload <- jsonlite::toJSON(
      list(format_version = 1L, project = state$project, selected = state$selected),
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
    htmltools::tags$textarea(
      id = "project_state_raw",
      class = "bp-code-transport",
      readonly = "readonly",
      tabindex = "-1",
      `aria-hidden` = "true",
      as.character(payload)
    )
  })

  preview_image_ui <- function() {
    path <- state$preview_image
    shiny::req(!is.null(path), file.exists(path))
    htmltools::tags$img(
      src = base64enc::dataURI(file = path, mime = "image/png"),
      alt = "ggplot2 preview generated from the current module stack"
    )
  }

  preview_overlay_ui <- function() {
    status <- state$preview_status
    if (identical(status, "success")) return(NULL)
    if (identical(status, "running")) {
      return(htmltools::tags$div(class = "bp-preview-overlay", htmltools::tags$span(class = "bp-spinner"), htmltools::tags$div(htmltools::tags$strong("正在生成真实 ggplot2 预览"), htmltools::tags$p("界面仍可继续操作；完成前会保留上一次成功图片。"))))
    }
    if (identical(status, "error")) {
      return(htmltools::tags$div(
        class = "bp-preview-overlay bp-preview-error",
        bp_icon("warning", 28),
        htmltools::tags$div(
          htmltools::tags$strong("预览失败 / Preview failed"),
          htmltools::tags$p(
            role = "region",
            tabindex = "0",
            `aria-label` = "Preview error details",
            state$preview_result$error %||% "Unknown R error"
          )
        )
      ))
    }
    if (identical(status, "cancelled")) {
      return(htmltools::tags$div(class = "bp-preview-overlay", bp_icon("info", 28), htmltools::tags$strong("Preview cancelled"), htmltools::tags$p("Your module state and code were not changed.")))
    }
    htmltools::tags$div(class = "bp-preview-overlay", bp_icon("plot", 30), htmltools::tags$div(htmltools::tags$strong("预览已就绪"), htmltools::tags$p("选择字段后由本地 R 生成图像。")))
  }

  output$preview_image <- shiny::renderUI(preview_image_ui())
  output$visual_preview_image <- shiny::renderUI(preview_image_ui())
  output$preview_overlay <- shiny::renderUI(preview_overlay_ui())
  output$visual_preview_overlay <- shiny::renderUI(preview_overlay_ui())
  shiny::outputOptions(output, "preview_image", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "visual_preview_image", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "preview_overlay", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "visual_preview_overlay", suspendWhenHidden = FALSE)

  output$active_data_preview <- shiny::renderUI({
    active_id <- state$project$active_data_source_id %||% "dataset_example"
    sources <- state$project$data_sources %||% list()
    if (!any(vapply(sources, function(source) identical(source$id, "dataset_example"), logical(1)))) {
      sources <- c(list(bp_example_data_source()), sources)
    }
    source_ids <- vapply(sources, function(source) source$id %||% "", character(1))
    selected_id <- state$data_preview_source_id %||% active_id
    if (!selected_id %in% source_ids) selected_id <- if (active_id %in% source_ids) active_id else "dataset_example"
    source <- sources[[match(selected_id, source_ids)]]
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[selected_id]]
    source_choices <- stats::setNames(source_ids, vapply(sources, function(item) {
      if (isTRUE(item$example)) paste0(item$name %||% "df", " — Example data") else paste0(item$name %||% "data", " — ", item$original_file_name %||% "Imported data")
    }, character(1)))

    if (is.null(data) || !is.data.frame(data)) {
      return(htmltools::tagList(
        htmltools::tags$div(
          class = "bp-workspace-data-summary",
          htmltools::tags$div(
            class = "bp-workspace-data-source-picker",
            htmltools::tags$span("Preview dataset"),
            shiny::selectInput("data_preview_source_id", label = NULL, choices = source_choices, selected = selected_id, selectize = FALSE)
          ),
          htmltools::tags$span("View only · plot mapping unchanged")
        ),
        htmltools::tags$div(
          class = "bp-workspace-data-empty",
          bp_icon("warning", 26),
          htmltools::tags$strong("Data source needs to be linked"),
          htmltools::tags$p(paste0(
            "Import ", source$original_file_name %||% "the original data file",
            " again to preview its rows. You can still select df — Example data above."
          ))
        )
      ))
    }

    shown_rows <- min(30L, nrow(data))
    htmltools::tagList(
      htmltools::tags$div(
        class = "bp-workspace-data-summary",
        htmltools::tags$div(
          class = "bp-workspace-data-source-picker",
          htmltools::tags$span("Preview dataset"),
          shiny::selectInput("data_preview_source_id", label = NULL, choices = source_choices, selected = selected_id, selectize = FALSE)
        ),
        htmltools::tags$div(
          class = "bp-workspace-data-summary-meta",
          htmltools::tags$span("View only · plot mapping unchanged"),
          htmltools::tags$span(paste0(
            "Showing ", format(shown_rows, big.mark = ","), " of ", format(nrow(data), big.mark = ","),
            " rows · ", format(ncol(data), big.mark = ","), " columns"
          ))
        )
      ),
      bp_data_preview_table(data, rows = 30L, columns = ncol(data), row_numbers = TRUE)
    )
  })
  shiny::outputOptions(output, "active_data_preview", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$data_preview_source_id, {
    selected_id <- input$data_preview_source_id %||% ""
    valid_ids <- c("dataset_example", vapply(state$project$data_sources %||% list(), function(source) source$id %||% "", character(1)))
    current_id <- shiny::isolate(state$data_preview_source_id %||% "")
    if (selected_id %in% valid_ids && !identical(selected_id, current_id)) state$data_preview_source_id <- selected_id
  }, ignoreInit = TRUE)

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
  shiny::outputOptions(output, "generated_code_transport", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "project_state_transport", suspendWhenHidden = FALSE)

  repair_visual_boxplot_grouping <- function(record_history = TRUE) {
    if (!identical(bp_visual_chart_type(state$project), "boxplot")) return(FALSE)
    repaired <- bp_visual_remove_boxplot_group_mappings(state$project)
    if (!isTRUE(repaired$changed)) return(FALSE)
    config <- bp_visual_boxplot_config_from_project(repaired$project)
    repaired$project$visual_config <- repaired$project$visual_config %||% list()
    repaired$project$visual_config$active_chart_type <- "boxplot"
    repaired$project$visual_config$boxplot <- config
    root_index <- bp_visual_first_instance(repaired$project, "r.ggplot2.ggplot")
    selected <- if (!is.na(root_index)) repaired$project$modules[[root_index]]$instance_id else state$selected
    commit(
      repaired$project, selected,
      record_history = record_history,
      origin = "visual_group_repair"
    )
    sync_visual_inputs(repaired$project, config)
    if (isTRUE(input$visual_auto_preview)) start_preview()
    TRUE
  }

  shiny::observeEvent(state$project, {
    origin <- shiny::isolate(state$last_commit_origin)
    if (identical(origin, "visual_config")) return()
    if (identical(shiny::isolate(state$interface_mode), "visual") &&
        isTRUE(repair_visual_boxplot_grouping(record_history = FALSE))) return()
    project <- shiny::isolate(state$project)
    config <- bp_visual_config_from_project(project)
    repaired <- bp_visual_repair_cross_chart_labels(config, config$chart_type, project)
    if (!identical(visual_label_signature(config), visual_label_signature(repaired))) {
      result <- bp_apply_visual_config(project, repaired, registry)
      commit(result$project, result$root_instance_id, record_history = FALSE, origin = "visual_repair")
      sync_visual_inputs(result$project, result$config)
      if (isTRUE(input$visual_auto_preview)) start_preview()
      return()
    }
    sync_visual_inputs(project, config)
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$interface_mode, {
    mode <- input$interface_mode$value %||% input$interface_mode %||% "visual"
    state$interface_mode <- mode
    if (identical(mode, "visual")) repair_visual_boxplot_grouping(record_history = TRUE)
  }, ignoreInit = FALSE)

  shiny::observeEvent(input$visual_pca_expression_source, {
    if (isTRUE(state$visual_syncing)) return()
    if (identical(bp_visual_chart_type(state$project), "pca")) return()
    source_id <- input$visual_pca_expression_source %||% ""
    current_id <- state$project$active_data_source_id %||% "dataset_example"
    if (!nzchar(source_id) || identical(source_id, current_id)) return()
    sources <- Filter(function(source) identical(source$id, source_id), state$project$data_sources %||% list())
    if (!length(sources)) return()
    source <- sources[[1]]
    result <- tryCatch(switch_registered_data_source(state$project, source), error = identity)
    if (inherits(result, "error")) {
      state$visual_syncing <- TRUE
      shiny::updateSelectInput(session, "visual_pca_expression_source", selected = current_id)
      later::later(function() state$visual_syncing <- FALSE, 0.35)
      shiny::showNotification(conditionMessage(result), type = "warning", duration = 8)
      return()
    }
    result$project$visual_config <- result$project$visual_config %||% list()
    result$project$visual_config$pca <- visual_matrix_source_config_from_inputs(result$project, source$id)
    config <- bp_visual_config_from_project(result$project)
    result$project$visual_config$active_chart_type <- config$chart_type
    result$project$visual_config[[config$chart_type]] <- config
    commit(result$project, result$root_instance_id, origin = "visual_source")
    state$data_preview_source_id <- source$id
    state$last_data_switch <- result
    notify_data_source_switch(result)
    sync_visual_inputs(result$project, config)
    validation <- bp_validate_visual_config(config, result$columns)
    if (isTRUE(input$visual_auto_preview) && isTRUE(validation$valid)) start_preview()
  }, ignoreInit = TRUE)

  visual_matrix_source_reactive <- shiny::debounce(shiny::reactive(list(
    orientation = input$visual_pca_orientation,
    feature_id_field = input$visual_pca_feature_id_field,
    expression_sample_id_field = input$visual_pca_expression_sample_id_field,
    metadata_source_id = input$visual_pca_metadata_source,
    metadata_sample_id_field = input$visual_pca_metadata_id_field,
    unmatched_sample_policy = input$visual_pca_unmatched_policy
  )), 480)
  shiny::observeEvent(visual_matrix_source_reactive(), {
    if (isTRUE(state$visual_syncing)) return()
    if (identical(bp_visual_chart_type(state$project), "pca")) return()
    current <- bp_pca_config_from_project(state$project)
    expression_source_id <- state$project$active_data_source_id %||% "dataset_example"
    incoming <- visual_matrix_source_config_from_inputs(state$project, expression_source_id)
    if (identical(incoming, current)) return()
    project <- bp_clone_project(state$project)
    project$visual_config <- project$visual_config %||% list()
    project$visual_config$pca <- incoming
    commit(project, record_history = TRUE, origin = "visual_source_config")
  }, ignoreInit = TRUE)

  visual_recommendation_for <- function(chart_type, source, data) {
    if (identical(chart_type, "volcano")) {
      bp_visual_recommend_volcano_fields(source, data)
    } else if (identical(chart_type, "boxplot")) {
      bp_visual_recommend_boxplot_fields(source, data)
    } else {
      bp_visual_recommend_scatter_fields(source, data)
    }
  }

  apply_visual_recommendation <- function(config, recommendation) {
    config$x_field <- recommendation$x_field %||% ""
    config$y_field <- recommendation$y_field %||% ""
    config$color_field <- recommendation$color_field %||% ""
    config$x_scale <- recommendation$x_scale %||% config$x_scale
    config$y_scale <- recommendation$y_scale %||% config$y_scale
    config
  }

  switch_visual_chart <- function(
    chart_type, project = shiny::isolate(state$project), record_history = TRUE,
    origin = "visual_source", allow_example_fallback = FALSE
  ) {
    chart_type <- if (chart_type %in% c("scatter", "volcano", "boxplot", "pca")) chart_type else "scatter"
    workflow <- project$analysis_workflow_mode %||% "generic"
    if (identical(chart_type, "volcano") && !identical(workflow, "rna_seq")) {
      shiny::showNotification("火山图仅在 RNA-seq 引导中提供；请先切换工作流模式。", type = "warning")
      sync_visual_inputs(project, bp_visual_config_from_project(project))
      return(FALSE)
    }
    if (identical(bp_visual_chart_type(project), chart_type)) {
      sync_visual_inputs(project, bp_visual_config_from_project(project))
      return(TRUE)
    }
    if (identical(chart_type, "pca")) {
      config <- bp_normalize_pca_config(project$visual_config$pca %||% bp_pca_defaults(project), project)
      active <- visual_source(project)
      if (!nzchar(config$expression_source_id %||% "") || isTRUE(bp_pca_source(project, config$expression_source_id)$derived)) {
        config$expression_source_id <- if (!is.null(active) && !isTRUE(active$derived)) active$id else "dataset_example"
      }
      result <- bp_apply_visual_pca_config(project, config, registry)
      if (project$name %in% c("Untitled scatter plot", "Untitled volcano plot", "Untitled boxplot")) {
        result$project$name <- "Untitled PCA plot"
        shiny::updateTextInput(session, "project_name", value = result$project$name)
      }
      commit(result$project, result$root_instance_id, record_history = record_history, origin = origin)
      sync_visual_inputs(result$project, result$config)
      computed <- compute_pca_for_config(result$config, result$project)
      if (!isTRUE(computed$ok)) {
        shiny::showNotification(computed$error %||% "PCA 数据配置需要调整。", type = "warning", duration = 10)
      } else {
        cache_pca_result(computed, result$project)
        shiny::showNotification(
          if (identical(workflow, "rna_seq")) "已切换到 PCA 图；请检查样本关联和预处理设置。" else "已切换到降维图；当前使用 PCA 方法。",
          type = "message"
        )
        if (isTRUE(input$visual_auto_preview)) start_preview()
      }
      return(TRUE)
    }
    config <- project$visual_config[[chart_type]] %||% switch(
      chart_type,
      volcano = bp_visual_volcano_defaults(project),
      boxplot = bp_visual_boxplot_defaults(project),
      bp_visual_scatter_defaults(project)
    )
    config <- bp_visual_repair_cross_chart_labels(config, chart_type, project)
    config$chart_type <- chart_type
    active <- visual_source(project)
    target_source_id <- if (!is.null(active) && isTRUE(active$derived)) config$data_source_id %||% "dataset_example" else project$active_data_source_id %||% "dataset_example"
    source <- bp_pca_source(project, target_source_id)
    if (is.null(source) || isTRUE(source$derived)) source <- bp_example_data_source()
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source$id]]
    if (is.null(data) && isTRUE(allow_example_fallback)) {
      source <- bp_example_data_source()
      data <- bp_default_environment()$df
    }
    if (is.null(data)) return(FALSE)
    recommendation <- visual_recommendation_for(chart_type, source, data)
    config$data_source_id <- source$id
    columns <- bp_data_source_columns(source, data)
    if (!nzchar(config$x_field %||% "") || !config$x_field %in% columns ||
        !nzchar(config$y_field %||% "") || !config$y_field %in% columns) {
      config <- apply_visual_recommendation(config, recommendation)
    }
    if (nzchar(config$color_field %||% "") && !config$color_field %in% columns) config$color_field <- recommendation$color_field %||% ""
    result <- bp_apply_visual_config(project, config, registry)
    if (project$name %in% c("Untitled scatter plot", "Untitled volcano plot", "Untitled boxplot")) {
      result$project$name <- switch(chart_type, volcano = "Untitled volcano plot", boxplot = "Untitled boxplot", "Untitled scatter plot")
      shiny::updateTextInput(session, "project_name", value = result$project$name)
    }
    commit(result$project, result$root_instance_id, record_history = record_history, origin = origin)
    sync_visual_inputs(result$project, result$config)
    validation <- bp_validate_visual_config(result$config, columns)
    if (!isTRUE(validation$valid)) {
      shiny::showNotification(
        switch(
          chart_type,
          volcano = "已切换到火山图；请选择倍数变化和显著性字段。",
          boxplot = if (identical(workflow, "rna_seq")) "已切换到表达箱线图；请选择表达值和实验分组字段。" else "已切换到箱线图；请选择分组字段和数值字段。",
          if (identical(workflow, "rna_seq")) "已切换到基因表达散点图；请选择两个表达字段。" else "已切换到散点图；请选择 X 和 Y 字段。"
        ),
        type = "warning", duration = 8
      )
    } else if (isTRUE(input$visual_auto_preview)) start_preview()
    TRUE
  }

  shiny::observeEvent(input$visual_chart_scatter, switch_visual_chart("scatter"), ignoreInit = TRUE)
  shiny::observeEvent(input$visual_chart_volcano, switch_visual_chart("volcano"), ignoreInit = TRUE)
  shiny::observeEvent(input$visual_chart_boxplot, switch_visual_chart("boxplot"), ignoreInit = TRUE)
  shiny::observeEvent(input$visual_chart_pca, switch_visual_chart("pca"), ignoreInit = TRUE)

  shiny::observeEvent(input$visual_recommend_fields, {
    source <- visual_source(state$project)
    if (is.null(source)) return()
    data <- if (isTRUE(source$example)) bp_default_environment()$df else state$data_objects[[source$id]]
    config <- visual_config_from_inputs()
    recommendation <- visual_recommendation_for(config$chart_type, source, data)
    if (!nzchar(recommendation$x_field) || !nzchar(recommendation$y_field)) {
      shiny::showNotification(
        switch(
          config$chart_type,
          volcano = "未找到可识别的 logFC 与 PValue/FDR 数值列；仍可手动选择。",
          boxplot = "未找到可用的分组字段与数值字段；仍可手动选择。",
          "当前数据源中没有足够的数值列用于散点图。"
        ),
        type = "warning"
      )
      return()
    }
    config <- apply_visual_recommendation(config, recommendation)
    result <- bp_apply_visual_config(state$project, config, registry)
    commit(result$project, result$root_instance_id, origin = "visual_source")
    sync_visual_inputs(result$project, result$config)
    shiny::showNotification("已应用字段建议；可继续手动调整。", type = "message")
    if (isTRUE(input$visual_auto_preview)) start_preview()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_new_scatter, {
    chart_type <- bp_visual_chart_type(state$project)
    project <- bp_new_scatter_project(registry)
    source <- bp_example_data_source()
    recommendation <- visual_recommendation_for(chart_type, source, bp_default_environment()$df)
    config <- switch(
      chart_type,
      volcano = bp_visual_volcano_defaults(project),
      boxplot = bp_visual_boxplot_defaults(project),
      pca = bp_pca_defaults(project),
      bp_visual_scatter_config_from_project(project)
    )
    config$chart_type <- chart_type
    if (identical(chart_type, "pca")) {
      config$expression_source_id <- "dataset_example"
      config$expression_orientation <- "genes_by_samples"
      config$feature_id_location <- "column"
      config$feature_id_field <- "gene"
      config$transform <- "none"
      config$variable_feature_count <- "all"
    } else {
      config <- apply_visual_recommendation(config, recommendation)
    }
    config$title <- switch(chart_type, volcano = "Untitled volcano plot", boxplot = "Untitled boxplot", pca = "Untitled PCA plot", "Untitled scatter plot")
    project$name <- config$title
    result <- bp_apply_visual_config(project, config, registry)
    commit(result$project, result$root_instance_id, origin = "visual_source")
    state$data_objects <- list()
    state$data_preview_source_id <- "dataset_example"
    state$preview_status <- "initial"
    state$preview_result <- NULL
    state$preview_image <- NULL
    shiny::updateTextInput(session, "project_name", value = result$project$name)
    sync_visual_inputs(result$project, result$config)
    if (isTRUE(input$visual_auto_preview)) start_preview()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_box_jitter, {
    if (isTRUE(state$visual_syncing)) return()
    current <- visual_active_config(state$project)
    if (!identical(current$chart_type %||% "scatter", "boxplot")) return()
    requested <- isTRUE(input$visual_box_jitter)
    if (identical(requested, isTRUE(current$box_jitter))) return()

    config <- visual_config_from_inputs()
    config$box_jitter <- requested
    if (requested) {
      config$box_outlier_restore <- isTRUE(current$box_show_outliers)
      config$box_show_outliers <- FALSE
    } else {
      config$box_show_outliers <- isTRUE(current$box_outlier_restore)
    }

    result <- bp_apply_visual_config(state$project, config, registry)
    commit(result$project, result$root_instance_id, origin = "visual_config")
    sync_visual_inputs(result$project, result$config)
    if (isTRUE(input$visual_auto_preview)) start_preview()
  }, ignoreInit = TRUE, priority = 200)

  shiny::observeEvent(input$visual_pca_confirm_recipe, {
    if (!identical(bp_visual_chart_type(state$project), "pca")) return()
    config <- bp_normalize_pca_config(visual_config_from_inputs(), state$project)
    source <- bp_pca_source(state$project, config$expression_source_id)
    data <- if (is.null(source)) NULL else pca_source_data(source$id, state$project)
    if (is.null(source) || is.null(data)) {
      shiny::showNotification("Raw count 数据源不可用；请重新导入或链接。", type = "error")
      return()
    }
    if (!isTRUE(source$semantic_confirmed) || !identical(source$semantic_type, "raw_counts")) {
      shiny::showNotification("请先将表达数据语义确认为 Raw count。", type = "warning")
      return()
    }
    if (identical(config$raw_count_normalization, "tmm_logcpm") && !requireNamespace("edgeR", quietly = TRUE)) {
      shiny::showNotification("当前 R 环境未安装 edgeR；请选择 log2(count + 1) 快速探索配方。", type = "error", duration = NULL)
      return()
    }
    config$input_semantic_type <- "raw_counts"
    config$transform <- "none"
    config$raw_count_recipe_confirmed_signature <- bp_raw_count_recipe_signature(config)
    prepared <- prepare_pca_config(config, state$project)
    if (!isTRUE(prepared$result$ok)) {
      shiny::showNotification(prepared$result$error %||% "Raw count PCA 配方执行失败。", type = "error", duration = NULL)
      return()
    }
    result <- bp_apply_visual_pca_config(
      state$project, prepared$config, registry, analysis_result = prepared$result
    )
    commit(result$project, result$root_instance_id, origin = "analysis_recipe")
    cache_pca_result(prepared$result, result$project)
    sync_visual_inputs(result$project, result$config)
    preparation <- prepared$result$preparation %||% list()
    shiny::showNotification(
      paste0(
        "Raw count PCA 已生成：保留 ", preparation$retained_feature_count %||% prepared$result$selected_feature_count,
        " / ", preparation$original_feature_count %||% prepared$result$selected_feature_count,
        " 个特征；方法为 ", preparation$normalization_label %||% config$raw_count_normalization, "。"
      ),
      type = "message", duration = 9
    )
    start_preview()
  }, ignoreInit = TRUE, priority = 300)

  visual_config_reactive <- shiny::debounce(shiny::reactive(visual_config_from_inputs()), 480)
  shiny::observeEvent(visual_config_reactive(), {
    if (!identical(state$interface_mode, "visual")) return()
    if (isTRUE(state$visual_syncing)) return()
    if (isTRUE(state$visual_suppress_debounced)) {
      state$visual_suppress_debounced <- FALSE
      return()
    }
    raw_config <- visual_config_reactive()
    if (!grepl("^#[0-9A-Fa-f]{6}$", trimws(raw_config$point_color %||% ""))) {
      state$visual_input_error <- "invalid_color"
      return()
    }
    is_pca <- identical(raw_config$chart_type, "pca")
    if (!is_pca && !grepl("^#[0-9A-Fa-f]{6}$", trimws(raw_config$reference_line_color %||% ""))) {
      state$visual_input_error <- "invalid_reference_color"
      return()
    }
    if (identical(raw_config$chart_type, "boxplot") &&
        !grepl("^#[0-9A-Fa-f]{6}$", trimws(raw_config$box_border_color %||% ""))) {
      state$visual_input_error <- "invalid_box_border_color"
      return()
    }
    if (identical(raw_config$chart_type, "boxplot") && isTRUE(raw_config$box_jitter) &&
        !grepl("^#[0-9A-Fa-f]{6}$", trimws(raw_config$box_jitter_color %||% ""))) {
      state$visual_input_error <- "invalid_box_jitter_color"
      return()
    }
    if (!is_pca) {
      vertical_lines <- bp_visual_parse_reference_values(raw_config$vertical_reference_lines)
      horizontal_lines <- bp_visual_parse_reference_values(raw_config$horizontal_reference_lines)
      if (!isTRUE(vertical_lines$valid) || !isTRUE(horizontal_lines$valid)) {
        state$visual_input_error <- "invalid_reference_lines"
        return()
      }
    }
    state$visual_input_error <- NULL
    current <- visual_active_config(state$project)
    incoming <- visual_normalize_config(raw_config, state$project)
    incoming$advanced_preserved <- isTRUE(current$advanced_preserved)
    current <- visual_normalize_config(current, state$project)
    prepared <- if (is_pca) prepare_pca_config(incoming, state$project) else NULL
    if (is_pca) incoming <- prepared$config
    if (identical(incoming, current)) return()
    validation <- if (is_pca && isTRUE(prepared$result$ok)) {
      bp_validate_visual_config(incoming, names(prepared$result$scores))
    } else if (is_pca) {
      list(valid = FALSE, errors = prepared$result$error %||% "PCA 计算失败。")
    } else {
      bp_validate_visual_config(incoming, visual_current_columns())
    }
    result <- if (is_pca) {
      bp_apply_visual_pca_config(
        state$project, incoming, registry,
        analysis_result = if (isTRUE(prepared$result$ok)) prepared$result else NULL
      )
    } else {
      bp_apply_visual_config(state$project, incoming, registry)
    }
    commit(result$project, result$root_instance_id, origin = "visual_config")
    if (is_pca && isTRUE(prepared$result$ok)) cache_pca_result(prepared$result, result$project)
    if (is_pca && length(prepared$cleared)) {
      shiny::showNotification(paste0("数据源变化后清除了不可用的 PCA 映射：", paste(prepared$cleared, collapse = "、"), "。"), type = "warning")
    }
    if (isTRUE(input$visual_auto_preview) && isTRUE(validation$valid)) start_preview()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_run_preview, {
    config <- visual_config_from_inputs()
    prepared <- if (identical(config$chart_type %||% "scatter", "pca")) prepare_pca_config(config, state$project) else NULL
    if (!is.null(prepared)) config <- prepared$config
    validation <- visual_validation(config)
    if (!isTRUE(validation$valid)) {
      shiny::showNotification(paste(validation$errors, collapse = " "), type = "warning", duration = 8)
      return()
    }
    normalized <- visual_normalize_config(config, state$project)
    current <- visual_normalize_config(visual_active_config(state$project), state$project)
    if (!identical(normalized, current)) {
      result <- if (!is.null(prepared)) {
        bp_apply_visual_pca_config(
          state$project, normalized, registry,
          analysis_result = if (isTRUE(prepared$result$ok)) prepared$result else NULL
        )
      } else {
        bp_apply_visual_config(state$project, normalized, registry)
      }
      commit(result$project, result$root_instance_id, origin = "visual_config")
    }
    if (!is.null(prepared) && isTRUE(prepared$result$ok)) cache_pca_result(prepared$result, state$project)
    start_preview()
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
    state$data_objects <- list()
    state$data_preview_source_id <- project$active_data_source_id %||% "dataset_example"
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
    if (identical(payload$kind, "value") && identical(payload$param, "data") && identical(instance$module_id, "r.ggplot2.ggplot")) {
      sources <- if (identical(bp_value_type(argument$value), "RSymbol")) {
        Filter(function(source) identical(source$name, argument$value$name), project$data_sources %||% list())
      } else list()
      if (length(sources)) {
        source <- sources[[1]]
        result <- tryCatch(switch_registered_data_source(project, source), error = identity)
        if (inherits(result, "error")) {
          current_argument <- state$project$modules[[index]]$arguments[[payload$param]] %||% bp_argument(origin = parameter$source)
          restore_value <- bp_argument_input_value(current_argument, parameter)
          commit(bp_clone_project(state$project), instance$instance_id, record_history = FALSE)
          session$onFlushed(function() {
            session$sendCustomMessage("bp_restore_parameter_value", list(
              instance_id = instance$instance_id,
              param = payload$param,
              value = restore_value
            ))
          }, once = TRUE)
          shiny::showNotification(conditionMessage(result), type = "warning", duration = 8)
          return()
        }
        commit(result$project, result$root_instance_id)
        state$data_preview_source_id <- source$id
        state$last_data_switch <- result
        notify_data_source_switch(result)
        return()
      }
      commit(project, instance$instance_id)
      state$last_data_switch <- list(custom_expression = TRUE, source = bp_value_to_source(argument$value))
      notify_custom_data_expression()
      return()
    }
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
    if (identical(edit$parameter, "data") && identical(project$modules[[index]]$module_id, "r.ggplot2.ggplot")) {
      state$last_data_switch <- list(custom_expression = TRUE, source = value)
      notify_custom_data_expression()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$undo, {
    history <- state$history
    if (!length(history)) return()
    previous <- history[[length(history)]]
    state$future <- c(list(bp_clone_project(state$project)), state$future)
    state$history <- history[-length(history)]
    commit(previous, state$selected, record_history = FALSE)
    state$data_preview_source_id <- previous$active_data_source_id %||% "dataset_example"
    state$last_data_switch <- NULL
    shiny::updateTextInput(session, "project_name", value = previous$name)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$redo, {
    future <- state$future
    if (!length(future)) return()
    next_project <- future[[1]]
    state$history <- c(state$history, list(bp_clone_project(state$project)))
    state$future <- future[-1]
    commit(next_project, state$selected, record_history = FALSE)
    state$data_preview_source_id <- next_project$active_data_source_id %||% "dataset_example"
    state$last_data_switch <- NULL
    shiny::updateTextInput(session, "project_name", value = next_project$name)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_undo, {
    history <- state$history
    if (!length(history)) return()
    previous <- history[[length(history)]]
    state$future <- c(list(bp_clone_project(state$project)), state$future)
    state$history <- history[-length(history)]
    commit(previous, state$selected, record_history = FALSE, origin = "visual_history")
    state$data_preview_source_id <- previous$active_data_source_id %||% "dataset_example"
    state$last_data_switch <- NULL
    shiny::updateTextInput(session, "project_name", value = previous$name)
    if (isTRUE(input$visual_auto_preview)) start_preview()
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_redo, {
    future <- state$future
    if (!length(future)) return()
    next_project <- future[[1]]
    state$history <- c(state$history, list(bp_clone_project(state$project)))
    state$future <- future[-1]
    commit(next_project, state$selected, record_history = FALSE, origin = "visual_history")
    state$data_preview_source_id <- next_project$active_data_source_id %||% "dataset_example"
    state$last_data_switch <- NULL
    shiny::updateTextInput(session, "project_name", value = next_project$name)
    if (isTRUE(input$visual_auto_preview)) start_preview()
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
    commit(project, project$modules[[1]]$instance_id)
    state$data_objects <- list()
    state$data_preview_source_id <- project$active_data_source_id %||% "dataset_example"
    shiny::updateTextInput(session, "project_name", value = project$name)
    state$expression_edit <- NULL
    state$preview_status <- "initial"
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$restore_project, {
    payload <- input$restore_project
    restored <- tryCatch({
      envelope <- jsonlite::fromJSON(payload$json %||% "", simplifyVector = FALSE)
      project <- bp_mark_data_sources_for_relink(bp_migrate_project(envelope$project %||% envelope))
      bp_validate_project(project, registry)
      list(project = project, selected = envelope$selected %||% NULL)
    }, error = identity)

    if (inherits(restored, "error")) {
      session$onFlushed(function() {
        session$sendCustomMessage(
          "bp_project_restore_status",
          list(ok = FALSE, message = conditionMessage(restored))
        )
      }, once = TRUE)
      return()
    }

    process <- state$preview_process
    if (!is.null(process) && process$is_alive()) process$kill()
    state$preview_process <- NULL
    state$preview_pending_image <- NULL

    commit(restored$project, restored$selected, record_history = FALSE)
    state$data_objects <- list()
    state$data_preview_source_id <- restored$project$active_data_source_id %||% "dataset_example"
    state$history <- list()
    state$future <- list()
    state$expression_edit <- NULL
    state$preview_status <- "initial"
    state$preview_result <- NULL
    shiny::updateTextInput(session, "project_name", value = restored$project$name)
    relink <- Filter(function(source) identical(source$status, "relink_required"), restored$project$data_sources %||% list())
    if (length(relink)) {
      shiny::showNotification(
        paste0("Project metadata restored. Re-import ", relink[[1]]$original_file_name, " to relink data source '", relink[[1]]$name, "'."),
        type = "warning", duration = NULL
      )
    }
    active_source <- Filter(function(source) identical(source$id, restored$project$active_data_source_id), restored$project$data_sources %||% list())
    can_preview <- length(active_source) && !identical(active_source[[1]]$status, "relink_required") && !isTRUE(active_source[[1]]$relink_required)
    session$onFlushed(function() {
      session$sendCustomMessage("bp_project_restore_status", list(ok = TRUE))
      if (isTRUE(can_preview)) start_preview()
    }, once = TRUE)
  }, ignoreInit = FALSE, ignoreNULL = TRUE, priority = 1000)

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
    state$data_objects <- list()
    state$data_preview_source_id <- parsed$active_data_source_id %||% "dataset_example"
    shiny::updateTextInput(session, "project_name", value = parsed$name)
    shiny::removeModal()
    if (identical(parsed$parse_support, "D")) {
      shiny::showNotification("The source was preserved as Raw R because its outer structure is unsupported.", type = "warning", duration = NULL)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$project_file, {
    shiny::req(input$project_file$datapath)
    project <- tryCatch(bp_mark_data_sources_for_relink(bp_load_project(input$project_file$datapath)), error = identity)
    if (inherits(project, "error")) {
      shiny::showNotification(conditionMessage(project), type = "error", duration = NULL)
      return()
    }
    commit(project, if (length(project$modules)) project$modules[[1]]$instance_id else NULL)
    state$data_objects <- list()
    state$data_preview_source_id <- project$active_data_source_id %||% "dataset_example"
    shiny::updateTextInput(session, "project_name", value = project$name)
    shiny::showNotification("Project restored with versioned module state.", type = "message")
    relink <- Filter(function(source) identical(source$status, "relink_required"), project$data_sources %||% list())
    if (length(relink)) shiny::showNotification("Imported data metadata was restored; re-import the original file to relink its contents.", type = "warning", duration = NULL)
    active_source <- Filter(function(source) identical(source$id, project$active_data_source_id), project$data_sources %||% list())
    if (length(active_source) && !identical(active_source[[1]]$status, "relink_required") && !isTRUE(active_source[[1]]$relink_required)) start_preview()
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

  make_pca_csv_download <- function(kind) shiny::downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9_-]+", "-", state$project$name), "-pca-", kind, ".csv"),
    content = function(file) {
      result <- shiny::isolate(compute_pca_for_config(bp_pca_config_from_project(state$project), state$project))
      if (!isTRUE(result$ok)) stop(result$error %||% "PCA 计算失败。", call. = FALSE)
      utils::write.csv(result[[kind]], file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$download_pca_scores <- make_pca_csv_download("scores")
  output$download_pca_loadings <- make_pca_csv_download("loadings")
  output$download_pca_normalized <- make_pca_csv_download("normalized_expression")
  output$visual_pca_normalized_export <- shiny::renderUI({
    result <- pca_result()
    if (is.null(result) || !isTRUE(result$ok) || !is.data.frame(result$normalized_expression)) return(NULL)
    shiny::downloadButton("download_pca_normalized", "导出标准化表达 CSV", class = "bp-command-button")
  })
  shiny::outputOptions(output, "download_pca_scores", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "download_pca_loadings", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "download_pca_normalized", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "visual_pca_normalized_export", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$run_preview, start_preview(), ignoreInit = TRUE)

  shiny::observeEvent(input$cancel_preview, {
    process <- state$preview_process
    if (!is.null(process) && process$is_alive()) {
      process$kill()
      state$preview_process <- NULL
      state$preview_status <- "cancelled"
      state$preview_result <- list(ok = FALSE, error = NULL, warnings = list(), messages = list())
      state$preview_pending_image <- NULL
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
      pending_image <- state$preview_pending_image
      if (isTRUE(result$ok) && !is.null(pending_image) && file.exists(pending_image)) {
        state$preview_image <- pending_image
        state$preview_status <- "success"
      } else {
        state$preview_status <- "error"
      }
    } else {
      stderr <- tryCatch(process$read_all_error(), error = function(error) "")
      state$preview_result <- list(ok = FALSE, error = if (nzchar(stderr)) stderr else "The R preview process exited without a result.", warnings = list(), messages = list())
      state$preview_status <- "error"
    }
    state$preview_pending_image <- NULL
    state$preview_process <- NULL
  })

  session$onFlushed(function() start_preview(), once = TRUE)
  session$onSessionEnded(function() {
    process <- shiny::isolate(state$preview_process)
    if (!is.null(process) && process$is_alive()) process$kill()
  })
  invisible(state)
}
