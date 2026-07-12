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
          cells <- c(list(htmltools::tags$th(class = "bp-data-row-number", scope = "row", row)), cells)
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
  preview <- if (length(supported)) {
    object <- imported$objects[[supported[[1]]$name]]
    tryCatch(bp_data_preview_table(bp_convert_r_object(object), rows = 12L), error = function(error) htmltools::tags$p(class = "bp-import-error", conditionMessage(error)))
  }
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
    if (!is.null(preview)) htmltools::tags$details(class = "bp-import-section", open = "open", htmltools::tags$summary("First supported object preview"), preview),
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
  initial <- bp_project_from_template("bio.volcano.basic", registry)
  selected_initial <- initial$modules[[2]]$instance_id
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
    preview_status_file = NULL,
    data_objects = list(),
    data_import = NULL,
    data_preview_source_id = "dataset_example",
    pending_mapping_source_id = NULL,
    pending_rename_source_id = NULL,
    last_data_switch = NULL
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

  start_preview <- function() {
    process <- shiny::isolate(state$preview_process)
    if (!is.null(process) && process$is_alive()) process$kill()
    status_path <- tempfile("bioplotblocks-preview-", fileext = ".json")
    image_path <- tempfile("bioplotblocks-preview-", fileext = ".png")
    state$preview_status <- "running"
    state$preview_result <- NULL
    state$preview_status_file <- status_path
    state$preview_image <- image_path
    active_id <- shiny::isolate(state$project$active_data_source_id %||% "dataset_example")
    active_sources <- Filter(function(source) identical(source$id, active_id), shiny::isolate(state$project$data_sources %||% list()))
    if (length(active_sources) && !isTRUE(active_sources[[1]]$example) && is.null(shiny::isolate(state$data_objects[[active_id]]))) {
      state$preview_status <- "error"
      state$preview_result <- list(
        ok = FALSE,
        error = paste0("Data source '", active_sources[[1]]$name, "' must be re-linked by importing ", active_sources[[1]]$original_file_name, " again."),
        warnings = list(), messages = list()
      )
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
    } else {
      state$preview_process <- process
    }
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
        ready <- !identical(source$status, "relink_required") && !isTRUE(source$relink_required) && (isTRUE(source$example) || !is.null(state$data_objects[[source$id]]))
        htmltools::tags$article(
          class = paste("bp-data-source-card", if (is_active) "is-active", if (!ready) "needs-relink"),
          htmltools::tags$div(
            class = "bp-data-source-card-main",
            htmltools::tags$div(
              htmltools::tags$strong(source$name),
              if (is_active) htmltools::tags$span(class = "bp-active-source-label", "Active plot data"),
              if (isTRUE(source$example)) htmltools::tags$span(class = "bp-example-source-label", "Example")
            ),
            htmltools::tags$p(paste0(
              toupper(source$source_type %||% "data"), " · ", format(source$rows %||% 0L, big.mark = ","), " rows × ", source$columns %||% 0L, " columns",
              if (!is.null(source$object_name) && !identical(source$object_name, source$name)) paste0(" · object ", source$object_name) else ""
            )),
            htmltools::tags$small(if (ready) source$original_file_name %||% "Built-in deterministic dataset" else paste0("Relink required: ", source$original_file_name %||% "original file"))
          ),
          htmltools::tags$div(
            class = "bp-data-source-card-actions",
            action_button(source, "preview", "Preview", disabled = !ready),
            action_button(source, "map", if (is_active) "Remap" else "Use in plot", disabled = !ready),
            if (!isTRUE(source$example)) action_button(source, "rename", "Rename"),
            if (!ready && !isTRUE(source$example)) action_button(source, "relink", "Relink"),
            if (!isTRUE(source$example)) action_button(source, "remove", "Remove", disabled = is_active, danger = TRUE)
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
      htmltools::tags$details(class = "bp-import-section", open = "open", htmltools::tags$summary("Data preview · first 30 rows"), bp_data_preview_table(imported$data)),
      htmltools::tags$details(class = "bp-import-section", htmltools::tags$summary("Column types and quality"), bp_data_column_table(profile)),
      if (length(warnings)) htmltools::tags$div(
        class = "bp-data-quality-warnings",
        htmltools::tags$strong(paste("Quality checks ·", length(warnings))),
        htmltools::tags$ul(lapply(warnings, function(warning) htmltools::tags$li(class = paste0("is-", warning$level), warning$message)))
      ),
      htmltools::tags$div(class = "bp-mapping-title", htmltools::tags$strong("Manual column mapping"), htmltools::tags$span("Mappings are confirmed only when you click Use data in plot.")),
      bp_data_mapping_controls(imported, state$project)
    )
  })

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
    if (!nzchar(mapping$x) || !nzchar(mapping$y)) {
      shiny::showNotification("Choose both X and Y columns.", type = "error")
      return()
    }
    updated_profile <- bp_profile_dataset(data)
    metadata_by_name <- stats::setNames(updated_profile$column_metadata, names(data))
    for (role in c("size", "alpha")) {
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
      return(htmltools::tags$div(
        class = "bp-preview-overlay bp-preview-error",
        bp_icon("warning", 28),
        htmltools::tags$strong("Preview failed"),
        htmltools::tags$p(
          role = "region",
          tabindex = "0",
          `aria-label` = "Preview error details",
          state$preview_result$error %||% "Unknown R error"
        )
      ))
    }
    if (identical(status, "cancelled")) {
      return(htmltools::tags$div(class = "bp-preview-overlay", bp_icon("info", 28), htmltools::tags$strong("Preview cancelled"), htmltools::tags$p("Your module state and code were not changed.")))
    }
    htmltools::tags$div(class = "bp-preview-overlay", bp_icon("plot", 30), htmltools::tags$strong("Preview ready"), htmltools::tags$p("Run the current module stack with local R."))
  })

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
    session$onFlushed(function() {
      session$sendCustomMessage("bp_project_restore_status", list(ok = TRUE))
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
  invisible(state)
}
