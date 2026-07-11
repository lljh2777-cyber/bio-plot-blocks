bp_action_button <- function(id, label, icon, primary = FALSE, class = NULL, title = NULL) {
  shiny::actionButton(
    id,
    label = htmltools::tagList(bp_icon(icon, 17), htmltools::tags$span(class = "bp-command-label", label)),
    class = paste("bp-command-button", if (primary) "bp-command-primary", class),
    title = title
  )
}

bp_workspace_ui <- function(root) {
  shiny::fluidPage(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "utf-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      htmltools::tags$title("BioPlotBlocks"),
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
          class = "bp-environment-lock",
          htmltools::tags$span(class = "bp-live-dot", `aria-hidden` = "true"),
          htmltools::tags$span("R 4.5.1 · ggplot2 4.0.1")
        ),
        htmltools::tags$nav(
          class = "bp-command-bar",
          `aria-label` = "Project commands",
          bp_action_button("new_project", "New", "plus"),
          bp_action_button("import_r", "Import R", "import"),
          bp_action_button("run_preview", "Run preview", "play", primary = TRUE, title = "Run preview (Ctrl+Enter)"),
          shiny::downloadButton(
            "download_project",
            label = htmltools::tagList(bp_icon("save", 17), htmltools::tags$span(class = "bp-command-label", "Save")),
            class = "bp-command-button"
          ),
          shiny::downloadButton(
            "download_r",
            label = htmltools::tagList(bp_icon("export", 17), htmltools::tags$span(class = "bp-command-label", "Export")),
            class = "bp-command-button"
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
        class = "bp-workspace",
        htmltools::tags$aside(
          class = "bp-panel bp-library-panel",
          `aria-label` = "Module library",
          htmltools::tags$div(
            class = "bp-library-search",
            htmltools::tags$span(class = "bp-search-icon", bp_icon("search", 17)),
            shiny::textInput("module_search", label = NULL, value = "", placeholder = "Search functions")
          ),
          shiny::uiOutput("library_filters"),
          htmltools::tags$div(
            class = "bp-library-heading",
            htmltools::tags$span("Function library"),
            htmltools::tags$span(class = "bp-library-heading-meta", "Package · Status")
          ),
          shiny::uiOutput("module_library"),
          shiny::uiOutput("template_library")
        ),
        htmltools::tags$section(
          class = "bp-panel bp-stack-panel",
          `aria-label` = "Layer stack",
          htmltools::tags$div(
            class = "bp-panel-titlebar",
            htmltools::tags$h2("Layer stack"),
            htmltools::tags$div(
              class = "bp-panel-actions",
              bp_action_button("undo", "Undo", "undo", title = "Undo (Ctrl+Z)"),
              bp_action_button("redo", "Redo", "redo", title = "Redo (Ctrl+Y)"),
              htmltools::tags$button(
                type = "button",
                class = "bp-command-button bp-focus-library",
                htmltools::tagList(bp_icon("plus", 17), htmltools::tags$span("Add layer"))
              )
            )
          ),
          shiny::uiOutput("assignment_editor"),
          shiny::uiOutput("layer_stack")
        ),
        htmltools::tags$aside(
          class = "bp-panel bp-inspector-panel",
          `aria-label` = "Parameter inspector",
          shiny::uiOutput("parameter_inspector")
        )
      ),
      htmltools::tags$section(
        class = "bp-lower-workspace",
        htmltools::tags$article(
          class = "bp-panel bp-preview-panel",
          htmltools::tags$div(
            class = "bp-panel-titlebar bp-lower-titlebar",
            htmltools::tags$h2("Preview"),
            htmltools::tags$div(
              class = "bp-panel-actions",
              shiny::actionButton(
                "cancel_preview",
                label = htmltools::tagList(bp_icon("close", 15), htmltools::tags$span("Cancel")),
                class = "bp-command-button bp-cancel-preview"
              ),
              htmltools::tags$span(class = "bp-preview-dimensions", "920 × 540 · 120 dpi")
            )
          ),
          htmltools::tags$div(
            class = "bp-preview-canvas",
            shiny::uiOutput("preview_image"),
            shiny::uiOutput("preview_overlay")
          )
        ),
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
          shiny::uiOutput("generated_code_transport")
        )
      ),
      htmltools::tags$footer(
        class = "bp-statusbar",
        shiny::uiOutput("status_bar")
      )
    ),
    htmltools::tags$div(
      class = "bp-hidden-file-input",
      shiny::fileInput("project_file", label = NULL, accept = c("application/json", ".json"))
    )
  )
}
