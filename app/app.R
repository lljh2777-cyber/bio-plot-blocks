library(shiny)

working <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
app_dir <- if (basename(working) == "app") working else file.path(working, "app")
root <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)
options(BioPlotBlocks.root = root)

source_order <- c(
  "ir-nodes.R", "module-registry.R", "module-instance.R", "codegen.R",
  "project-store.R", "parser.R", "diagnostics.R", "runtime.R", "templates.R",
  "ui-bindings.R"
)
for (file in source_order) {
  sys.source(file.path(root, "R", file), envir = globalenv())
}
sys.source(file.path(app_dir, "modules", "help-ui.R"), envir = globalenv())
sys.source(file.path(app_dir, "modules", "workspace-ui.R"), envir = globalenv())
sys.source(file.path(app_dir, "modules", "workspace-server.R"), envir = globalenv())

registry <- bp_load_registry()
templates <- bp_load_template()

ui <- bp_workspace_ui(root)

server <- function(input, output, session) {
  bp_workspace_server(input, output, session, registry, templates, root)
}

shinyApp(ui, server)
