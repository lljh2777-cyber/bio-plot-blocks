root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (!file.exists(file.path(root, "DESCRIPTION"))) root <- normalizePath(file.path(root, "..", ".."), winslash = "/")
options(BioPlotBlocks.root = root)

source_order <- c(
  "ir-nodes.R", "module-registry.R", "module-instance.R", "data-sources.R", "pca.R", "visual-config.R", "codegen.R",
  "project-store.R", "parser.R", "diagnostics.R", "runtime.R", "templates.R",
  "ui-bindings.R"
)
for (file in source_order) source(file.path(root, "R", file), local = globalenv())

registry <- bp_load_registry()
