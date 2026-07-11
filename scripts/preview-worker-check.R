#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/")
root <- dirname(dirname(script_path))
options(BioPlotBlocks.root = root)

source_order <- c(
  "ir-nodes.R", "module-registry.R", "module-instance.R", "codegen.R",
  "project-store.R", "parser.R", "diagnostics.R", "runtime.R", "templates.R",
  "ui-bindings.R"
)
for (file in source_order) source(file.path(root, "R", file))

project <- bp_project_from_template("bio.volcano.basic")
status_path <- tempfile(fileext = ".json")
image_path <- tempfile(fileext = ".png")
process <- bp_start_preview_process(project, root, status_path, image_path)
process$wait(timeout = 30000)

if (process$is_alive()) process$kill()
if (!identical(process$get_exit_status(), 0L)) {
  stop("Preview worker failed: ", process$read_all_error())
}
stopifnot(file.exists(status_path), file.exists(image_path), file.info(image_path)$size > 1000)
result <- jsonlite::fromJSON(status_path, simplifyVector = FALSE)
stopifnot(isTRUE(result$ok))
cat("Cancellable preview worker rendered", file.info(image_path)$size, "bytes.\n")
