#' Create the deterministic local example environment
#'
#' @return Environment containing `df`, a palette, and ggplot2 exports.
#' @export
bp_default_environment <- function() {
  environment <- new.env(parent = baseenv())
  set.seed(20260711)
  n <- 420L
  log2fc <- stats::rnorm(n, sd = 1.18)
  signal <- abs(log2fc) + stats::rexp(n, rate = 1.2)
  neg_log10 <- pmin(8, pmax(0.05, signal + stats::rnorm(n, sd = 0.35)))
  status <- ifelse(log2fc <= -1 & neg_log10 >= -log10(0.05), "Down",
    ifelse(log2fc >= 1 & neg_log10 >= -log10(0.05), "Up", "NS")
  )
  group <- factor(rep(c("Control", "Treatment A", "Treatment B"), length.out = n))

  environment$df <- data.frame(
    gene = paste0("GENE", seq_len(n)),
    log2FC = log2fc,
    neg_log10_padj = neg_log10,
    padj = 10^(-neg_log10),
    status = factor(status, levels = c("Down", "NS", "Up")),
    group = group,
    expression = exp(stats::rnorm(n, 2.2, 0.55)),
    value = stats::rnorm(n, as.numeric(group), 0.6),
    PC1 = log2fc * 2 + stats::rnorm(n),
    PC2 = stats::rnorm(n, as.numeric(group), 1.2),
    condition = group,
    stringsAsFactors = FALSE
  )
  environment$palette <- c(Down = "#2C7FB8", NS = "grey70", Up = "#D73027")

  exports <- getNamespaceExports("ggplot2")
  for (symbol in exports) {
    assign(symbol, getExportedValue("ggplot2", symbol), envir = environment)
  }
  environment
}

#' Execute a project using the real local ggplot2 runtime
#'
#' @param project BioPlotBlocks project.
#' @param registry Optional registry.
#' @param environment Evaluation environment.
#' @param timeout_seconds Local execution time limit.
#' @return Structured plot result and diagnostics.
#' @export
bp_execute_project <- function(
    project,
    registry = NULL,
    environment = bp_default_environment(),
    timeout_seconds = 12) {
  registry <- registry %||% bp_load_registry()
  warnings <- character()
  messages <- character()
  started <- Sys.time()

  result <- tryCatch(
    withCallingHandlers(
      {
        setTimeLimit(elapsed = timeout_seconds, transient = TRUE)
        expression <- bp_plot_language(project, registry)
        plot <- eval(expression, envir = environment)
        if (!inherits(plot, "ggplot")) {
          stop("The generated expression did not return a ggplot object.", call. = FALSE)
        }
        plot
      },
      warning = function(condition) {
        warnings <<- c(warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      },
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    error = identity,
    finally = setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  )

  error <- if (inherits(result, "error")) conditionMessage(result) else NULL
  plot <- if (inherits(result, "error")) NULL else result
  if (bp_has_raw_expression(project)) {
    warnings <- unique(c(
      warnings,
      "Raw R Expressions are preserved verbatim and execute with local user permissions."
    ))
  }

  list(
    ok = is.null(error),
    plot = plot,
    error = error,
    warnings = unique(warnings),
    messages = unique(messages),
    elapsed_ms = round(as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000),
    versions = list(
      r = paste(R.version$major, R.version$minor, sep = "."),
      ggplot2 = as.character(utils::packageVersion("ggplot2"))
    )
  )
}

bp_render_preview_to_files <- function(project, root, status_path, image_path) {
  options(BioPlotBlocks.root = root)
  result <- bp_execute_project(project)
  if (isTRUE(result$ok)) {
    grDevices::png(
      filename = image_path,
      width = project$settings$preview_width %||% 920,
      height = project$settings$preview_height %||% 540,
      res = project$settings$preview_dpi %||% 120,
      bg = "white"
    )
    print(result$plot)
    grDevices::dev.off()
  }
  serializable <- result
  serializable$plot <- NULL
  jsonlite::write_json(serializable, status_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(serializable)
}

bp_start_preview_process <- function(project, root, status_path, image_path) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop("The callr package is required for cancellable preview execution.", call. = FALSE)
  }
  callr::r_bg(
    function(project, root, status_path, image_path) {
      options(BioPlotBlocks.root = root)
      files <- list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
      priority <- c(
        "ir-nodes.R", "module-registry.R", "module-instance.R", "codegen.R",
        "parser.R", "project-store.R", "diagnostics.R", "runtime.R", "templates.R",
        "ui-bindings.R"
      )
      files <- files[order(match(basename(files), priority, nomatch = length(priority) + 1L))]
      for (file in files) sys.source(file, envir = globalenv())
      bp_render_preview_to_files(project, root, status_path, image_path)
    },
    args = list(
      project = project,
      root = root,
      status_path = status_path,
      image_path = image_path
    ),
    supervise = TRUE,
    stdout = "|",
    stderr = "|"
  )
}
