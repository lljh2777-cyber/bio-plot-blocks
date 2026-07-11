#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
url_index <- match("--url", args)
url <- if (!is.na(url_index) && url_index < length(args)) args[[url_index + 1L]] else "http://127.0.0.1:3838"

browser <- chromote::ChromoteSession$new(width = 1280, height = 720)
on.exit(if (browser$is_active()) browser$close(), add = TRUE)
browser$go_to(url, delay = 4)

edit_state <- browser$Runtime$evaluate(
  "(async () => { const input = document.querySelector('input[aria-label=\"Value for alpha\"]'); if (!input) throw new Error('alpha input missing'); input.value = '0.85'; input.dispatchEvent(new Event('input', {bubbles:true})); input.dispatchEvent(new Event('change', {bubbles:true})); await new Promise(resolve => setTimeout(resolve, 900)); const code = document.querySelector('.bp-code-editor')?.textContent || ''; return {codeUpdated:code.includes('alpha = 0.85'), layers:document.querySelectorAll('.bp-layer-card').length}; })()",
  awaitPromise = TRUE,
  returnByValue = TRUE
)$result$value

add_state <- browser$Runtime$evaluate(
  "(async () => { const button = document.querySelector('[data-module-id=\"r.ggplot2.labs\"]'); if (!button) throw new Error('labs button missing'); button.click(); await new Promise(resolve => setTimeout(resolve, 650)); return {layers:document.querySelectorAll('.bp-layer-card').length, codeLines:document.querySelectorAll('.bp-code-line').length}; })()",
  awaitPromise = TRUE,
  returnByValue = TRUE
)$result$value

undo_state <- browser$Runtime$evaluate(
  "(async () => { document.getElementById('undo').click(); await new Promise(resolve => setTimeout(resolve, 650)); return {layers:document.querySelectorAll('.bp-layer-card').length, codeLines:document.querySelectorAll('.bp-code-line').length, errors:document.querySelector('.bp-error-status')?.textContent.trim()}; })()",
  awaitPromise = TRUE,
  returnByValue = TRUE
)$result$value

cat("LIVE_EDIT", jsonlite::toJSON(edit_state, auto_unbox = TRUE), "\n")
cat("ADD_MODULE", jsonlite::toJSON(add_state, auto_unbox = TRUE), "\n")
cat("UNDO", jsonlite::toJSON(undo_state, auto_unbox = TRUE), "\n")

stopifnot(isTRUE(edit_state$codeUpdated))
stopifnot(identical(add_state$layers, 8L), identical(add_state$codeLines, 8L))
stopifnot(identical(undo_state$layers, 7L), identical(undo_state$codeLines, 7L), identical(undo_state$errors, "0 errors"))
