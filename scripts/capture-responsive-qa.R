#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
value_after <- function(flag, default) {
  index <- match(flag, args)
  if (is.na(index) || index == length(args)) default else args[[index + 1L]]
}

url <- value_after("--url", "http://127.0.0.1:3838")
out_dir <- value_after("--out-dir", tempdir())
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
desktop_path <- file.path(out_dir, "bioplotblocks-desktop-qa.png")
mobile_path <- file.path(out_dir, "bioplotblocks-mobile-qa.png")

desktop <- chromote::ChromoteSession$new(width = 1536, height = 1024)
on.exit(if (desktop$is_active()) desktop$close(), add = TRUE)
desktop$go_to(url, delay = 4)
desktop$screenshot(desktop_path, cliprect = c(0, 0, 1536, 1024), delay = 1)
desktop_state <- desktop$Runtime$evaluate(
  "JSON.stringify({width:innerWidth,height:innerHeight,layers:document.querySelectorAll('.bp-layer-card').length,preview:Boolean(document.querySelector('.bp-preview-canvas img')),errors:document.querySelector('.bp-error-status')?.textContent.trim()})",
  returnByValue = TRUE
)$result$value
desktop$close()

mobile <- chromote::ChromoteSession$new(width = 390, height = 844, mobile = TRUE)
on.exit(if (mobile$is_active()) mobile$close(), add = TRUE)
mobile$go_to(url, delay = 4)
mobile$screenshot(mobile_path, cliprect = c(0, 0, 390, 844), delay = 1)
mobile_state <- mobile$Runtime$evaluate(
  "JSON.stringify({width:innerWidth,height:innerHeight,bodyClient:document.body.clientWidth,bodyScroll:document.body.scrollWidth,workspace:getComputedStyle(document.querySelector('.bp-workspace')).display,runVisible:Boolean(document.getElementById('run_preview')?.getBoundingClientRect().width)})",
  returnByValue = TRUE
)$result$value
mobile$close()

cat("DESKTOP_STATE", desktop_state, "\n")
cat("MOBILE_STATE", mobile_state, "\n")
cat("DESKTOP_SCREENSHOT", normalizePath(desktop_path, winslash = "/"), "\n")
cat("MOBILE_SCREENSHOT", normalizePath(mobile_path, winslash = "/"), "\n")
