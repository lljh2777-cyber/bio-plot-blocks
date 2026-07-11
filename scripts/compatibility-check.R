#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/")
root <- dirname(dirname(script_path))
matrix <- jsonlite::fromJSON(file.path(root, "config", "compatibility-matrix.json"), simplifyVector = FALSE)

active_r <- paste(R.version$major, R.version$minor, sep = ".")
active_ggplot2 <- as.character(utils::packageVersion("ggplot2"))
locked <- matrix$locked_environment

if (!identical(active_r, locked$r)) stop("R version mismatch: expected ", locked$r, ", got ", active_r)
if (!identical(active_ggplot2, locked$ggplot2)) stop("ggplot2 version mismatch: expected ", locked$ggplot2, ", got ", active_ggplot2)

cat("Compatibility lock confirmed: R", active_r, "and ggplot2", active_ggplot2, "\n")
