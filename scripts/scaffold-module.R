#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/")
root <- dirname(dirname(script_path))
source(file.path(root, "skills", "r-function-to-module", "scripts", "inspect_function.R"), chdir = TRUE)
