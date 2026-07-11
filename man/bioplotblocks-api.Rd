\name{bioplotblocks-api}
\alias{bp_argument}
\alias{bp_call_value}
\alias{bp_create_project}
\alias{bp_default_environment}
\alias{bp_execute_project}
\alias{bp_generate_code}
\alias{bp_generate_lines}
\alias{bp_get_spec}
\alias{bp_instantiate_module}
\alias{bp_load_project}
\alias{bp_load_registry}
\alias{bp_load_template}
\alias{bp_parse_code}
\alias{bp_project_from_template}
\alias{bp_save_project}
\alias{bp_validate_module_spec}
\alias{bp_validate_project}
\title{BioPlotBlocks semantic, registry, project, and execution API}
\usage{
bp_argument(state = "unset", value = NULL, origin = "formal",
  source_text = NULL, position = NULL, dynamic = FALSE)

bp_call_value(function_name, arguments = list(), namespace = NULL)

bp_create_project(name = "Untitled plot")

bp_default_environment()

bp_execute_project(project, registry = NULL,
  environment = bp_default_environment(), timeout_seconds = 12)

bp_generate_code(project, registry = NULL, include_setup = FALSE)

bp_generate_lines(project, registry = NULL)

bp_get_spec(registry, module_id)

bp_instantiate_module(module_id, registry, instance_id = NULL)

bp_load_project(path)

bp_load_registry(path = NULL)

bp_load_template(template_id = NULL, path = NULL)

bp_parse_code(code, registry = NULL)

bp_project_from_template(template, registry = NULL)

bp_save_project(project, path)

bp_validate_module_spec(spec)

bp_validate_project(project, registry = NULL)
}
\arguments{
  \item{state}{Argument state such as \code{unset}, \code{explicit},
  \code{explicit_null}, \code{explicit_na}, or \code{raw_expression}.}
  \item{value}{A typed R semantic value.}
  \item{origin}{Argument provenance such as \code{formal} or
  \code{dots_aesthetic}.}
  \item{source_text}{Optional original R source fragment.}
  \item{position}{Optional original argument position.}
  \item{dynamic}{Whether the argument was not declared by the ModuleSpec.}
  \item{function_name}{R function symbol.}
  \item{arguments}{Named list of typed function-call arguments.}
  \item{namespace}{Optional R package namespace.}
  \item{name}{Project display name.}
  \item{project}{A BioPlotBlocks project list.}
  \item{registry}{Registry returned by \code{bp_load_registry()}.}
  \item{environment}{R environment used for local execution.}
  \item{timeout_seconds}{Elapsed execution time limit.}
  \item{include_setup}{Whether to prepend \code{library(ggplot2)}.}
  \item{module_id}{Declarative module identifier.}
  \item{instance_id}{Optional stable module-instance identifier.}
  \item{path}{Input or output file/directory path.}
  \item{template_id}{Template identifier or template JSON path.}
  \item{code}{R source to parse.}
  \item{template}{Template identifier or parsed template definition.}
  \item{spec}{Parsed ModuleSpec list.}
}
\value{
Depending on the function, a typed argument or call, project, ModuleSpec,
registry, generated source/line records, parsed project, execution result, or
an invisible successful validation value.
}
\description{
Core functions used by the Shiny editor and tests to keep declarative modules,
typed R semantics, generated code, persistence, and real ggplot2 execution in
sync.
}
