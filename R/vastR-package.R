#' @title vastR: Visual Argument Structure Tool for R
#'
#' @description
#' An R implementation of the Visual Argument Structure Tool (VAST)
#' by Leising, Grenke, & Cramer (2023, Meta-Psychology).
#'
#' VAST provides a visual language for mapping argument structures. This
#' package lets you programmatically construct VAST diagrams and render
#' them via Graphviz / DiagrammeR.
#'
#' @section Workflow:
#' 1. Create a model with [vast_model()]
#' 2. Add nodes with [add_nodes()] using constructors like [vast_concept()],
#'    [vast_name()], [vast_diamond()], etc.
#' 3. Add edges with [add_edges()] using [vast_causation()],
#'    [vast_implication()], etc.
#' 4. Optionally add higher-order concept groups with [add_groups()]
#' 5. Render with [vast_render()] or export with [vast_export_svg()]
#'
#' @references
#' Leising, D., Grenke, O., & Cramer, M. (2023). Visual Argument Structure
#' Tool (VAST) Version 1.0. *Meta-Psychology*, 7.
#' \doi{10.15626/MP.2021.2911}
#'
#' @docType package
#' @name vastR-package
#' @keywords internal
"_PACKAGE"
