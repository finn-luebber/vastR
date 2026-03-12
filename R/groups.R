# ============================================================================
# vastR - Higher-order concepts (groups / clusters)
# ============================================================================

#' Create a higher-order concept (group)
#'
#' A higher-order concept is a specific combination of VAST elements
#' that may, in its entirety, apply to certain objects. Visually, it
#' is rendered as a bounding box (Graphviz subgraph cluster) around
#' the contained nodes.
#'
#' @param id Character. Unique group identifier.
#' @param label Character. Display label for the group. Defaults to `id`.
#' @param node_ids Character vector. IDs of nodes that belong to this group.
#' @param fillcolor Character. Background fill color.
#' @param bordercolor Character. Border color.
#' @param penwidth Numeric. Border line width.
#' @param fontsize Numeric. Font size for the label.
#' @param style Character. Graphviz style string for the cluster
#'   (e.g., `"filled,rounded"`, `"filled,dashed"`, `"filled,bold"`).
#' @param labeljust Character. Label justification: `"l"` (left),
#'   `"c"` (center), `"r"` (right).
#' @param labelloc Character. Label location: `"t"` (top), `"b"` (bottom).
#'
#' @return A list of class `vast_group`.
#'
#' @examples
#' vast_group("HOC1", label = "Higher-Order Concept",
#'            node_ids = c("X", "Y", "Z"))
#'
#' @export
vast_group <- function(id,
                       label       = id,
                       node_ids    = character(0),
                       fillcolor   = "#F5F5F5",
                       bordercolor = "gray40",
                       penwidth    = 2.0,
                       fontsize    = 12,
                       style       = "filled",
                       labeljust   = "l",
                       labelloc    = "t") {
  group <- list(
    id              = id,
    label           = label,
    node_ids        = node_ids,
    child_group_ids = character(0),
    fillcolor       = fillcolor,
    bordercolor     = bordercolor,
    penwidth        = penwidth,
    fontsize        = fontsize,
    fontname        = "Arial",
    style           = style,
    labeljust       = labeljust,
    labelloc        = labelloc
  )
  class(group) <- "vast_group"
  group
}


#' Create a nested higher-order concept
#'
#' Like [vast_group()], but additionally accepts `child_group_ids`:
#' other groups that should be rendered as nested clusters inside
#' this one.
#'
#' @inheritParams vast_group
#' @param child_group_ids Character vector. IDs of child `vast_group`
#'   objects to nest inside this group.
#'
#' @return A list of class `vast_group`.
#'
#' @examples
#' vast_nested_group("outer", label = "All Factors",
#'                   node_ids = c("diamond1"),
#'                   child_group_ids = c("bio", "env"))
#'
#' @export
vast_nested_group <- function(id,
                              label           = id,
                              node_ids        = character(0),
                              child_group_ids = character(0),
                              fillcolor       = "#EEEEEE",
                              bordercolor     = "gray30",
                              penwidth        = 2.5,
                              fontsize        = 14,
                              style           = "filled,bold",
                              labeljust       = "l",
                              labelloc        = "t") {
  group <- vast_group(
    id          = id,
    label       = label,
    node_ids    = node_ids,
    fillcolor   = fillcolor,
    bordercolor = bordercolor,
    penwidth    = penwidth,
    fontsize    = fontsize,
    style       = style,
    labeljust   = labeljust,
    labelloc    = labelloc
  )
  group$child_group_ids <- child_group_ids
  group
}


#' Print a VAST group
#' @param x A `vast_group` object.
#' @param ... Ignored.
#' @export
print.vast_group <- function(x, ...) {
  cat(sprintf("VAST group: id=%s, label=%s, nodes=%s",
              x$id, x$label, paste(x$node_ids, collapse = ", ")))
  if (length(x$child_group_ids) > 0) {
    cat(sprintf(", children=%s", paste(x$child_group_ids, collapse = ", ")))
  }
  cat("\n")
  invisible(x)
}
