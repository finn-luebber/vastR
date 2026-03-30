# ============================================================================
# vastR - Edge / relationship creation functions
# ============================================================================

# ---- Internal helper ----

.make_edge <- function(from, to, type, strength, color, style,
                       penwidth, fontsize, index,
                       lhead = NULL, ltail = NULL,
                       bidirectional = FALSE) {
  type_label <- type
  if (!is.null(index)) {
    type_label <- paste0(type, index)
  }

  if (!is.null(strength) && !identical(strength, "")) {
    edge_label <- paste0(type_label, " ", strength)
  } else {
    edge_label <- type_label
  }

  edge <- list(
    from          = from,
    to            = to,
    type          = type,
    strength      = strength,
    label         = edge_label,
    color         = color,
    style         = style,
    penwidth      = penwidth,
    fontsize      = fontsize,
    fontname      = "Arial",
    lhead         = lhead,
    ltail         = ltail,
    bidirectional = bidirectional
  )
  class(edge) <- "vast_edge"
  edge
}


#' Create a VAST relationship (edge)
#'
#' The generic edge constructor. All typed relationship helpers
#' ([vast_naming()], [vast_causation()], etc.) call this internally.
#' Use this directly when you need a custom relationship type or
#' a plain structural link with no type letter.
#'
#' @param from Character. ID of the source node.
#' @param to Character. ID of the target node.
#' @param type Character. Relationship type letter. Standard types are
#'   `"n"` (naming), `"i"` (implication), `"c"` (causation),
#'   `"t"` (transformation), `"p"` (prediction), `"r"` (reasoning),
#'   `"u"` (unknown). Use `""` for a plain structural link with no label.
#'   Custom single letters or abbreviations are also accepted.
#' @param strength Numeric, Character, or `NULL`. Relationship strength.
#'   Can be a number (e.g., `0.5`, `-0.3`), a verbal label (`"weak"`,
#'   `"strong"`), a special marker (`"?"`, `"<> 0"`, `"0"`), or `NULL`
#'   (unspecified; default positive).
#' @param color Character. Edge color (any valid Graphviz color).
#' @param style Character. Graphviz edge style: `"solid"`, `"dashed"`,
#'   `"dotted"`, `"bold"`.
#' @param penwidth Numeric. Edge line width.
#' @param fontsize Numeric. Font size for the edge label.
#' @param index Character or `NULL`. Optional index for the coefficient
#'   (e.g., `"1"` renders as `"c1"` for causation).
#' @param lhead Character or `NULL`. ID of a group (higher-order concept)
#'   that the arrow should visually point **to**. The arrow will terminate
#'   at the border of that group's bounding box instead of at the target
#'   node. The `to` node must be inside that group.
#' @param ltail Character or `NULL`. ID of a group that the arrow should
#'   visually originate **from**. The arrow will start at the border of
#'   that group's bounding box. The `from` node must be inside that group.
#' @param bidirectional Logical. If `TRUE`, the arrow is drawn with
#'   arrowheads in both directions. Use for symmetric relationships
#'   (e.g., identical concepts via `i 1`, mutual predictions). A single
#'   strength value applies to both directions; if strengths differ by
#'   direction, use two separate edges instead.
#'
#' @return A list of class `vast_edge`.
#'
#' @examples
#' vast_relation("X", "Y", type = "c", strength = 0.5)
#' vast_relation("A", "B", type = "", color = "gray60")
#'
#' # Arrow pointing to the border of group "bio" instead of node "genes":
#' vast_causation("ext", "genes", lhead = "bio")
#'
#' # Bidirectional implication (identity):
#' vast_implication("X", "Y", strength = 1, bidirectional = TRUE)
#'
#' @export
vast_relation <- function(from,
                          to,
                          type          = "c",
                          strength      = NULL,
                          color         = "gray30",
                          style         = "solid",
                          penwidth      = 1.5,
                          fontsize      = 10,
                          index         = NULL,
                          lhead         = NULL,
                          ltail         = NULL,
                          bidirectional = FALSE) {
  .make_edge(from, to, type, strength, color, style,
             penwidth, fontsize, index, lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a naming relationship (n)
#'
#' Arrow from a concept to its name. Represents the link between a
#' cognitive concept and the word used to denote it.
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_naming <- function(from, to, strength = NULL, index = NULL,
                       lhead = NULL, ltail = NULL,
                       bidirectional = FALSE,
                       color = "#795548", style = "solid",
                       penwidth = 1.5, ...) {
  .make_edge(from, to, type = "n", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a conceptual implication relationship (i)
#'
#' How much thinking of something as X also implies thinking of it as Y.
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_implication <- function(from, to, strength = NULL, index = NULL,
                            lhead = NULL, ltail = NULL,
                            bidirectional = FALSE,
                            color = "#1565C0", style = "solid",
                            penwidth = 1.5, ...) {
  .make_edge(from, to, type = "i", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a causation relationship (c)
#'
#' How reliably X will trigger Y. Implies temporal order: causes
#' always precede effects.
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_causation <- function(from, to, strength = NULL, index = NULL,
                          lhead = NULL, ltail = NULL,
                          bidirectional = FALSE,
                          color = "#C62828", style = "solid",
                          penwidth = 1.5, ...) {
  .make_edge(from, to, type = "c", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a transformation relationship (t)
#'
#' The applicability of Y can be deduced from X by computation
#' (e.g., unit conversion, scoring procedures).
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_transformation <- function(from, to, strength = NULL, index = NULL,
                               lhead = NULL, ltail = NULL,
                               bidirectional = FALSE,
                               color = "#6A1B9A", style = "solid",
                               penwidth = 1.5, ...) {
  .make_edge(from, to, type = "t", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a prediction relationship (p)
#'
#' Knowing X tells us something about Y, without necessarily
#' knowing the mechanism underlying the association.
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_prediction <- function(from, to, strength = NULL, index = NULL,
                           lhead = NULL, ltail = NULL,
                           bidirectional = FALSE,
                           color = "#2E7D32", style = "solid",
                           penwidth = 1.5, ...) {
  .make_edge(from, to, type = "p", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a reasoning relationship (r)
#'
#' How much X is a reason to believe Y. Not limited to formally
#' logical conclusions.
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_reasoning <- function(from, to, strength = NULL, index = NULL,
                          lhead = NULL, ltail = NULL,
                          bidirectional = FALSE,
                          color = "#E65100", style = "solid",
                          penwidth = 1.5, ...) {
  .make_edge(from, to, type = "r", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create an unknown relationship (u)
#'
#' A relationship is assumed to exist, but its exact nature is
#' not (yet) known. Displayed with a dashed line.
#'
#' @inheritParams vast_relation
#' @param ... Additional arguments passed to [vast_relation()].
#'
#' @return A list of class `vast_edge`.
#' @export
vast_unknown <- function(from, to, strength = NULL, index = NULL,
                        lhead = NULL, ltail = NULL,
                        bidirectional = FALSE,
                        color = "gray50", style = "dashed",
                        penwidth = 1.5, ...) {
  .make_edge(from, to, type = "u", strength = strength,
             color = color, style = style,
             penwidth = penwidth, fontsize = 10, index = index,
             lhead = lhead, ltail = ltail,
             bidirectional = bidirectional)
}


#' Create a noise arrow
#'
#' A noise arrow points toward a concept but originates from an
#' invisible point node (created via [vast_noise_source()]).
#' Represents unspecified additional influences on a concept.
#'
#' @param noise_id Character. ID of the noise source node.
#' @param to Character. ID of the target concept.
#' @param strength Numeric, Character, or `NULL`. Noise coefficient.
#'   Special values: `"<> 0"` (bidirectional), `"0"` (no noise),
#'   `"?"` (unknown).
#' @param color Character. Edge color.
#'
#' @return A list of class `vast_edge`.
#'
#' @examples
#' vast_noise("ns1", "Y", strength = "<> 0")
#'
#' @export
vast_noise <- function(noise_id, to, strength = NULL, color = "gray50") {
  if (!is.null(strength) && !identical(strength, "")) {
    edge_label <- as.character(strength)
  } else {
    edge_label <- ""
  }

  edge <- list(
    from     = noise_id,
    to       = to,
    type     = "noise",
    strength = strength,
    label    = edge_label,
    color    = color,
    style    = "solid",
    penwidth = 1.0,
    fontsize = 10,
    fontname = "Arial"
  )
  class(edge) <- "vast_edge"
  edge
}


#' Print a VAST edge
#' @param x A `vast_edge` object.
#' @param ... Ignored.
#' @export
print.vast_edge <- function(x, ...) {
  cat(sprintf("VAST edge [%s]: %s -> %s", x$type, x$from, x$to))
  if (!is.null(x$strength)) cat(sprintf(" (strength: %s)", x$strength))
  cat("\n")
  invisible(x)
}
