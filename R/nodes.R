# ============================================================================
# vastR - Node creation functions
# ============================================================================

# ---- Internal helper to build a node list ----

.make_node <- function(id, label, type, shape, style, fillcolor,
                       fontsize, width, height, penwidth,
                       is_html = FALSE) {
  node <- list(
    id        = id,
    label     = label,
    type      = type,
    shape     = shape,
    style     = style,
    fillcolor = fillcolor,
    fontsize  = fontsize,
    fontname  = "Arial",
    width     = width,
    height    = height,
    penwidth  = penwidth,
    is_html   = is_html
  )
  class(node) <- "vast_node"
  node
}


#' Create a VAST concept node
#'
#' Concepts are the basic building blocks of VAST: frames with abstract
#' labels. By default they are assigned a value range of 0 to 1.
#'
#' @param id Character. Unique node identifier (e.g., `"X"`, `"C1"`).
#' @param label Character. Display label. Defaults to `id`.
#' @param fillcolor Character. Fill color (any valid Graphviz color).
#' @param fontsize Numeric. Font size in points.
#' @param style Character. Graphviz node style string
#'   (e.g., `"filled"`, `"filled,bold"`).
#' @param width Numeric. Minimum node width in inches.
#' @param height Numeric. Minimum node height in inches.
#' @param penwidth Numeric. Border line width.
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_concept("X")
#' vast_concept("SES", label = "Socioeconomic\nStatus", fillcolor = "#D0E8FF")
#'
#' @export
vast_concept <- function(id,
                         label     = id,
                         fillcolor = "#E8F0FE",
                         fontsize  = 12,
                         style     = "filled",
                         width     = 1.2,
                         height    = 0.5,
                         penwidth  = 1.5) {
  .make_node(id, label, type = "concept", shape = "box",
             style = style, fillcolor = fillcolor, fontsize = fontsize,
             width = width, height = height, penwidth = penwidth)
}


#' Create a VAST name node
#'
#' Names are natural-language labels for concepts. They are displayed
#' in quotation marks with a dashed border to distinguish them from
#' concepts.
#'
#' @inheritParams vast_concept
#' @param label Character. The name/word (will be displayed in quotes).
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_name("n_sun", label = "Sun")
#'
#' @export
vast_name <- function(id,
                      label,
                      fillcolor = "#FFF3E0",
                      fontsize  = 12,
                      style     = "filled,dashed",
                      width     = 1.2,
                      height    = 0.5,
                      penwidth  = 1.5) {
  quoted_label <- paste0('\u201C', label, '\u201D')
  .make_node(id, quoted_label, type = "name", shape = "box",
             style = style, fillcolor = fillcolor, fontsize = fontsize,
             width = width, height = height, penwidth = penwidth)
}


#' Create a VAST data node
#'
#' Data nodes represent empirical observations (measured variables).
#' They are displayed as a box with a thick black edge on the left side,
#' mirroring the convention in Structural Equation Modelling for manifest
#' variables.
#'
#' @inheritParams vast_concept
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_data("D1", label = "Survey (N=500)")
#'
#' @export
vast_data <- function(id,
                      label     = id,
                      fillcolor = "#E8F0FE",
                      fontsize  = 12) {
  html_label <- sprintf(
    paste0(
      '<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="4">',
      '<TR>',
      '<TD BGCOLOR="black" WIDTH="6"></TD>',
      '<TD BGCOLOR="%s"> %s </TD>',
      '</TR>',
      '</TABLE>>'
    ),
    fillcolor, label
  )

  .make_node(id, html_label, type = "data", shape = "plaintext",
             style = "", fillcolor = "", fontsize = fontsize,
             width = 0, height = 0, penwidth = 0, is_html = TRUE)
}


#' Create a VAST IS node
#'
#' Represents a claim about whether something IS the case.
#' Displayed as a pentagon.
#'
#' @inheritParams vast_concept
#' @param value Character or Numeric or `NULL`. The IS value
#'   (e.g., `"1"`, `0.5`, `"?"`). Use `NULL` for no value displayed.
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_is("is1", value = 2)
#' vast_is("is2")
#'
#' @export
vast_is <- function(id,
                    value     = NULL,
                    fillcolor = "#E8EAF6",
                    fontsize  = 12,
                    penwidth  = 1.5) {
  lbl <- if (is.null(value) || identical(value, "")) "IS" else paste0("IS\n", value)
  .make_node(id, lbl, type = "is", shape = "pentagon",
             style = "filled", fillcolor = fillcolor, fontsize = fontsize,
             width = 0.7, height = 0.5, penwidth = penwidth)
}


#' Create a VAST OUGHT node
#'
#' Represents a claim about whether something OUGHT to be the case.
#' Displayed as a pentagon.
#'
#' @inheritParams vast_is
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_ought("o1", value = 0)
#'
#' @export
vast_ought <- function(id,
                       value     = NULL,
                       fillcolor = "#FCE4EC",
                       fontsize  = 12,
                       penwidth  = 1.5) {
  lbl <- if (is.null(value) || identical(value, "")) "OUGHT" else paste0("OUGHT\n", value)
  .make_node(id, lbl, type = "ought", shape = "pentagon",
             style = "filled", fillcolor = fillcolor, fontsize = fontsize,
             width = 0.7, height = 0.5, penwidth = penwidth)
}


#' Create a VAST perspective node
#'
#' Represents a perspective-holder's degree of agreement with a claim.
#' Displayed as an oval (ellipse).
#'
#' @inheritParams vast_concept
#' @param holder Character. Name of the perspective holder (e.g., `"Daniel"`).
#' @param value Numeric or Character or `NULL`. Degree of agreement.
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_perspective("p1", holder = "Daniel", value = 0.9)
#'
#' @export
vast_perspective <- function(id,
                             holder,
                             value     = NULL,
                             fillcolor = "#E0F2F1",
                             fontsize  = 12,
                             penwidth  = 1.5) {
  lbl <- if (is.null(value) || identical(value, "")) holder else paste0(holder, "\n", value)
  .make_node(id, lbl, type = "perspective", shape = "ellipse",
             style = "filled", fillcolor = fillcolor, fontsize = fontsize,
             width = 1.0, height = 0.5, penwidth = penwidth)
}


#' Create a VAST diamond node (logical connective or custom function)
#'
#' Diamonds join multiple inputs into a single output. The label can be
#' a standard logical connective (`"AND"`, `"OR"`, `"XOR"`) or any
#' arbitrary function or formula (e.g., `"Y = 2.45X + Z"`,
#' `"weighted mean"`, `"max(A, B)"`).
#'
#' When `footnote` is provided, the formula is displayed as a footnote
#' at the bottom of the diagram, and the diamond label is automatically
#' replaced with a superscript number (e.g., `"\u00B9"`, `"\u00B2"`).
#' The numbering is assigned automatically based on the order diamonds
#' with footnotes appear in the model.
#'
#' @inheritParams vast_concept
#' @param label Character. Content of the diamond.
#' @param footnote Logical or Character or `NULL`. Controls whether the
#'   diamond's label is moved to a footnote area at the bottom of the
#'   diagram. If `TRUE`, the current `label` text is used as the footnote
#'   and the diamond displays an asterisk with a superscript number
#'   (e.g., `"*\u00B9"`). If a character string, that string is used as
#'   the footnote text instead. If `NULL` (default), no footnote is
#'   created and the label is displayed normally in the diamond.
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_diamond("d1", label = "AND")
#' vast_diamond("fn1", label = "Y = 2X + Z")
#' vast_diamond("fn2", label = "Y = 2.45X + Z", footnote = TRUE)
#'
#' @export
vast_diamond <- function(id,
                         label     = "AND",
                         fillcolor = "#FFFDE7",
                         fontsize  = 10,
                         penwidth  = 1.5,
                         footnote  = NULL) {
  # Determine footnote text: TRUE means use the label as footnote text
  fn_text <- NULL
  if (isTRUE(footnote)) {
    fn_text <- label
  } else if (is.character(footnote) && nzchar(footnote)) {
    fn_text <- footnote
  }

  node <- .make_node(id, label, type = "diamond", shape = "diamond",
                     style = "filled", fillcolor = fillcolor,
                     fontsize = fontsize,
                     width = 1.0, height = 0.8, penwidth = penwidth)
  node$footnote <- fn_text
  node
}


#' Create a noise source node
#'
#' In VAST, noise arrows point toward a concept but do not originate
#' from a named concept. This creates an invisible point node to serve
#' as the arrow's origin. Pair it with [vast_noise()] to draw the arrow.
#'
#' @param id Character. Unique node identifier.
#'
#' @return A list of class `vast_node`.
#'
#' @examples
#' vast_noise_source("ns1")
#'
#' @export
vast_noise_source <- function(id) {
  node <- list(
    id        = id,
    label     = "",
    type      = "noise_source",
    shape     = "point",
    style     = "invis",
    fillcolor = "black",
    fontsize  = 1,
    fontname  = "Arial",
    width     = 0.05,
    height    = 0.05,
    penwidth  = 0,
    is_html   = FALSE
  )
  class(node) <- "vast_node"
  node
}


#' Print a VAST node
#' @param x A `vast_node` object.
#' @param ... Ignored.
#' @export
print.vast_node <- function(x, ...) {
  cat(sprintf("VAST node [%s]: id=%s, label=%s\n",
              x$type, x$id, x$label))
  invisible(x)
}
