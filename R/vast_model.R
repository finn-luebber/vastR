# ============================================================================
# vastR - Model container, DOT generation, rendering, and export
# ============================================================================

#' Create an empty VAST model
#'
#' A VAST model is the top-level container holding all nodes, edges,
#' and groups. Build it up with [add_nodes()], [add_edges()], and
#' [add_groups()], then render with [vast_render()] or convert to
#' DOT with [vast_to_dot()].
#'
#' All builder functions support R's native pipe (`|>`).
#'
#' @param title Character. Optional title displayed above the diagram.
#' @param rankdir Character. Layout direction: `"TB"` (top-to-bottom),
#'   `"LR"` (left-to-right), `"BT"`, or `"RL"`.
#' @param naming_mode Character. How concept-name relationships are displayed:
#'   \describe{
#'     \item{`"integrated"`}{(Default) Concepts and names coexist in the
#'       diagram, connected by naming (n) arrows.}
#'     \item{`"separated"`}{The main diagram shows concepts with abstract
#'       labels only. Naming relationships are displayed in a separate
#'       legend area beside the main diagram.}
#'     \item{`"fimm"`}{Finger-is-Moon-Mode. Concept nodes display the
#'       label of their (first) connected name node. Name nodes and
#'       naming edges are hidden from the diagram.}
#'   }
#' @param fontsize Numeric. Default global font size (used for graph title).
#' @param fontsize_nodes Numeric. Font size for all non-name nodes
#'   (concepts, IS/OUGHT, perspectives, diamonds, data).
#' @param fontsize_names Numeric. Font size for name nodes. Useful for
#'   shrinking long natural-language labels independently.
#' @param fontsize_edges Numeric. Font size for all edge labels.
#' @param analyst Character. Name(s) of the analyst(s) who created
#'   the display (e.g., `"Daniel"`, `"Daniel, Marcos"`). Displayed
#'   in the top-left corner of the diagram.
#' @param bgcolor Character. Background color of the graph.
#' @param nodesep Numeric. Minimum horizontal space between nodes (inches).
#' @param ranksep Numeric. Minimum vertical space between ranks (inches).
#'
#' @return A list of class `vast_model`.
#'
#' @examples
#' m <- vast_model(title = "My Argument", naming_mode = "fimm")
#' m <- vast_model(fontsize_names = 9, fontsize_edges = 8)
#' m <- vast_model(analyst = "Daniel, Marcos")
#'
#' @export
vast_model <- function(title          = "",
                       rankdir        = "TB",
                       naming_mode    = c("integrated", "separated", "fimm"),
                       fontsize       = 12,
                       fontsize_nodes = 12,
                       fontsize_names = 10,
                       fontsize_edges = 10,
                       analyst        = "",
                       bgcolor        = "white",
                       nodesep        = 0.6,
                       ranksep        = 0.8) {
  naming_mode <- match.arg(naming_mode)

  model <- list(
    title          = title,
    rankdir        = rankdir,
    naming_mode    = naming_mode,
    fontsize       = fontsize,
    fontsize_nodes = fontsize_nodes,
    fontsize_names = fontsize_names,
    fontsize_edges = fontsize_edges,
    analyst        = analyst,
    bgcolor        = bgcolor,
    nodesep        = nodesep,
    ranksep        = ranksep,
    nodes          = list(),
    edges          = list(),
    groups         = list()
  )
  class(model) <- "vast_model"
  model
}


#' Add nodes to a VAST model
#'
#' @param model A `vast_model` object.
#' @param ... One or more `vast_node` objects, or a single list of them.
#'
#' @return The updated `vast_model` (invisibly pipe-friendly).
#'
#' @examples
#' m <- vast_model() |>
#'   add_nodes(
#'     vast_concept("X"),
#'     vast_concept("Y")
#'   )
#'
#' @export
add_nodes <- function(model, ...) {
  stopifnot(inherits(model, "vast_model"))
  new_nodes <- list(...)
  if (length(new_nodes) == 1 && is.list(new_nodes[[1]]) &&
      !inherits(new_nodes[[1]], "vast_node")) {
    new_nodes <- new_nodes[[1]]
  }
  for (node in new_nodes) {
    stopifnot(inherits(node, "vast_node"))
    model$nodes[[node$id]] <- node
  }
  model
}


#' Add edges to a VAST model
#'
#' @param model A `vast_model` object.
#' @param ... One or more `vast_edge` objects, or a single list of them.
#'
#' @return The updated `vast_model`.
#'
#' @examples
#' m <- vast_model() |>
#'   add_nodes(vast_concept("X"), vast_concept("Y")) |>
#'   add_edges(vast_causation("X", "Y", strength = 0.5))
#'
#' @export
add_edges <- function(model, ...) {
  stopifnot(inherits(model, "vast_model"))
  new_edges <- list(...)
  if (length(new_edges) == 1 && is.list(new_edges[[1]]) &&
      !inherits(new_edges[[1]], "vast_edge")) {
    new_edges <- new_edges[[1]]
  }
  for (edge in new_edges) {
    stopifnot(inherits(edge, "vast_edge"))
    model$edges[[length(model$edges) + 1]] <- edge
  }
  model
}


#' Add groups (higher-order concepts) to a VAST model
#'
#' @param model A `vast_model` object.
#' @param ... One or more `vast_group` objects, or a single list of them.
#'
#' @return The updated `vast_model`.
#'
#' @export
add_groups <- function(model, ...) {
  stopifnot(inherits(model, "vast_model"))
  new_groups <- list(...)
  if (length(new_groups) == 1 && is.list(new_groups[[1]]) &&
      !inherits(new_groups[[1]], "vast_group")) {
    new_groups <- new_groups[[1]]
  }
  for (grp in new_groups) {
    stopifnot(inherits(grp, "vast_group"))
    model$groups[[grp$id]] <- grp
  }
  model
}


# ============================================================================
# Internal: naming mode resolution
# ============================================================================

#' Resolve naming relationships for mode switching
#'
#' @param model A vast_model object.
#' @return A list with components: concept_to_name, name_node_ids,
#'   naming_edge_idx.
#' @keywords internal
.resolve_naming <- function(model) {
  concept_to_name <- character()
  name_node_ids   <- character()
  naming_edge_idx <- integer()

  for (i in seq_along(model$edges)) {
    edge <- model$edges[[i]]
    if (identical(edge$type, "n")) {
      naming_edge_idx <- c(naming_edge_idx, i)
      from_id <- edge$from
      to_id   <- edge$to

      if (to_id %in% names(model$nodes) &&
          identical(model$nodes[[to_id]]$type, "name")) {
        name_node_ids <- c(name_node_ids, to_id)
        if (!(from_id %in% names(concept_to_name))) {
          concept_to_name[from_id] <- model$nodes[[to_id]]$label
        }
      }
    }
  }

  list(
    concept_to_name = concept_to_name,
    name_node_ids   = unique(name_node_ids),
    naming_edge_idx = naming_edge_idx
  )
}


# ============================================================================
# Internal DOT generation
# ============================================================================

#' Generate DOT for a single node
#' @param node A vast_node object.
#' @param label_override Optional label to display instead of node$label.
#' @param fontsize_override Optional fontsize to use instead of node$fontsize.
#' @keywords internal
.node_to_dot <- function(node, label_override = NULL, fontsize_override = NULL) {
  is_html <- isTRUE(node$is_html)

  display_label <- if (!is.null(label_override)) label_override else node$label

  if (is_html && is.null(label_override)) {
    label_str <- paste0("label=", display_label)
  } else {
    label_str <- paste0('label="', gsub('"', '\\\\"', display_label), '"')
  }

  fs <- if (!is.null(fontsize_override)) fontsize_override else node$fontsize

  attrs <- c(
    label_str,
    paste0('shape="', node$shape, '"')
  )

  if (nzchar(node$style))     attrs <- c(attrs, paste0('style="', node$style, '"'))
  if (nzchar(node$fillcolor)) attrs <- c(attrs, paste0('fillcolor="', node$fillcolor, '"'))
  if (fs > 0)                 attrs <- c(attrs, paste0('fontsize=', fs))
  if (nzchar(node$fontname))  attrs <- c(attrs, paste0('fontname="', node$fontname, '"'))
  if (node$width > 0)         attrs <- c(attrs, paste0('width=', node$width))
  if (node$height > 0)        attrs <- c(attrs, paste0('height=', node$height))
  if (node$penwidth > 0)      attrs <- c(attrs, paste0('penwidth=', node$penwidth))

  paste0('  "', node$id, '" [', paste(attrs, collapse = ", "), '];')
}


#' Generate DOT for a single edge
#' @param edge A vast_edge object.
#' @param fontsize_override Optional fontsize for the edge label.
#' @keywords internal
.edge_to_dot <- function(edge, fontsize_override = NULL) {
  fs <- if (!is.null(fontsize_override)) fontsize_override else edge$fontsize

  attrs <- c(
    paste0('label="', gsub('"', '\\\\"', edge$label), '"'),
    paste0('color="', edge$color, '"'),
    paste0('style="', edge$style, '"'),
    paste0('penwidth=', edge$penwidth),
    paste0('fontsize=', fs),
    paste0('fontname="', edge$fontname, '"'),
    'fontcolor="gray20"'
  )

  # Bidirectional arrow support
  if (isTRUE(edge$bidirectional)) {
    attrs <- c(attrs, 'dir="both"')
  }

  # Compound edge support
  if (!is.null(edge$lhead) && nzchar(edge$lhead)) {
    attrs <- c(attrs, paste0('lhead="cluster_', edge$lhead, '"'))
  }
  if (!is.null(edge$ltail) && nzchar(edge$ltail)) {
    attrs <- c(attrs, paste0('ltail="cluster_', edge$ltail, '"'))
  }

  paste0('  "', edge$from, '" -> "', edge$to, '" [',
         paste(attrs, collapse = ", "), '];')
}


#' Generate DOT for a group (subgraph cluster)
#' @param group A vast_group object.
#' @param all_groups Named list of all groups in the model.
#' @param all_nodes Named list of all nodes in the model.
#' @param indent Current indentation string.
#' @param label_overrides Named character vector of label overrides.
#' @param skip_node_ids Character vector of node IDs to suppress.
#' @param fontsize_nodes Numeric. Font size override for non-name nodes.
#' @param fontsize_names Numeric. Font size override for name nodes.
#' @keywords internal
.group_to_dot <- function(group, all_groups, all_nodes, indent = "  ",
                          label_overrides = NULL, skip_node_ids = NULL,
                          fontsize_nodes = NULL, fontsize_names = NULL) {
  lines <- character()
  lines <- c(lines, paste0(indent, 'subgraph "cluster_', group$id, '" {'))
  lines <- c(lines, paste0(indent, '  label="',
                            gsub('"', '\\\\"', group$label), '";'))
  lines <- c(lines, paste0(indent, '  style="', group$style, '";'))
  lines <- c(lines, paste0(indent, '  fillcolor="', group$fillcolor, '";'))
  lines <- c(lines, paste0(indent, '  color="', group$bordercolor, '";'))
  lines <- c(lines, paste0(indent, '  penwidth=', group$penwidth, ';'))
  lines <- c(lines, paste0(indent, '  fontsize=', group$fontsize, ';'))
  lines <- c(lines, paste0(indent, '  fontname="', group$fontname, '";'))
  lines <- c(lines, paste0(indent, '  labeljust="', group$labeljust, '";'))
  lines <- c(lines, paste0(indent, '  labelloc="', group$labelloc, '";'))

  # Nested child groups
  child_ids <- group$child_group_ids
  if (length(child_ids) > 0) {
    for (cid in child_ids) {
      if (cid %in% names(all_groups)) {
        lines <- c(lines, .group_to_dot(all_groups[[cid]], all_groups,
                                        all_nodes, paste0(indent, "  "),
                                        label_overrides = label_overrides,
                                        skip_node_ids = skip_node_ids,
                                        fontsize_nodes = fontsize_nodes,
                                        fontsize_names = fontsize_names))
      }
    }
  }

  # Nodes in this group (skip suppressed nodes)
  for (nid in group$node_ids) {
    if (!is.null(skip_node_ids) && nid %in% skip_node_ids) next
    if (nid %in% names(all_nodes)) {
      node <- all_nodes[[nid]]
      override <- if (!is.null(label_overrides) && nid %in% names(label_overrides)) {
        label_overrides[[nid]]
      } else {
        NULL
      }
      fs <- if (identical(node$type, "name")) fontsize_names else fontsize_nodes
      lines <- c(lines, paste0(indent, "  ",
                                .node_to_dot(node,
                                             label_override = override,
                                             fontsize_override = fs)))
    }
  }

  lines <- c(lines, paste0(indent, '}'))
  lines
}


#' Build HTML table cell attributes that mirror a Graphviz node style
#'
#' Translates the node's `style` string (e.g., `"filled"`, `"filled,dashed"`,
#' `"rounded"`) into the corresponding HTML `<TD>` attributes. BGCOLOR is
#' only set when the style includes `"filled"`; STYLE is built from the
#' applicable keywords (ROUNDED, DASHED, DOTTED).
#'
#' @param node_style Character. The node's Graphviz style string.
#' @param fillcolor Character. The node's fill color.
#' @return A character string of TD attributes (e.g., ` BGCOLOR="#E8F0FE" BORDER="1" STYLE="ROUNDED"`).
#' @keywords internal
.legend_cell_attrs <- function(node_style, fillcolor) {
  # BGCOLOR only when the node is actually filled
  has_filled <- grepl("filled", node_style, fixed = TRUE)
  bg <- if (has_filled) paste0(' BGCOLOR="', fillcolor, '"') else ""

  # Collect applicable HTML table cell style keywords
  styles <- character()
  if (grepl("rounded", node_style, fixed = TRUE)) styles <- c(styles, "ROUNDED")
  if (grepl("dashed",  node_style, fixed = TRUE)) styles <- c(styles, "DASHED")
  if (grepl("dotted",  node_style, fixed = TRUE)) styles <- c(styles, "DOTTED")

  style_attr <- if (length(styles) > 0) {
    paste0(' STYLE="', paste(styles, collapse = ","), '"')
  } else {
    ""
  }

  paste0(bg, ' BORDER="1"', style_attr)
}


#' Generate a naming legend as a compact HTML table node for separated mode
#' @keywords internal
.naming_legend_to_dot <- function(model, naming_info) {
  lines <- character()

  # Collect unique concept->name pairs
  pairs <- list()
  seen <- character()

  for (i in naming_info$naming_edge_idx) {
    edge <- model$edges[[i]]
    pair_key <- paste0(edge$from, "->", edge$to)
    if (pair_key %in% seen) next
    seen <- c(seen, pair_key)

    concept_node <- model$nodes[[edge$from]]
    name_node    <- model$nodes[[edge$to]]
    if (is.null(concept_node) || is.null(name_node)) next

    pairs[[length(pairs) + 1]] <- list(
      concept_label = concept_node$label,
      concept_fill  = concept_node$fillcolor,
      concept_style = concept_node$style,
      name_label    = name_node$label,
      name_fill     = name_node$fillcolor,
      name_style    = name_node$style
    )
  }

  if (length(pairs) == 0) return(character())

  # Font sizes for the legend inherit from the model
  fs_c <- model$fontsize_nodes
  fs_n <- model$fontsize_names

  # Build HTML table
  html <- '<<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0" CELLPADDING="6" COLOR="gray60" BGCOLOR="#FAFAFA">'
  # Header row
  html <- paste0(html,
    '<TR><TD COLSPAN="3" ALIGN="left"><B><FONT FACE="Arial" POINT-SIZE="11">Naming</FONT></B></TD></TR>')

  for (p in pairs) {
    concept_lbl <- gsub('"', '&quot;', gsub('\n', '<BR/>', p$concept_label))
    name_lbl    <- gsub('"', '&quot;', gsub('\n', '<BR/>', p$name_label))

    # Build HTML table cell attributes that mirror the actual node styles.
    # Graphviz HTML table cells support STYLE="ROUNDED", STYLE="DASHED",
    # STYLE="DOTTED", and combinations thereof. BGCOLOR is only applied
    # when the node style includes "filled"; otherwise the cell background
    # is left white to match the unfilled node in the main diagram.
    concept_td <- .legend_cell_attrs(p$concept_style, p$concept_fill)
    name_td    <- .legend_cell_attrs(p$name_style, p$name_fill)

    html <- paste0(html,
      '<TR>',
      '<TD', concept_td, '>',
      '<FONT FACE="Arial" POINT-SIZE="', fs_c, '">', concept_lbl, '</FONT></TD>',
      '<TD><FONT FACE="Arial" POINT-SIZE="9" COLOR="#795548"> n\u2192 </FONT></TD>',
      '<TD', name_td, '>',
      '<FONT FACE="Arial" POINT-SIZE="', fs_n, '">', name_lbl, '</FONT></TD>',
      '</TR>')
  }

  html <- paste0(html, '</TABLE>>')

  lines <- c(lines, paste0('  _naming_legend [label=', html,
                            ', shape="plaintext", margin="0"];'))
  lines
}


# ============================================================================
# Public DOT / render / export API
# ============================================================================

#' Convert a VAST model to Graphviz DOT code
#'
#' Generates a complete DOT string that can be rendered by
#' [DiagrammeR::grViz()] or any Graphviz-compatible tool.
#'
#' The output respects the model's `naming_mode`:
#' \describe{
#'   \item{`"integrated"`}{All nodes and edges rendered as defined.}
#'   \item{`"separated"`}{Name nodes and naming edges are moved to a
#'     separate legend cluster. Concepts keep their abstract labels.}
#'   \item{`"fimm"`}{Concept nodes display their first connected name's
#'     label. Name nodes and naming edges are hidden.}
#' }
#'
#' @param model A `vast_model` object.
#'
#' @return A single character string containing valid DOT code.
#'
#' @examples
#' m <- vast_model(title = "Example") |>
#'   add_nodes(vast_concept("X"), vast_concept("Y")) |>
#'   add_edges(vast_causation("X", "Y"))
#' cat(vast_to_dot(m))
#'
#' @export
vast_to_dot <- function(model) {
  stopifnot(inherits(model, "vast_model"))

  naming_mode <- model$naming_mode
  naming_info <- .resolve_naming(model)

  # Build label overrides and skip lists based on naming mode
  label_overrides <- NULL
  skip_node_ids   <- NULL
  skip_edge_idx   <- NULL

  if (naming_mode == "fimm") {
    label_overrides <- naming_info$concept_to_name
    skip_node_ids   <- naming_info$name_node_ids
    skip_edge_idx   <- naming_info$naming_edge_idx

  } else if (naming_mode == "separated") {
    skip_node_ids <- naming_info$name_node_ids
    skip_edge_idx <- naming_info$naming_edge_idx
  }

  lines <- character()
  lines <- c(lines, "digraph VAST {")

  # Graph attributes
  lines <- c(lines, paste0('  graph [rankdir="', model$rankdir, '",'))
  lines <- c(lines, paste0('         bgcolor="', model$bgcolor, '",'))
  lines <- c(lines, paste0('         fontsize=', model$fontsize, ','))
  lines <- c(lines, paste0('         nodesep=', model$nodesep, ','))
  lines <- c(lines, paste0('         ranksep=', model$ranksep, ','))
  lines <- c(lines, '         compound=true,')
  lines <- c(lines, '         fontname="Arial"];')
  lines <- c(lines, "")

  # Title and analyst as combined HTML table label at top
  has_title   <- nzchar(model$title)
  has_analyst <- nzchar(model$analyst)
  if (has_title || has_analyst) {
    lines <- c(lines, '  labelloc="t";')
    if (has_title && !has_analyst) {
      lines <- c(lines, paste0('  label="',
                                gsub('"', '\\\\"', model$title), '";'))
    } else if (!has_title && has_analyst) {
      lines <- c(lines, paste0('  label="Analyst: ',
                                gsub('"', '\\\\"', model$analyst), '";'))
    } else {
      # Both: use HTML table label
      title_esc   <- gsub('"', '&quot;', gsub('&', '&amp;', model$title))
      analyst_esc <- gsub('"', '&quot;', gsub('&', '&amp;', model$analyst))
      html_label <- paste0(
        '  label=<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="2">',
        '<TR><TD><FONT FACE="Arial" POINT-SIZE="', model$fontsize, '">',
        title_esc, '</FONT></TD></TR>',
        '<TR><TD><FONT FACE="Arial" POINT-SIZE="10" COLOR="gray30">',
        'Analyst: ', analyst_esc, '</FONT></TD></TR>',
        '</TABLE>>;'
      )
      lines <- c(lines, html_label)
    }
    lines <- c(lines, "")
  }

  # FIMM indicator
  if (naming_mode == "fimm") {
    lines <- c(lines, '  fimm_label [label="FIMM", shape="plaintext",')
    lines <- c(lines, '    fontsize=10, fontname="Arial", fontcolor="gray50"];')
    lines <- c(lines, "")
  }

  # Collect footnotes from diamond nodes and assign superscript numbers
  # Unicode superscripts: 1-9
  .superscripts <- c("\u00B9", "\u00B2", "\u00B3", "\u2074", "\u2075",
                     "\u2076", "\u2077", "\u2078", "\u2079")
  footnote_entries <- list()  # list of list(superscript, text)
  footnote_label_overrides <- list()  # node_id -> display label

  fn_counter <- 0
  for (nid in names(model$nodes)) {
    node <- model$nodes[[nid]]
    if (identical(node$type, "diamond") && !is.null(node$footnote)) {
      fn_counter <- fn_counter + 1
      # Build superscript: for numbers > 9, concatenate digit superscripts
      if (fn_counter <= 9) {
        sup <- .superscripts[fn_counter]
      } else {
        digits <- strsplit(as.character(fn_counter), "")[[1]]
        sup <- paste0(sapply(as.integer(digits), function(d) {
          if (d == 0) "\u2070" else .superscripts[d]
        }), collapse = "")
      }
      # Diamond shows: *¹  (asterisk with superscript number)
      footnote_label_overrides[[nid]] <- paste0("*", sup)
      # Footnote at bottom shows: ¹ formula text
      footnote_entries[[length(footnote_entries) + 1]] <- list(
        superscript = sup,
        text        = node$footnote
      )
    }
  }

  # Merge footnote label overrides into the main label_overrides
  if (length(footnote_label_overrides) > 0) {
    if (is.null(label_overrides)) label_overrides <- character()
    for (nid in names(footnote_label_overrides)) {
      label_overrides[nid] <- footnote_label_overrides[[nid]]
    }
  }

  # Determine which nodes live inside groups
  grouped_node_ids <- character()
  all_child_group_ids <- character()
  for (grp in model$groups) {
    grouped_node_ids <- c(grouped_node_ids, grp$node_ids)
    if (length(grp$child_group_ids) > 0) {
      all_child_group_ids <- c(all_child_group_ids, grp$child_group_ids)
    }
  }
  grouped_node_ids <- unique(grouped_node_ids)

  # Ungrouped nodes (skipping suppressed name nodes)
  ungrouped_ids <- setdiff(names(model$nodes), grouped_node_ids)
  if (!is.null(skip_node_ids)) {
    ungrouped_ids <- setdiff(ungrouped_ids, skip_node_ids)
  }
  if (length(ungrouped_ids) > 0) {
    lines <- c(lines, "  // --- Nodes ---")
    for (nid in ungrouped_ids) {
      node <- model$nodes[[nid]]
      override <- if (!is.null(label_overrides) && nid %in% names(label_overrides)) {
        label_overrides[[nid]]
      } else {
        NULL
      }
      fs <- if (identical(node$type, "name")) model$fontsize_names else model$fontsize_nodes
      lines <- c(lines, .node_to_dot(node,
                                     label_override = override,
                                     fontsize_override = fs))
    }
    lines <- c(lines, "")
  }

  # Top-level groups (not nested inside another)
  top_level_ids <- setdiff(names(model$groups), all_child_group_ids)
  if (length(top_level_ids) > 0) {
    lines <- c(lines, "  // --- Higher-Order Concepts ---")
    for (gid in top_level_ids) {
      lines <- c(lines, .group_to_dot(model$groups[[gid]], model$groups,
                                      model$nodes,
                                      label_overrides = label_overrides,
                                      skip_node_ids = skip_node_ids,
                                      fontsize_nodes = model$fontsize_nodes,
                                      fontsize_names = model$fontsize_names))
    }
    lines <- c(lines, "")
  }

  # Edges — first validate that all referenced node IDs exist
  all_node_ids <- names(model$nodes)
  if (length(model$edges) > 0) {
    missing <- character()
    for (i in seq_along(model$edges)) {
      if (!is.null(skip_edge_idx) && i %in% skip_edge_idx) next
      edge <- model$edges[[i]]
      if (!(edge$from %in% all_node_ids)) {
        missing <- c(missing, edge$from)
      }
      if (!(edge$to %in% all_node_ids)) {
        missing <- c(missing, edge$to)
      }
    }
    missing <- unique(missing)
    if (length(missing) > 0) {
      existing <- paste0('"', all_node_ids, '"', collapse = ", ")
      stop("Edge(s) reference node ID(s) that do not exist: ",
           paste0('"', missing, '"', collapse = ", "), ".\n",
           "  Existing node IDs are: ", existing, ".\n",
           "  Check for typos in your edge definitions.",
           call. = FALSE)
    }
  }

  # Edges (skipping suppressed naming edges)
  if (length(model$edges) > 0) {
    lines <- c(lines, "  // --- Relationships ---")
    for (i in seq_along(model$edges)) {
      if (!is.null(skip_edge_idx) && i %in% skip_edge_idx) next
      lines <- c(lines, .edge_to_dot(model$edges[[i]],
                                     fontsize_override = model$fontsize_edges))
    }
    lines <- c(lines, "")
  }

  # Separated mode: add the naming legend as a compact table node
  if (naming_mode == "separated" && length(naming_info$naming_edge_idx) > 0) {
    lines <- c(lines, "  // --- Naming Legend ---")
    lines <- c(lines, .naming_legend_to_dot(model, naming_info))
    lines <- c(lines, "")
  }

  # Footnotes from diamond nodes (forced to last rank)
  if (length(footnote_entries) > 0) {
    lines <- c(lines, "  // --- Footnotes ---")
    fn_parts <- vapply(footnote_entries, function(fn) {
      paste0(fn$superscript, " ", fn$text)
    }, character(1))
    fn_text <- paste(fn_parts, collapse = "\\n")
    lines <- c(lines, paste0('  _footnotes [label="',
                              gsub('"', '\\\\"', fn_text),
                              '", shape="plaintext",'))
    lines <- c(lines, '    fontsize=10, fontname="Arial", fontcolor="gray30"];')

    # Find an anchor node to constrain footnotes to the bottom.
    # We pick a "sink" node — one with no outgoing edges in the rendered
    # graph — so the footnote is pulled below the lowest rank.
    # The invisible edge keeps the footnote node connected to the graph,
    # which avoids an assertion crash in viz.js's network-simplex ranker
    # that occurs when rank=sink is used with a disconnected node.
    rendered_ids <- names(model$nodes)
    if (!is.null(skip_node_ids)) {
      rendered_ids <- setdiff(rendered_ids, skip_node_ids)
    }
    sources <- character()
    for (i in seq_along(model$edges)) {
      if (!is.null(skip_edge_idx) && i %in% skip_edge_idx) next
      sources <- c(sources, model$edges[[i]]$from)
    }
    sink_ids <- setdiff(rendered_ids, unique(sources))
    # Fall back to the last rendered node if every node has outgoing edges
    anchor_id <- if (length(sink_ids) > 0) sink_ids[1] else rendered_ids[length(rendered_ids)]
    lines <- c(lines, paste0('  "', anchor_id,
                              '" -> _footnotes [style="invis", weight=10];'))
    lines <- c(lines, "")
  }

  lines <- c(lines, "}")
  paste(lines, collapse = "\n")
}


#' Render a VAST model as an interactive diagram
#'
#' Displays the diagram in the RStudio Viewer pane or in a web browser.
#' Requires the \pkg{DiagrammeR} package.
#'
#' @param model A `vast_model` object.
#' @param width Numeric or `NULL`. Widget width in pixels.
#' @param height Numeric or `NULL`. Widget height in pixels.
#'
#' @return A `DiagrammeR` htmlwidget object (rendered as a side-effect).
#'
#' @examples
#' \dontrun{
#' m <- vast_model() |>
#'   add_nodes(vast_concept("X"), vast_concept("Y")) |>
#'   add_edges(vast_causation("X", "Y"))
#' vast_render(m)
#' }
#'
#' @export
vast_render <- function(model, width = NULL, height = NULL) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required. ",
         "Install it with: install.packages('DiagrammeR')",
         call. = FALSE)
  }
  dot_string <- vast_to_dot(model)
  DiagrammeR::grViz(dot_string, width = width, height = height)
}


#' Export a VAST model to SVG
#'
#' Requires packages \pkg{DiagrammeR} and \pkg{DiagrammeRsvg}.
#'
#' @param model A `vast_model` object.
#' @param file Character. Output file path (should end in `.svg`).
#'
#' @return Invisible `NULL`. The SVG file is written as a side-effect.
#'
#' @export
vast_export_svg <- function(model, file = "vast_diagram.svg") {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Package 'DiagrammeR' is required.", call. = FALSE)
  }
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Package 'DiagrammeRsvg' is required. ",
         "Install it with: install.packages('DiagrammeRsvg')",
         call. = FALSE)
  }

  widget  <- vast_render(model)
  svg_str <- DiagrammeRsvg::export_svg(widget)
  writeLines(svg_str, file)
  message("SVG exported to: ", file)
  invisible(NULL)
}


#' Export a VAST model to PNG
#'
#' Requires packages \pkg{DiagrammeR}, \pkg{DiagrammeRsvg}, and \pkg{rsvg}.
#'
#' @param model A `vast_model` object.
#' @param file Character. Output file path (should end in `.png`).
#' @param width Numeric. Image width in pixels.
#' @param height Numeric. Image height in pixels.
#'
#' @return Invisible `NULL`. The PNG file is written as a side-effect.
#'
#' @export
vast_export_png <- function(model, file = "vast_diagram.png",
                            width = 1200, height = 800) {
  if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("Package 'rsvg' is required. ",
         "Install it with: install.packages('rsvg')",
         call. = FALSE)
  }

  tmp_svg <- tempfile(fileext = ".svg")
  vast_export_svg(model, file = tmp_svg)
  svg_raw <- readLines(tmp_svg, warn = FALSE)
  rsvg::rsvg_png(charToRaw(paste(svg_raw, collapse = "\n")),
                 file = file, width = width, height = height)
  unlink(tmp_svg)
  message("PNG exported to: ", file)
  invisible(NULL)
}


#' Print a VAST model summary
#' @param x A `vast_model` object.
#' @param ... Ignored.
#' @export
print.vast_model <- function(x, ...) {
  cat("VAST Model")
  if (nzchar(x$title)) cat(": ", x$title)
  cat("\n")
  if (nzchar(x$analyst)) cat("  Analyst:    ", x$analyst, "\n")
  cat("  Nodes:      ", length(x$nodes), "\n")
  cat("  Edges:      ", length(x$edges), "\n")
  cat("  Groups:     ", length(x$groups), "\n")
  cat("  Naming mode:", x$naming_mode, "\n")
  cat("  Font sizes:  nodes=", x$fontsize_nodes,
      ", names=", x$fontsize_names,
      ", edges=", x$fontsize_edges, "\n")
  cat("  Layout:     ", x$rankdir, "\n")
  invisible(x)
}
