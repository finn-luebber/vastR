test_that("vast_model creates a valid model", {
  m <- vast_model(title = "Test", rankdir = "LR")
  expect_s3_class(m, "vast_model")
  expect_equal(m$title, "Test")
  expect_equal(m$rankdir, "LR")
  expect_equal(length(m$nodes), 0)
  expect_equal(length(m$edges), 0)
  expect_equal(length(m$groups), 0)
})

test_that("add_nodes adds nodes to model", {
  m <- vast_model() |>
    add_nodes(vast_concept("X"), vast_concept("Y"))
  expect_equal(length(m$nodes), 2)
  expect_true("X" %in% names(m$nodes))
  expect_true("Y" %in% names(m$nodes))
})

test_that("add_nodes accepts a list of nodes", {
  nodes <- list(vast_concept("A"), vast_concept("B"))
  m <- vast_model() |> add_nodes(nodes)
  expect_equal(length(m$nodes), 2)
})

test_that("add_edges adds edges to model", {
  m <- vast_model() |>
    add_nodes(vast_concept("X"), vast_concept("Y")) |>
    add_edges(vast_causation("X", "Y"))
  expect_equal(length(m$edges), 1)
})

test_that("add_groups adds groups to model", {
  m <- vast_model() |>
    add_nodes(vast_concept("X"), vast_concept("Y")) |>
    add_groups(vast_group("G1", node_ids = c("X", "Y")))
  expect_equal(length(m$groups), 1)
  expect_true("G1" %in% names(m$groups))
})

test_that("vast_to_dot produces valid DOT structure", {
  m <- vast_model(title = "Test Model") |>
    add_nodes(vast_concept("X"), vast_concept("Y")) |>
    add_edges(vast_causation("X", "Y", strength = 0.5))

  dot <- vast_to_dot(m)

  expect_type(dot, "character")
  expect_true(grepl("^digraph VAST \\{", dot))
  expect_true(grepl("\\}$", dot))
  expect_true(grepl('"X"', dot))
  expect_true(grepl('"Y"', dot))
  expect_true(grepl('"X" -> "Y"', dot))
  expect_true(grepl("c 0.5", dot))
  expect_true(grepl("Test Model", dot))
})

test_that("DOT output includes FIMM label when naming_mode is fimm", {
  m <- vast_model(naming_mode = "fimm") |>
    add_nodes(vast_concept("X"))
  dot <- vast_to_dot(m)
  expect_true(grepl("FIMM", dot))
})

test_that("FIMM mode replaces concept labels with name labels", {
  m <- vast_model(naming_mode = "fimm") |>
    add_nodes(
      vast_concept("S", "S"),
      vast_name("n_sun", "Sun")
    ) |>
    add_edges(vast_naming("S", "n_sun"))
  dot <- vast_to_dot(m)

  # The concept node should display the name label, not "S"
  expect_true(grepl("Sun", dot))
  # The name node should be suppressed
  expect_false(grepl('"n_sun"', dot))
  # The naming edge should be suppressed
  expect_false(grepl('"S" -> "n_sun"', dot))
})

test_that("separated mode hides name nodes and creates legend", {
  m <- vast_model(naming_mode = "separated") |>
    add_nodes(
      vast_concept("S", "S"),
      vast_name("n_sun", "Sun")
    ) |>
    add_edges(vast_naming("S", "n_sun"))
  dot <- vast_to_dot(m)

  # Concept should keep its abstract label
  expect_true(grepl('"S"', dot))
  # Name node should NOT appear in main diagram
  expect_false(grepl('"n_sun" \\[', dot))
  # Naming edge should NOT appear in main diagram
  expect_false(grepl('"S" -> "n_sun"', dot))
  # Legend table node should exist
  expect_true(grepl("_naming_legend", dot))
  expect_true(grepl("Naming", dot))
})

test_that("integrated mode shows everything as-is", {
  m <- vast_model(naming_mode = "integrated") |>
    add_nodes(
      vast_concept("S", "S"),
      vast_name("n_sun", "Sun")
    ) |>
    add_edges(vast_naming("S", "n_sun"))
  dot <- vast_to_dot(m)

  # Both nodes and the naming edge should be present
  expect_true(grepl('"S"', dot))
  expect_true(grepl('"n_sun"', dot))
  expect_true(grepl('"S" -> "n_sun"', dot))
  # No legend, no FIMM
  expect_false(grepl("_naming_legend", dot))
  expect_false(grepl("FIMM", dot))
})

test_that("naming_mode defaults to integrated", {
  m <- vast_model()
  expect_equal(m$naming_mode, "integrated")
})

test_that("grouped nodes appear inside subgraph cluster", {
  m <- vast_model() |>
    add_nodes(vast_concept("X"), vast_concept("Y"), vast_concept("Z")) |>
    add_groups(vast_group("G1", node_ids = c("X", "Y")))
  dot <- vast_to_dot(m)

  expect_true(grepl("cluster_G1", dot))
  # Z should be outside the cluster
  # X and Y should be inside
})

test_that("nested groups produce nested subgraphs", {
  m <- vast_model() |>
    add_nodes(vast_concept("A"), vast_concept("B"), vast_concept("C")) |>
    add_groups(
      vast_group("inner", node_ids = c("A")),
      vast_nested_group("outer", node_ids = c("C"),
                        child_group_ids = c("inner"))
    )
  dot <- vast_to_dot(m)

  expect_true(grepl("cluster_outer", dot))
  expect_true(grepl("cluster_inner", dot))
})

test_that("print.vast_model produces output", {
  m <- vast_model(title = "Print Test") |>
    add_nodes(vast_concept("X"))
  out <- capture.output(print(m))
  expect_true(any(grepl("VAST Model", out)))
  expect_true(any(grepl("Nodes", out)))
})

test_that("fontsize overrides are applied in DOT output", {
  m <- vast_model(fontsize_nodes = 14, fontsize_names = 8, fontsize_edges = 7) |>
    add_nodes(
      vast_concept("X", "X"),
      vast_name("n_x", "Thing")
    ) |>
    add_edges(
      vast_naming("X", "n_x"),
      vast_causation("X", "X")
    )
  dot <- vast_to_dot(m)

  # Concept node should have fontsize=14
  expect_true(grepl('fontsize=14', dot))
  # Name node should have fontsize=8
  expect_true(grepl('fontsize=8', dot))
  # Edges should have fontsize=7
  expect_true(grepl('fontsize=7', dot))
})
