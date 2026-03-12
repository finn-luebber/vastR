test_that("vast_concept creates a valid node", {
  n <- vast_concept("X")
  expect_s3_class(n, "vast_node")
  expect_equal(n$id, "X")
  expect_equal(n$type, "concept")
  expect_equal(n$shape, "box")
  expect_equal(n$label, "X")
})

test_that("vast_concept accepts custom parameters", {
  n <- vast_concept("C1", label = "My Concept", fillcolor = "red",
                    fontsize = 14, width = 2.0)
  expect_equal(n$label, "My Concept")
  expect_equal(n$fillcolor, "red")
  expect_equal(n$fontsize, 14)
  expect_equal(n$width, 2.0)
})

test_that("vast_name wraps label in quotes", {
  n <- vast_name("n1", label = "Sun")
  expect_s3_class(n, "vast_node")
  expect_equal(n$type, "name")
  expect_true(grepl("Sun", n$label))
  expect_equal(n$style, "filled,dashed")
})

test_that("vast_data creates HTML label node", {
  n <- vast_data("D1", label = "Survey")
  expect_s3_class(n, "vast_node")
  expect_equal(n$type, "data")
  expect_equal(n$shape, "plaintext")
  expect_true(isTRUE(n$is_html))
  expect_true(grepl("Survey", n$label))
  expect_true(grepl("TABLE", n$label))
})

test_that("vast_is and vast_ought create pentagons", {
  is_node <- vast_is("i1", value = 2)
  expect_equal(is_node$shape, "pentagon")
  expect_true(grepl("IS", is_node$label))
  expect_true(grepl("2", is_node$label))

  ought_node <- vast_ought("o1")
  expect_equal(ought_node$shape, "pentagon")
  expect_equal(ought_node$label, "OUGHT")
})

test_that("vast_perspective creates an ellipse", {
  p <- vast_perspective("p1", holder = "Daniel", value = 0.9)
  expect_equal(p$shape, "ellipse")
  expect_true(grepl("Daniel", p$label))
  expect_true(grepl("0.9", p$label))
})

test_that("vast_diamond accepts custom labels", {
  d1 <- vast_diamond("d1", "AND")
  expect_equal(d1$shape, "diamond")
  expect_equal(d1$label, "AND")

  d2 <- vast_diamond("d2", "Y = 2X + Z")
  expect_equal(d2$label, "Y = 2X + Z")
})

test_that("vast_noise_source creates invisible point", {
  ns <- vast_noise_source("ns1")
  expect_equal(ns$shape, "point")
  expect_equal(ns$style, "invis")
})
