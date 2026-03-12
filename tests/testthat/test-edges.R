test_that("vast_relation creates a valid edge", {
  e <- vast_relation("X", "Y", type = "c", strength = 0.5)
  expect_s3_class(e, "vast_edge")
  expect_equal(e$from, "X")
  expect_equal(e$to, "Y")
  expect_equal(e$type, "c")
  expect_equal(e$label, "c 0.5")
})

test_that("edge label omits strength when NULL", {
  e <- vast_relation("X", "Y", type = "c")
  expect_equal(e$label, "c")
})

test_that("edge index is appended to type label", {
  e <- vast_relation("X", "Y", type = "c", strength = 0.3, index = "1")
  expect_equal(e$label, "c1 0.3")
})

test_that("empty type produces empty label", {
  e <- vast_relation("X", "Y", type = "")
  expect_equal(e$label, "")
})

test_that("convenience wrappers set correct type and color", {
  expect_equal(vast_naming("A", "B")$type, "n")
  expect_equal(vast_implication("A", "B")$type, "i")
  expect_equal(vast_causation("A", "B")$type, "c")
  expect_equal(vast_transformation("A", "B")$type, "t")
  expect_equal(vast_prediction("A", "B")$type, "p")
  expect_equal(vast_reasoning("A", "B")$type, "r")
  expect_equal(vast_unknown("A", "B")$type, "u")

  expect_equal(vast_unknown("A", "B")$style, "dashed")
})

test_that("vast_noise creates edge from noise source", {
  e <- vast_noise("ns1", "Y", strength = "<> 0")
  expect_s3_class(e, "vast_edge")
  expect_equal(e$type, "noise")
  expect_equal(e$from, "ns1")
  expect_equal(e$label, "<> 0")
})

test_that("vast_noise with no strength has empty label", {
  e <- vast_noise("ns1", "Y")
  expect_equal(e$label, "")
})
