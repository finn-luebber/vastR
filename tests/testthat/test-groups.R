test_that("vast_group creates a valid group", {
  g <- vast_group("G1", label = "My Group", node_ids = c("X", "Y"))
  expect_s3_class(g, "vast_group")
  expect_equal(g$id, "G1")
  expect_equal(g$label, "My Group")
  expect_equal(g$node_ids, c("X", "Y"))
  expect_equal(g$child_group_ids, character(0))
})

test_that("vast_nested_group includes child group ids", {
  g <- vast_nested_group("outer", node_ids = c("Z"),
                         child_group_ids = c("inner1", "inner2"))
  expect_s3_class(g, "vast_group")
  expect_equal(g$child_group_ids, c("inner1", "inner2"))
  expect_equal(g$node_ids, c("Z"))
})
