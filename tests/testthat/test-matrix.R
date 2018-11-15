context("Matrix")

xs <- Sym("xs")

test_that("Sym", {
  expect_equal(as.character(xs), "xs")
  expect_equal(class(xs), c("Sym", "character"))
})
