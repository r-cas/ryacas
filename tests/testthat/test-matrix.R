context("Matrix")

x <- paste0("x", 1:2)
xs <- as.Sym(x)

test_that("Sym vec", {
  expect_equal(x, c("x1", "x2"))
  expect_s3_class(xs, "Sym")
  expect_s3_class(xs, "character")
  expect_output(print(xs), "Sym vector:\\n\\( x1 \\), \\( x2 \\)")
})

xs_eval <- Eval(xs, list(x1 = 2, x2 = 3))

test_that("Sym vec eval", {
  expect_equal(unlist(xs_eval), c(2, 3))
})



# 
# A <- matrix(paste0(paste0("a", 1:2), rep(1:2, each = 2)), 2, 2)
# As <- as.Sym(A)
# Eval(As, list(a11 = 11, a12 = 12, a21 = 21, a22 = 22))
# 
# As*As
