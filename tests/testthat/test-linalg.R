# Determinant

# Example 1
A <- mtcars[, c(1, 3, 4, 5, 6, 7)]
Sigma <- cov(A)
ex1.default <- det(Sigma)
ex1.yac_symbol <- det(ysym(Sigma))
ex1.yac_symbol <- y_fn(ex1.yac_symbol, "N", "10")
ex1.yac_symbol <- as.numeric(ex1.yac_symbol$yacas_cmd)
test_that("3678523", {
  expect_equal(
    ex1.default,
    ex1.yac_symbol
  )
})
# Example 2
B <- matrix(
  c("x1", "x3", "x2", "x4"),
  ncol = 2
)
ex2.yac_symbol <- det(ysym(B))
test_that("x1*x4-x2*x3", {
  expect_equal(
    "x1*x4-x2*x3",
    ex2.yac_symbol$yacas_cmd
  )
})
# Example 3
C <- matrix(
  c(1, 0.5, 0.25,
    0.5, 1, 0.75,
    0.25, 0.75, 1),
  ncol = 3
)
x1 <- C[1, 1]
x2 <- C[2, 1]
x3 <- C[3, 1]
x4 <- C[1, 2]
x5 <- C[2, 2]
x6 <- C[3, 2]
x7 <- C[1, 3]
x8 <- C[2, 3]
x9 <- C[3, 3]
C <- matrix(
  c("x1", "x4", "x7",
    "x2", "x5", "x8",
    "x3", "x6", "x9"),
  ncol = 3
)
ex3.yac_symbol <- det(ysym(C))
D <- matrix(
  c(x1, x4, x7,
    x2, x5, x8,
    x3, x6, x9),
  ncol = 3
)
ex3.default <- det(D)
test_that("x1*x5*x9-x1*x6*x8+x3*x4*x8-x2*x4*x9+x2*x6*x7-x3*x5*x7", {
  expect_equal(
    "x1*x5*x9-x1*x6*x8+x3*x4*x8-x2*x4*x9+x2*x6*x7-x3*x5*x7",
    ex3.yac_symbol$yacas_cmd
  )
})
test_that("0.3125", {
  expect_equal(
    ex3.default,
    eval(yac_expr(ex3.yac_symbol))
  )
})

# Errors

test_that("x is not of class yac_symbol", {
  expect_error(
    det.yac_symbol(C)
  )
})

test_that("x is not a yac_symbol matrix", {
  expect_error(
    det(ysym(c(1:5)))
  )
})

# Trace

# Example 4

E <- matrix(
  c("x1", "x4", "x7",
    "x2", "x5", "x8",
    "x3", "x6", "x9"),
  ncol = 3
)
F <- matrix(
  c(1, 11, 6, 0, 5, 12, 3, 2, -5),
  ncol = 3
)
ex4.yac_symbol <- tr(ysym(E))
test_that("x1+x5+x9", {
  expect_equal(
    "x1+x5+x9",
    ex4.yac_symbol$yacas_cmd
  )
})

# Example 5

ex5.yac_symbol <- tr(ysym(F))
ex5.default <- tr(F)
test_that("1", {
  expect_equal(
    ex5.default,
    eval(yac_expr(ex5.yac_symbol))
  )
})

# Errors

test_that("x is not of class yac_symbol", {
  expect_error(
    tr.yac_symbol(E)
  )
})

test_that("x is not a yac_symbol matrix", {
  expect_error(
    tr(ysym(c(1:5)))
  )
})

test_that("x is not numeric", {
  expect_error(
    tr(E)
  )
})

test_that("x is not a matrix", {
  expect_error(
    tr(c(1:10))
  )
})

test_that("x is not square", {
  expect_error(
    tr(matrix(c(1:10), ncol = 2))
  )
})
