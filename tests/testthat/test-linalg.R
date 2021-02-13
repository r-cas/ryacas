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

test_that("x is not square", {
  expect_error(
    det(ysym(matrix(c(1:10), ncol = 2)))
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

## yacas does not require a square matrix for `Trace`.
## It simply gets the sum of the diagonal elements of a matrix.
## Should we remove the requirement in tr.default that the input is a square matrix?

tr(ysym(matrix(c(1:10), ncol = 2)))

## In yacas, if the input is a vector, `Trace` simply gets the sum of all the elements of the vector.
## Should we remove the requirement in tr.yac_symbol that the input is a matrix?
## x := {1, 2, 3, 4};
## Trace(x);
## 10

# MatrixPower

# Example 6

G <- unname(Sigma)
ex6.default <- pow(G, 4)
ex6.yac_symbol <- as_r(pow(ysym(G), 4))
test_that("Equal to matrix multiplication", {
  expect_true(
    all.equal(
      ex6.default,
      ex6.yac_symbol,
      G %*% G %*% G %*% G
    )
  )
})

# Example 7

ex7.default <- pow(G, 0)
ex7.yac_symbol <- as_r(pow(ysym(G), 0))
test_that("Identity Matrix", {
  expect_true(
    all.equal(
      ex7.default,
      ex7.yac_symbol,
      diag(dim(G)[1])
    )
  )
})

# Example 8

ex8.default <- pow(G, 1)
ex8.yac_symbol <- as_r(pow(ysym(G), 1))
test_that("G", {
  expect_true(
    all.equal(
      ex8.default,
      ex8.yac_symbol,
      G
    )
  )
})

# Example 9

## pow.yac_symbol does not accept negative powers

InverseG <- solve(G)
ex9.default <- pow(G, -4)
#ex9.yac_symbol <- as_r(pow(ysym(G), -4))
test_that("Negative power", {
  expect_true(
    all.equal(
      ex9.default,
      #ex9.yac_symbol,
      InverseG %*% InverseG %*% InverseG %*% InverseG
    )
  )
})

# Example 10

## pow.yac_symbol does not accept negative powers

ex10.default <- pow(G, -1)
#ex10.yac_symbol <- as_r(pow(ysym(G), -1))
test_that("Inverse", {
  expect_true(
    all.equal(
      ex10.default,
      #ex10.yac_symbol,
      InverseG
    )
  )
})

# Vectorize

# Example 11

ex11.default <- vec(G)
ex11.yac_symbol <- vec(ysym(G))
test_that("vec", {
  expect_true(
    all.equal(
      ex11.default,
      ex11.yac_symbol,
      as.vector(G)
    )
  )
})

# HalfVectorize

# Example 12

H <- matrix(
  c(1, 2, 2, 3),
  ncol = 2
)
ex12.default <- vech(H)
ex12.yac_symbol <- vech(ysym(H))
test_that("vech 2 by 2", {
  expect_true(
    all.equal(
      ex12.default,
      ex12.yac_symbol
    )
  )
})

I <- matrix(
  c(1, 2, 3, 2, 4, 5, 3, 5, 6),
  ncol = 3
)

ex13.default <- vech(I)
ex13.yac_symbol <- vech(ysym(I))
test_that("vech 3 by 3", {
  expect_true(
    all.equal(
      ex13.default,
      ex13.yac_symbol
    )
  )
})

# Errors

test_that("Square", {
  expect_error(
    vech(matrix(1:10, ncol = 2))
  )
})

test_that("Symmetric", {
  expect_error(
    vech(matrix(1:9, ncol = 3))
  )
})
