context("Ryacas symbol")

##############################

test_that("dim()/length()", {
  for (nrow in 1:4) {
    info <- paste0("length = ", nrow)
    vec <- integer(nrow)
    expect_null(dim(vec))
    expect_equal(length(vec), nrow, info = info)
    
    for (ncol in 1:4) {
      info <- paste0("nrow = ", nrow, "; ncol = ", ncol)
      
      mat <- matrix(0, nrow = nrow, ncol = ncol)
      expect_equal(dim(mat), c(nrow, ncol), info = info)
      
      B <- yac_symbol(mat)
      expect_equal(dim(B), dim(mat), info = info)
      expect_equal(length(B), length(mat), info = info)
    }
  }
})

##############################

A <- matrix(1:16, nrow = 4, ncol = 4)
a <- 1:4

test_that("yac_symbol()", {
  B <- yac_symbol(A)
  b <- yac_symbol(a)

  expect_s3_class(B, "yac_symbol")
  expect_s3_class(b, "yac_symbol")
  
  expect_equal(A, as_r(B))
  
  x <- B %*% b
  expect_s3_class(x, "yac_symbol")
  expect_equal(x$yacas_cmd, "{90,100,110,120}")
  
  expect_equal(eval(yac_expr(x)), c(A %*% a))
})

B <- yac_symbol(A)
b <- yac_symbol(a)

test_that("yac_str()", {
  expect_equal(yac_str(B), gsub(" ", "", as_y(A), fixed = TRUE))
})

test_that("yac_expr()", {
  expect_equal(eval(yac_expr(B)), A)
})

test_that("yac_silent()", {
  x <- yac_silent(B) # yac_str()
  expect_equal(x, yac_str(B))
})

test_that("tex()", {
  expect_equal(tex(B), yac_str(y_fn(as_y(A), "TeXForm")))
  expect_equal(tex(B), yac_str(y_fn(as_y(A), "TeXForm")))
})

test_that("y_fn", {
  expect_equal(t(A), as_r(y_fn(B, "Transpose")))
  expect_equal(sum(diag(A)), as_r(y_fn(B, "Trace")))
})

test_that("as_r", {
  expect_equal(as_r(B), apply(as_r(B$yacas_cmd), 2, as.numeric))
  
  # A1 <- A
  # A1[2, 2] <- "x"
  # B1 <- yac_symbol(A1)
  # 
  # expect_equal(as_r(B1), A1)
  
  A1 <- A
  A1[2, 2] <- "x"
  B1 <- yac_symbol(A1)
  
  A2 <- A
  A2[2, 2] <- 999
  expect_equal(eval(yac_expr(B1), list(x = 999)), A2)
})

test_that("diag", {
  expect_equal(diag(A), as_r(diag(B)))
  
  A1 <- A
  B1 <- B
  diag(A1) <- 999
  diag(B1) <- 999
  expect_equal(A1, as_r(B1))
  
  A1 <- A
  B1 <- B
  diag(A1) <- 999
  diag(B1) <- "a"
  expect_equal(A1, eval(as_r(B1), list(a = 999)))
  
  A1 <- A
  B1 <- B
  x <- c("a", "b", "c", "d")
  diag(A1) <- 901:904
  diag(B1) <- x
  expect_equal(A1, eval(as_r(B1), list(a = 901, b = 902, c = 903, d = 904)))
})

test_that("lower.tri/upper.tri", {
  for (func in c(lower.tri, upper.tri)) {
    nm <- gsub('^.*\"(.*)\".*$', "\\1", as.character(body(func))[2])
    
    expect_equal(func(A), func(B), info = nm)
    expect_equal(A[func(A)], as_r(B[func(B)]))
    
    A1 <- A
    B1 <- B
    A1[func(A1)] <- 999
    B1[func(B1)] <- 999
    expect_equal(A1, as_r(B1))
    
    A1 <- A
    B1 <- B
    A1[func(A1)] <- 999
    B1[func(B1)] <- "a"
    expect_equal(A1,eval(as_r(B1), list(a = 999)))
    
    
    A1 <- A
    B1 <- B
    x <- c("a", "b", "c", "d", "e", "f")
    A1[func(A1)] <- 901:906
    B1[func(B1)] <- x
    expect_equal(A1, eval(as_r(B1), 
                          list(a = 901, b = 902, c = 903, 
                               d = 904, e = 905, f = 906)))
  }
})



test_that("solve()", {
  # Modified from solve()
  hilbert_r <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  
  hilbert_y <- function(n) { 
    mat <- matrix("", nrow = n, ncol = n)
    
    for (i in 1:n) {
      for (j in 1:n) {
        mat[i, j] <- paste0("1 / (", (i-1), " + ", j, ")")
      }
    }
    
    return(mat)
  }
  
  A1 <- hilbert_r(4)
  B1 <- yac_symbol(as_y(hilbert_y(4)))

  expect_equal(A1, as_r(B1))
  expect_equal(solve(A1), as_r(solve(B1)))
})


test_that("Getters for vectors", {
  expect_equal(ncol(a), nrow(a))
  expect_equal(ncol(b), nrow(b))
  expect_equal(dim(a), dim(b))
  expect_equal(length(a), length(b))
  
  src <- seq_len(length(a))
  
  # Subsets of size 1, 2, ..., nrow(A)
  for (indices_count in src) {
    idx <- combn(src, indices_count)
    
    for (i in 1L:ncol(idx)) {
      i_idx <- idx[, i]
      expect_equal(a[i_idx], as_r(B[i_idx]))
    }
  }
})


test_that("Getters for matrices", {
  expect_equal(ncol(A), nrow(A))
  expect_equal(ncol(B), nrow(B))
  expect_equal(dim(A), dim(B))
  expect_equal(length(A), length(B))
  
  # Only one subscript, no rows/cols
  for (i in seq_len(length(A))) {
    expect_equal(A[i], as_r(B[i]))
  }
  
  src <- seq_len(nrow(A))
  
  # Subsets of size 1, 2, ..., nrow(A)
  for (indices_count in src) {
    idx <- combn(src, indices_count)

    # All rows
    for (i in 1L:ncol(idx)) {
      i_idx <- idx[, i]
      expect_equal(A[i_idx, ], as_r(B[i_idx, ]))
    }
    
    # All columns
    for (i in 1L:ncol(idx)) {
      i_idx <- idx[, i]
      expect_equal(A[, i_idx], as_r(B[, i_idx]))
    }
    
    if (ncol(idx) == 1L) {
      next
    }
    
    # All subsets
    for (i1 in 1L:(ncol(idx)-1L)) {
      i1_idx <- idx[, i1]
      
      for (i2 in (i1+1L):ncol(idx)) {
        i2_idx <- idx[, i2]
        
        expect_equal(A[i1_idx, i2_idx], as_r(B[i1_idx, i2_idx]), 
                     info = paste0("i1 = ", i1, "; i2 = ", i2))
      }
    }
  }
})



test_that("Setters for matrices", {
  expect_equal(ncol(A), nrow(A))
  expect_equal(ncol(B), nrow(B))
  expect_equal(dim(A), dim(B))
  expect_equal(length(A), length(B))
  
  # Only one subscript, no rows/cols
  for (i in seq_len(length(A))) {
    A1 <- A
    B1 <- B
    
    A1[i] <- 999
    B1[i] <- 999

    expect_equal(A1, as_r(B1))
  }
  
  src <- seq_len(nrow(A))
  
  # Subsets of size 1, 2, ..., nrow(A)
  for (indices_count in src) {
    idx <- combn(src, indices_count)
    
    # All rows
    for (i in 1L:ncol(idx)) {
      i_idx <- idx[, i]

      A1 <- A
      B1 <- B
      A1[i_idx, ] <- 999
      B1[i_idx, ] <- 999
      expect_equal(A1, as_r(B1))
      
      A1 <- A
      B1 <- B
      A1[i_idx, ] <- 900 + seq_along(i_idx)
      B1[i_idx, ] <- 900 + seq_along(i_idx)
      expect_equal(A1, as_r(B1))
      
      A1 <- A
      B1 <- B
      A1[i_idx, ] <- 900 + seq_len(length(i_idx) * ncol(A))
      B1[i_idx, ] <- 900 + seq_len(length(i_idx) * ncol(A))
      expect_equal(A1, as_r(B1))
    }
    
    # All columns
    for (i in 1L:ncol(idx)) {
      i_idx <- idx[, i]
      
      A1 <- A
      B1 <- B
      A1[, i_idx] <- 999
      B1[, i_idx] <- 999
      expect_equal(A1, as_r(B1))
      
      A1 <- A
      B1 <- B
      A1[, i_idx] <- 900 + seq_along(i_idx)
      B1[, i_idx] <- 900 + seq_along(i_idx)
      expect_equal(A1, as_r(B1))
      
      A1 <- A
      B1 <- B
      A1[, i_idx] <- 900 + seq_len(length(i_idx) * ncol(A))
      B1[, i_idx] <- 900 + seq_len(length(i_idx) * ncol(A))
      expect_equal(A1, as_r(B1))
    }
    
    if (ncol(idx) == 1L) {
      next
    }
    
    # All subsets
    for (i1 in 1L:(ncol(idx)-1L)) {
      i1_idx <- idx[, i1]
      
      for (i2 in (i1+1L):ncol(idx)) {
        i2_idx <- idx[, i2]
        
        A1 <- A
        B1 <- B
        A1[i1_idx, i2_idx] <- 999
        B1[i1_idx, i2_idx] <- 999
        expect_equal(A1, as_r(B1), info = paste0("i1 = ", i1, "; i2 = ", i2))
        
        A1 <- A
        B1 <- B
        A1[i1_idx, i2_idx] <- 900 + seq_along(i_idx)
        B1[i1_idx, i2_idx] <- 900 + seq_along(i_idx)
        expect_equal(A1, as_r(B1), info = paste0("i1 = ", i1, "; i2 = ", i2))
        
        A1 <- A
        B1 <- B
        A1[i1_idx, i2_idx] <- 900 + seq_len(length(i1_idx) * length(i2_idx))
        B1[i1_idx, i2_idx] <- 900 + seq_len(length(i1_idx) * length(i2_idx))
        expect_equal(A1, as_r(B1), info = paste0("i1 = ", i1, "; i2 = ", i2))
      }
    }
  }
})

test_that("Derivatives", {
  L <- yac_symbol("x^2 * (y/4) - a*(3*x + 3*y/2 - 45)")
  
  # derivative
  expect_equal(as.character(as_r(deriv(L, "x"))), 
               "(x * y)/2 - 3 * a")
  expect_equal(as.character(as_r(deriv(L, c("x", "y", "a")))), 
               "c((x * y)/2 - 3 * a, x^2/4 - (3 * a)/2, 45 - (3 * x + (3 * y)/2))")
  
  # Hessian
  expect_equal(as.character(as_r(Hessian(L, "x"))), 
               "rbind(c(y/2))")
  expect_equal(as.character(as_r(Hessian(L, c("x", "y", "a")))), 
               "rbind(c(y/2, x/2, -3), c(x/2, 0, -3/2), c(-3, -3/2, 0))")
  
  # Jacobian
  L2 <- yac_symbol(c("x^2 * (y/4) - a*(3*x + 3*y/2 - 45)", 
                     "x^3 + 4*a^2")) # just some function
  expect_equal(as.character(as_r(Jacobian(L2, "x"))), 
               "rbind(c((x * y)/2 - 3 * a), c(3 * x^2))")
  expect_equal(as.character(as_r(Jacobian(L2, c("x", "y", "a")))), 
               paste0("rbind(c((x * y)/2 - 3 * a, x^2/4 - (3 * a)/2, ", 
                      "45 - (3 * x + (3 * y)/2)), c(3 * x^2, 0, 8 * a))"))

})



test_that("solve linear system", {
  # ------------------------------------
  # Input validation
  # ------------------------------------
  poly <- yac_symbol("x^2 - x - 6")
  expect_error(solve(poly))
  
  # ------------------------------------
  # Matrix inverse
  # ------------------------------------
  A <- outer(0:3, 1:4, "-") + diag(2:5)
  a <- 1:4
  B <- yac_symbol(A)
  b <- yac_symbol(a)
  expect_equal(solve(A), as_r(solve(B)))
  
  # ------------------------------------
  # Linear system of equations
  # ------------------------------------
  # Input validation
  expect_error(solve(B, poly))
  
  # Functionality
  expect_equal(solve(A, a), as_r(solve(B, b)))
})

test_that("solve (roots/others)", {
  A <- outer(0:3, 1:4, "-") + diag(2:5)
  a <- 1:4
  B <- yac_symbol(A)
  b <- yac_symbol(a)

  
  poly <- yac_symbol("x^2 - x - 6")
  expect_error(solve(poly))
  
  expect_error(solve(B, poly))
  expect_error(solve(poly, B))
  expect_error(solve(poly, b))
  
  # Roots
  expect_equal(as.character(solve(poly, "x")), "{x==(-2),x==3}")
  expect_equal(as_r(y_rmvars(solve(poly, "x"))), c(-2, 3))
  
  # Equation
  expect_equal(as.character(solve(poly, 3, "x")), "{x==(Sqrt(37)+1)/2,x==(1-Sqrt(37))/2}")
  expect_equal(as.character(solve(poly, 3, "x")), as.character(solve(poly, "3", "x")))
  expect_equal(as_r(y_rmvars(solve(poly, 3, "x"))), c(3.54138126514911, -2.54138126514911))
})

