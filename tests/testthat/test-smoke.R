context("Ryacas smoke tests")

test_that("Version", {
  expect_equal(yac_str("Version()"), "1.9.2")
})

test_that("yac_str()", {
  expect_equal(yac_str("Factor(x^2-1)"), "(x+1)*(x-1)")
  expect_equal(yac_str("Expand((x+1)*(x-1))"), "x^2-1")
})

test_that("yac_str()", {
  expect_equal(yac_expr("Factor(x^2-1)"), expression((x+1)*(x-1)))
  expect_equal(yac_expr("Expand((x+1)*(x-1))"), expression(x^2-1))
  
  expect_equal(eval(yac_expr("Factor(x^2-1)"), list(x = -1)), 0)
  expect_equal(eval(yac_expr("Factor(x^2-1)"), list(x =  1)), 0)
})

test_that("yac_silent()", {
  expect_equal(0L, length(capture.output(yac_silent("Factor(x^2-1)"))))
})

test_that("yac_core()", {
  x <- yac_core("Factor(x^2-1)")
  expect_equal(length(x), 2L)
  expect_equal(x[1L], "")
  expect_equal(x[2L], "(x+1)*(x-1);")
})

test_that("yac()", {
  expect_equal(yac("Version()"), "1.9.2")
  expect_equal(yac("Version()", rettype = "str"), "1.9.2")
  expect_equal(yac("Version()", rettype = "expr"), expression("1.9.2"))
  expect_equal(yac("Version()", rettype = "silent"), invisible("1.9.2"))
})

test_that("y_*()", {
  expect_equal(as_y(1:9), "{1, 2, 3, 4, 5, 6, 7, 8, 9}")
  expect_equal(as_r("{1, 2, 3, 4, 5, 6, 7, 8, 9}"), as.character(1:9))
  expect_equal(as_r(as_y(1:9)), as.character(1:9))
  
  x <- matrix(1:9, 3, 3)
  y <- "{{1, 4, 7}, {2, 5, 8}, {3, 6, 9}}"
  expect_equal(as_y(x), y)
  expect_equal(as_r(y), apply(x, 2, as.character))
  expect_equal(as_r(y), apply(x, 2, as.character))
})

test_that("y_print()", {
  A <- diag(4)
  Ayac <- as_y(A)
  expect_equal(paste0(capture.output(y_print(Ayac)), collapse = "\n"), 
               "{{1, 0, 0, 0},\n {0, 1, 0, 0},\n {0, 0, 1, 0},\n {0, 0, 0, 1}} ")
  
  B <- A
  B[2, 2] <- "-t"
  Byac <- as_y(B)
  expect_equal(paste0(capture.output(y_print(Byac)), collapse = "\n"), 
               "{{ 1,  0,  0,  0},\n { 0, -t,  0,  0},\n { 0,  0,  1,  0},\n { 0,  0,  0,  1}} ")
})

test_that("Solve", {
  yac_silent("poly := x^2 - 1")
  
  x1 <- yac_str("Solve(poly == 0, x)")
  expect_equal(x1, "{x==1,x==(-1)}")
  y1 <- x1 %>% y_rmvars() %>% yac_str()
  expect_equal(y1, "{1,-1}")
  expect_equal(sort(as_r(y1)), sort(as_r("{1,-1}")))
})

test_that("y_fn()", {
  x <- yac_core("Factor(x^2-1)")
  expect_equal(length(x), 2L)
  expect_equal(x[1L], "")
  expect_equal(x[2L], "(x+1)*(x-1);")
})

# y_eval

test_that("y_eval(as.r=FALSE)", {
  eq <- ysym("2*y+x^2+2*x-3")
  expect_equal(y_eval(eq, x=3, y=2)$yacas_cmd, "16")
})

test_that("y_eval(as.r=TRUE)", {
  eq <- ysym("2*y+x^2+2*x-3")
  expect_equal(y_eval(eq, x=3, y=2, as.r=TRUE), 16)
})