test_that("# 48", {
  cmd <- "Solve(a^2 + b^2 == c^2, b)"
  
  e <- cmd %>% yac_str() %>% yac_expr()
  expect_false(grepl('fabs(', as.character(e), fixed = TRUE))
  
  e <- cmd %>% y_rmvars() %>% yac_expr()
  expect_equal(sort(round(eval(e, list(a = 2, c = 3)), 4)), 
               c(-2.2361+0i, 2.2361+0i))
})

test_that("# 50", {
  cmd <- "Integrate(x) x^2* Exp(-(x))"
  e <- cmd %>% yac_str()
  expect_equal(e, "(-2)*(x+1)*Exp(-x)-x^2*Exp(-x)")
})

test_that("# 48 - Ceil() -> ceiling()", {
  e <- yac_expr("Ceil(x)")
  expect_equal(1, eval(e, list(x = 0.5)))
})
