test_that("# 48", {
  cmd <- "Solve(a^2 + b^2 == c^2, b)"
  
  e <- cmd %>% yac_str() %>% yac_expr()
  expect_false(grepl('fabs(', as.character(e), fixed = TRUE))
  
  e <- cmd %>% y_rmvars() %>% yac_expr()
  expect_equal(sort(round(eval(e, list(a = 2, c = 3)), 4)), 
               c(-2.2361+0i, 2.2361+0i))
})
