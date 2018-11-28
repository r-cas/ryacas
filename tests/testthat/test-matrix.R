context("Matrix")

x <- paste0("x", 1:2)
xs <- as.Sym(x)

test_that("Sym vec", {
  expect_equal(1L, get_xml_list_depth(yacas_evaluate(xs)[1]))
  expect_equal(x, c("x1", "x2"))
  expect_s3_class(xs, "Sym")
  expect_s3_class(xs, "character")
  expect_output(print(xs), "Yacas vector:\\n\\[1\\] x1 x2")
})

xs_eval <- Eval(xs, list(x1 = 2, x2 = 3))

test_that("Sym vec eval", {
  expect_equal(unlist(xs_eval), c(2, 3))
})


test_that("Sym mat", {
  N <- 3
  Achr <- diag("1", 1 + N)
  Achr[cbind((1:N)+1, 1:N)] <- "-a"
  Achr[cbind((2:(N+1))-1, 2:(N+1))] <- "-sin(b)"
  As <- as.Sym(Achr)
  
  expect_equal(2L, get_xml_list_depth(yacas_evaluate(As)[1]))
  expect_equal(yacas(As)$LinAlgForm, Achr)
  expect_equal(yacas(As)$LinAlgDim, dim(Achr))
  expect_s3_class(As, "Sym")
  expect_s3_class(As, "character")
  expect_output(print(As), "Yacas matrix:\\n     \\[,1\\] \\[,2\\]    \\[,3\\]    \\[,4\\]   \\n\\[1,\\] 1    -sin\\(b\\) 0       0      \\n\\[2,\\] -a   1       -sin\\(b\\) 0      \\n\\[3,\\] 0    -a      1       -sin\\(b\\)\\n\\[4,\\] 0    0       -a      1      ")
})

test_that("Sym mat/vec error", {
  # Lists of different depths
  expect_true(is.infinite(get_xml_list_depth(yacas_evaluate("{ x1, { x2 } }")[1])))
  expect_true(is.infinite(get_xml_list_depth(yacas_evaluate("{ { x1 }, x2 }")[1])))
  
  expect_true(is.infinite(get_xml_list_depth(yacas_evaluate("{ { x1 }, { x2, a } }")[1])))
})


test_that("Sym mat", {
  A <- matrix(paste0(paste0("a", 1:2), rep(1:2, each = 2)), 2, 2)
  As <- as.Sym(A)
  Avals <- list(a11 = 11, a12 = 12, a21 = 21, a22 = 22)
  Ares <- Eval(As, Avals)
  expect_equal(Ares %*% Ares, Eval(As*As, Avals))
})

