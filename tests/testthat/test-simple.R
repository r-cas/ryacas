context("Simple tests")

test_that("Sym", {
  xs <- Sym('xs')
  texp <- Taylor(exp(xs), xs, 0, 3)
  
  expect_equal(as.character(xs), "xs")
  expect_equal(class(xs), c("Sym", "character"))
  
  expect_equal(class(texp), c("Sym", "character"))
  expect_equal(as.expression(texp), expression(xs + xs^2/2 + xs^3/6 + 1))
  expect_equal(as.character(as.expression(texp)), "xs + xs^2/2 + xs^3/6 + 1")
  expect_equal(as.character(texp), "( Taylor( xs , 0 , 3 ) ( Exp ( xs ) ) )")
})

test_that("TeXForm", {
  expect_equal(yacas("TeXForm(x*x)"), "$x ^{2}$")
  expect_equal(TeXForm(yacas("x*x")), "$x ^{2}$")
  
  expect_equal(yacas("TeXForm(x*x)", retclass = "unquote"), "$x ^{2}$")
  
  x <- Sym("x")
  expect_equal(TeXForm(x*x), "$x ^{2}$")
})
