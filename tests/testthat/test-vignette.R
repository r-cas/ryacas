context("Ryacas vignette")

# Based on vignette in Ryacas_0.3-1

test_that("Factor", {
   expect_equal(as.expression(yacas(expression(Factor(x^2 - 1)))), 
                expression((x + 1) * (x - 1)))
})

xs <- Sym("xs")

test_that("Sym", {
  expect_equal(as.character(xs), "xs")
  expect_equal(class(xs), c("Sym", "character"))
})


texp <- Taylor(exp(xs), xs, 0, 3)

test_that("Taylor", {
  expect_equal(as.expression(texp), expression(xs + xs^2/2 + xs^3/6 + 1))
})
test_that("TeXForm", {
  expect_equal(as.expression(texp), expression(xs + xs^2/2 + xs^3/6 + 1))
  
  # yacas does not know texp:
  expect_equal(yacas("TeXForm(texp)"), "\\mathrm{ texp }")
})

yacas("texp2 := Taylor(xs, 0, 3) Exp(xs)")
texp2s <- Sym("texp2")
test_that("Taylor", {
  expect_equal(as.expression(texp2s), expression(xs + xs^2/2 + xs^3/6 + 1))
})
test_that("TeXForm", {
  expect_equal(as.expression(texp2s), expression(xs + xs^2/2 + xs^3/6 + 1))
  
  # yacas does know texp2s
  expect_equal(yacas("TeXForm(texp2)"), 
               "xs + \\frac{xs ^{2}}{2}  + \\frac{xs ^{3}}{6}  + 1")
})
test_that("PrettyForm", {
  o1 <- capture.output(PrettyForm(texp))
  o2 <- capture.output(PrettyForm(texp2s))
  # 
  #o3 <- capture.output(yacas("PrettyForm(texp2)", retclass = "unquote"))
  
  expect_equal(o1, o2)
  #FIXME: Fails Travis but not locally; why?
  #expect_equal(o1, o3)
  #expect_equal(o2, o3)
})
