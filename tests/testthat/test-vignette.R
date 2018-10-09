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

if (FALSE) {
  yacas("PrettyForm(texp)")
  yacas("TeXForm(texp)", retclass = "unquote")
  
  PrettyForm(texp)
  TeXForm(texp)
}
