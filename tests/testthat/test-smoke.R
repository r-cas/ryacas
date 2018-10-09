context("Ryacas smoke tests")

test_that("Solve", {
    Asym <- Sym("Asym")
    xmid <- Sym("xmid")
    scal <- Sym("scal")
    x <- Sym("x")
    y <- Sym("y")

    r <- Solve(Asym/(1+exp((xmid-x)/scal))==y, x)

    expect_equal(as.expression(r), expression(list(x == xmid - log(Asym/y - 1) * scal)))
})

test_that("Limit", {
    x <- Sym("x")
    r <- Limit(1/(1-x), x, Infinity)
    expect_equal(as.expression(r), expression(0))
})

test_that("Differentiation", {
    x <- Sym("x")
    r <- deriv(x^3, x)
    expect_equal(as.expression(r), expression(3 * x^2))
})

test_that("Integration", {
    x <- Sym("x")
    r <- Integrate(x^3, x)
    expect_equal(as.expression(r), expression(x^4/4))
    r <- Integrate(x^3, x, 0, 1)
    expect_equal(as.expression(r), expression(1/4))
})

test_that("Limit", {
    x <- Sym("x")
    h <- Sym("h")
    r <- Limit((cos(x+h)-cos(x))/h, h, 0)
    expect_equal(as.expression(r), expression(-sin(x)))
})

