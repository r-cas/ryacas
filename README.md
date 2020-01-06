
<!-- README.md is generated from README.Rmd. Please edit only README.Rmd! -->

# Ryacas

[![Build
Status](https://travis-ci.org/r-cas/ryacas.svg?branch=master)](https://travis-ci.org/r-cas/ryacas)
[![Build
status](https://ci.appveyor.com/api/projects/status/c8fsb1dvj5gmh703/branch/master?svg=true)](https://ci.appveyor.com/project/r-cas/ryacas/branch/master)
[![JOSS
status](https://joss.theoj.org/papers/69b3947f8900504a25aa36f65d14500b/status.svg)](https://joss.theoj.org/papers/69b3947f8900504a25aa36f65d14500b)
[![DOI](https://zenodo.org/badge/36067045.svg)](https://zenodo.org/badge/latestdoi/36067045)

Ryacas is an [R](https://www.r-project.org/) interface to the free
[yacas](http://www.yacas.org) Computer Algebra System. Ryacas allows one
to send unprocessed yacas strings and certain other R objects to yacas
process from R and get back the result. It also has facilities for
manipulating yacas strings and R expressions destined for yacas
processing.

It can be used for arbitrary-precision arithmetic, symbolic math, ASCII
pretty printing and translating R to TeX.

## Install from GitHub

To build and install from Github using R 3.3.0 (or later) and the R
`devtools` package 1.11.0 (or later) run this command from within `R`:

    devtools::install_github("r-cas/ryacas0", 
                             build_opts = c("--no-resave-data", "--no-manual"))

You can also install the package without vignettes if needed as follows:

    devtools::install_github("r-cas/ryacas")

## Online info

For vignettes, overview, pointers to additional information,
installation instructions and a sample session see
<http://r-cas.github.io/ryacas/>.

Yacas documentation can be found at <http://yacas.readthedocs.org/>.

## Contribute, issues, and support

Please use the issue tracker at <https://github.com/r-cas/ryacas/issues>
if you want to notify us of an issue or need support. If you want to
contribute, please either create an issue or make a pull request.

## Brief examples

Below we show a few examples. We highly recommend reading the “[Getting
started](http://r-cas.github.io/ryacas/articles/getting-started.html)”
vignette (and the other vignettes) for a more thorough introduction to
the package.

There are two interfaces: a high-level interface that makes `yacas`
objects work similar to `R` objects, and a low-level interface where the
user can write `yacas` code and get results as strings or as `R`
expressions. Below, we demonstrate both.

### High-level interface

A brief example with a polynomial is:

``` r
x <- ysym("x")
p <- x^2+x-6
p
#> y: x^2+x-6
y_fn(p, "Factor")
#> y: (x-2)*(x+3)
p %>% y_fn("Factor")
#> y: (x-2)*(x+3)
p %>% as_r()
#> expression(x^2 + x - 6)
```

A small matrix example follows:

``` r
A <- outer(0:3, 1:4, "-") + diag(2:5)
a <- 1:4
B <- ysym(A)
B
#> {{ 1, -2, -3, -4},
#>  { 0,  2, -2, -3},
#>  { 1,  0,  3, -2},
#>  { 2,  1,  0,  4}}
solve(B)
#> {{   37/202,     3/101,    41/202,    31/101},
#>  {(-17)/101,    30/101,     3/101,     7/101},
#>  {(-19)/202,  (-7)/101,    39/202,  (-5)/101},
#>  { (-5)/101,  (-9)/101, (-11)/101,     8/101}}
B[2, 3] <- "x"
B
#> {{ 1, -2, -3, -4},
#>  { 0,  2,  x, -3},
#>  { 1,  0,  3, -2},
#>  { 2,  1,  0,  4}}
b <- ysym(a)
b[1] <- "x"
b
#> {x, 2, 3, 4}
B %*% b
#> {x-29, 3*x+4-12, x+9-8, 2*x+18}
t(B)
#> {{ 1,  0,  1,  2},
#>  {-2,  2,  0,  1},
#>  {-3,  x,  3,  0},
#>  {-4, -3, -2,  4}}
B[, 2:3]
#> {{-2, -3},
#>  { 2,  x},
#>  { 0,  3},
#>  { 1,  0}}
```

### Low-level interface

Returning strings with `yac_str()`:

``` r
yac_str("x+x+x+x")
#> [1] "4*x"
yac_str("Factor(x^2+x-6)")
#> [1] "(x-2)*(x+3)"
yac_str("D(x) x^2+x-6")
#> [1] "2*x+1"
```

Returning `R` expressions with `yac_expr()`:

``` r
yac_expr("x+x+x+x")
#> expression(4 * x)
eval(yac_expr("x+x+x+x"), list(x = 4))
#> [1] 16
yac_expr("Factor(x^2+x-6)")
#> expression((x - 2) * (x + 3))
yac_expr("D(x) x^2+x-6")
#> expression(2 * x + 1)
```

Using functions easier (using
[`magrittr`](https://cran.r-project.org/package=magrittr)’s pipe,
`%>%`):

``` r
"x^2+x-6" %>% y_fn("Factor") %>% yac_str()
#> [1] "(x-2)*(x+3)"
"x^2+x-6" %>% y_fn("D(x)") %>% yac_expr()
#> expression(2 * x + 1)
```

Solving equations (removes the `x==` from `yacas` with the `y_rmvars()`
function):

``` r
sol <- yac_str("Solve(x^2+x-6 == 0, x)")
sol
#> [1] "{x==2,x==(-3)}"
sol %>% y_rmvars() %>% yac_str()
#> [1] "{2,-3}"
sol %>% y_rmvars() %>% yac_expr()
#> expression(c(2, -3))
sol %>% y_rmvars() %>% yac_expr() %>% eval()
#> [1]  2 -3
```

And output in TeX:

``` r
"3/4 + Pi/8" %>% y_fn("Simplify") %>% y_fn("TeXForm") %>% yac_str()
#> [1] "\\frac{\\pi  + 6}{8} "
```

And arbitrary precision (see also the “[Arbitrary-precision
arithmetic](http://r-cas.github.io/ryacas/articles/arbitrary-precision.html)”
vignette):

``` r
yac_str("N(Pi, 50)")
#> [1] "3.1415926535897932384626433832795028841971693993751058209"
```

## Yacas

The package contains the yacas distribution. The development of the
yacas source code is available at
<https://github.com/grzegorzmazur/yacas/> . For more information on
yacas see <http://www.yacas.org/> or the documention directly at
<https://yacas.readthedocs.io/>.

-----

Mikkel Meyer Andersen, mikl at math dot aau dot dk  
Rob Goedman, goedman at mac dot com  
Gabor Grothendieck, ggrothendieck at gmail dot com  
Søren Højsgaard, sorenh at math dot aau dot dk  
Ayal Pinkus, apinkus at xs4all dot nl  
Grzegorz Mazur, teoretyk at gmail dot com
