
<!-- README.md is generated from README.Rmd. Please edit only README.Rmd! -->

# Ryacas

[![Build
Status](https://travis-ci.org/mikldk/ryacas.svg?branch=master)](https://travis-ci.org/mikldk/ryacas)
[![Build
status](https://ci.appveyor.com/api/projects/status/c8fsb1dvj5gmh703/branch/master?svg=true)](https://ci.appveyor.com/project/mikldk/ryacas/branch/master)

Ryacas is an [R](https://www.r-project.org/) interface to the free
[yacas](http://www.yacas.org) Computer Algebra System. Ryacas allows one
to send R expressions, unprocessed yacas strings and certain other R
objects to yacas process from R and get back the result. It also has
facilities for manipulating yacas strings and R expressions destined for
yacas processing.

It can be used for exact arithmetic, symbolic math, ASCII pretty
printing and translating R to TeX.

## Install from GitHub

To build and install from github using R 3.3.0 (or later) and the R
`devtools` package 1.11.0 (or later) run this command from within R:

    devtools::install_github("mikldk/ryacas")

## Online info

For vignettes, overview, pointers to additional information,
installation instructions and a sample session see
<http://mikldk.github.io/ryacas/>.

Yacas documentation can be found at <http://yacas.readthedocs.org/>

## Brief examples

Below we show a few examples. Please refer to the [“Getting
started”](http://mikldk.github.io/ryacas/articles/getting-started.html)
vignette (and the other vignettes) for more information.

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

Using functions easier:

``` r
"x^2+x-6" %>% y_fn("Factor") %>% yac_str()
#> [1] "(x-2)*(x+3)"
"x^2+x-6" %>% y_fn("D(x)") %>% yac_expr()
#> expression(2 * x + 1)
```

Solving equations (removes the `x==` from `yacas`):

``` r
yac_str("Solve(x^2+x-6 == 0, x)")
#> [1] "{x==2,x==(-3)}"
yac_solve_str("Solve(x^2+x-6 == 0, x)")
#> [1] "{2,-3}"
yac_solve_expr("Solve(x^2+x-6 == 0, x)")
#> expression(c(2, -3))
"x^2+x-6 == 0" %>% y_fn("Solve", "x") %>% yac_solve_str()
#> [1] "{2,-3}"
```

And output in TeX:

``` r
"3/4 + Pi/8" %>% y_fn("Simplify") %>% y_fn("TeXForm") %>% yac_str()
#> [1] "\\frac{\\pi  + 6}{8} "
```

And arbitrary precision:

``` r
yac_str("N(Pi, 50)")
#> [1] "3.1415926535897932384626433832795028841971693993751058209"
```

## Yacas

The package contains stripped-down yacas distribution. For the complete
yacas source code see <https://github.com/grzegorzmazur/yacas/> . For
more information on yacas see <http://www.yacas.org/> or the documention
directly at <https://yacas.readthedocs.io/>. .

-----

Mikkel Meyer Andersen, mikl at math dot aau dot dk  
Rob Goedman, goedman at mac dot com  
Gabor Grothendieck, ggrothendieck at gmail dot com  
Søren Højsgaard, sorenh at math dot aau dot dk  
Ayal Pinkus, apinkus at xs4all dot nl  
Grzegorz Mazur, teoretyk at gmail dot com
