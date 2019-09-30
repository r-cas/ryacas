---
title: 'Ryacas: A computer algebra system in R'
authors:
- affiliation: 1
  name: Mikkel Meyer Andersen
  orcid: 0000-0002-0234-0266
- affiliation: 1
  name: Søren Højsgaard
  orcid: 0000-0002-3269-9552
date: "23 September 2019"
bibliography: paper.bib
tags:
- cas
- mathematics
- symbolic mathematics
- statistics
- tex
- latex
affiliations:
- index: 1
  name: Department of Mathematical Sciences, Aalborg University, Denmark
---

# Summary

`Ryacas` is an `R` [@R] package that enables a 
computer algebra system (CAS) within `R` via the open source computer 
algebra system (CAS) `yacas` [@Pinkus2002; @yacas], which is short 
for "**y**et **a**nother **c**omputer **a**lgebra **s**ystem".

`Ryacas` both includes a high-level (symbol) interface 
using `R` objects like matrices and vectors as well as direct access to 
the underlying `yacas` such that the user can use the full `yacas` 
system, including for example defining new summation rules. 

From a statistician's perspective, `Ryacas` does provide convenient
tools directly in `R` like

* sums, 
* limits, 
* differentiation, 
* integration, 
* symbolic matrices and vectors,
* simplification, and
* outputting in TeX format, 

which are helpful in both research and teaching.  With `Ryacas`, these
tools are conveniently available from within `R` through
`Ryacas`. However, it must be stressed that `yacas` is nowhere as
powerful as the larger commercial CASs.

`yacas` is easy to use and extensible so that the user can 
define new rules, for example for simplification or summations.
More information about `yacas` is available at <http://www.yacas.org/>. 

`Ryacas` contains a number of vignettes that described both 
how to use the high-level interface directly with `R` objects and how 
to use the underlying `yacas` system.

`Ryacas` contains a version of `yacas` which is bundled into `Ryacas` using  `Rcpp` [@Rcpp]. 
This means that `Ryacas` can be installed like any other R package with no special installation steps being required.


# References
