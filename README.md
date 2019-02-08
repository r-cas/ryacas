# Ryacas #

[![Build Status](https://travis-ci.org/mikldk/ryacas.svg?branch=master)](https://travis-ci.org/mikldk/ryacas)
[![Build status](https://ci.appveyor.com/api/projects/status/c8fsb1dvj5gmh703/branch/master?svg=true)](https://ci.appveyor.com/project/mikldk/ryacas/branch/master)

Ryacas is an [R](https://www.r-project.org/) interface to
the free [yacas](http://www.yacas.org) Computer Algebra
System.  Ryacas allows one to send R expressions,
unprocessed yacas strings and certain other R objects to
yacas process from R and get back the result. It also has
facilities for manipulating yacas strings and R expressions
destined for yacas processing.

It can be used for exact arithmetic, symbolic math, ASCII
pretty printing and translating R to TeX. 

## Install from github ##

To build and install from github using R 3.3.0 (or later) and the R devtools package 1.11.0 (or later) run this command from within R:

    remotes::install_github("mikldk/ryacas")

## Online info ##

For vignettes, overview, pointers to additional information, installation
instructions and a sample session see <http://mikldk.github.io/ryacas/>. 
The old site was <http://code.google.com/p/ryacas/>.

Yacas documentation can be found at http://yacas.readthedocs.org/

## More ##

Once Ryacas is installed, pointers to additional information
can be found with these R commands:

    library(Ryacas)
    package?Ryacas


## Yacas ##

The package contains stripped-down yacas distribution. For the complete yacas source code see <https://github.com/grzegorzmazur/yacas/> . For more information on yacas see <http://www.yacas.org/>.

---

Rob Goedman, goedman at mac dot com  
Gabor Grothendieck, ggrothendieck at gmail dot com  
Søren Højsgaard, sorenh at math dot aau dot dk  
Ayal Pinkus, apinkus at xs4all dot nl  
Grzegorz Mazur, teoretyk at gmail dot com  
Mikkel Meyer Andersen, mikl at math dot aau dot dk  
