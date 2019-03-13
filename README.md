[![Travis-CI build Status](https://travis-ci.org/ahgroup/DSAIRM.svg?branch=master)](https://travis-ci.org/ahgroup/DSAIRM)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ahgroup/DSAIRM?branch=master&svg=true)](https://ci.appveyor.com/project/ahgroup/DSAIRM)
[![Coverage status](https://codecov.io/gh/ahgroup/DSAIRM/branch/master/graph/badge.svg)](https://codecov.io/github/ahgroup/DSAIRM?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/DSAIRM)](https://cran.r-project.org/package=DSAIRM)

# DSAIRM
Dynamical Systems Approach to Immune Response Modeling

## Description
This R package consists of a set of simulations (refered to here as apps) that teach within-host infection dynamics and immune response modeling from a dynamical system perspective. By manipulating the models through a graphical (Shiny) user interface and working through the provided instructions, you can learn about some important concepts of within-host and immmune response modeling. 

## Installation
I assume you have `R` installed. I also highly recommend `RStudio`, though it's not required.

1. Install the CRAN release in the usual way with `install.packages('DSAIRM')`.
2. The latest development version (more features but potentially buggy) can be installed from github, using the `devtools` package. If you don't have it, install it first. The following commands will get you up and running:

```r
install.packages('devtools')
devtools::install_github('ahgroup/DSAIRM')
```
## Basic Use
After install (which you need to do only once), load the package by runing `library('DSAIRM')`. You should receive a short greeting. Now you can open the DSAIRM main menu by running `dsairmmenu()`. From the main menu, choose the different apps corresponding to different modeling topics and scenarios. Each app contains information on the model and topic that are covered. Each app also contains a list of recommeded tasks to work through in order to learn about a specific topic. Once done exploring, close the main menu to exit back to the `R` console.

## Advanced Use
You can call the underlying simulation functions directly from the `R` console. You can also download the code for all functions from the main menu and modify them to your own needs. See [the package vignette](https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html) for more details on the different ways to use the package. 

## Contributing to the package
The package is on GitHub and you can use the usual GitHub process to file bug reports, send feature requests, contribute updates and bug fixes, etc. If you have any comments or feedback, I very much welcome them. Please file a [GitHub issue](https://github.com/ahgroup/DSAIRM/issues) and let me know what you think.

The package is built in a way that makes it (hopefully) easy for others to contribute new simulations/apps. To that end, the package contains [this Markdown file](https://github.com/ahgroup/DSAIRM/blob/master/inst/docsfordevelopers/documentation.md) which provides further information on the details of the package structure. If you plan to develop new apps, or make other substantial contributions, it's best to get in touch with me first via email or GitHub.


## Further information
* A 'companion' package to this one, called Dynamical Systems Approaches for Infectious Disease Epidemiology (DSAIDE), focuses on models for infectious disease epidemiology (the population level). It has the same structure as DSAIRM. [See the DSAIDE site for more information.](https://ahgroup.github.io/DSAIDE/index.html)
* I have solutions (in progress) to most of the 'What to do' tasks for the different apps. If you are an instructor using this package as part of a class, email me if you are interested in having access to these solutions.

## Citation and Contributors
This R package is developed and maintained by [Andreas Handel](http://handelgroup.uga.edu/). 

A full list of contributors [can be found here](https://ahgroup.github.io/DSAIRM/authors.html).



