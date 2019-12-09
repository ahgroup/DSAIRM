[![Travis-CI build Status](https://travis-ci.org/ahgroup/DSAIRM.svg?branch=master)](https://travis-ci.org/ahgroup/DSAIRM)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ahgroup/DSAIRM?branch=master&svg=true)](https://ci.appveyor.com/project/ahgroup/DSAIRM)
[![Coverage status](https://codecov.io/gh/ahgroup/DSAIRM/branch/master/graph/badge.svg)](https://codecov.io/github/ahgroup/DSAIRM?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/DSAIRM)](https://cran.r-project.org/package=DSAIRM)
[![CRAN checks](https://cranchecks.info/badges/summary/DSAIRM)](https://cran.r-project.org/web/checks/check_results_DSAIRM.html)
[![metacran monthly downloads](http://cranlogs.r-pkg.org/badges/DSAIDE)](https://cran.r-project.org/package=DSAIRM)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/DSAIDE?color=ff69b4)](https://cran.r-project.org/package=DSAIRM)


# DSAIRM
Dynamical Systems Approach to Immune Response Modeling

## Description
DSAIRM is an R package containing a set of simulation modelss (apps) that teach within-host infection dynamics and immune response modeling from a dynamical system perspective. 

All models can be explored through a graphical user interface, no reading or writing code is required. Each app comes with documenation and instructions which teach important concepts of within-host and immune response modeling, and how to use simulation models to understand such concepts. 

It is also possible to go beyond the graphical interface and directly access and modify all simulations to adapt them to your needs.

## Getting Started
The best approach to use this package is to install it, load it, and start the main menu, then you are ready to go. These lines of code typed into the `R` console will get you there:

```
install.packages('DSAIRM')
library('DSAIRM')
dsairmmenu()
```

You can also give the package [a quick try online, without having to install it](https://handelgroup.shinyapps.io/dsairm/). I still recommend you install it like any regular R package should you decide that you want to use it. Note that I only have a free plan for shinyapps.io, where the online version is hosted. If the link fails to work it likely means I reached my maximum monthly allowed capacity. In that case, just go ahead and install the package. It's only 3 commands to get it up and running! 

For a quick introduction to the package, step-by-step instructions on getting started, and more information on the different ways you can use the package [see the tutorial (vignette)](https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html).


## Further information
* A 'companion' package to this one, called Dynamical Systems Approaches for Infectious Disease Epidemiology (DSAIDE), focuses on models for infectious disease epidemiology (the population level). It has the same structure as DSAIRM. [See the DSAIDE site for more information.](https://ahgroup.github.io/DSAIDE/index.html)
* I have solutions (in progress) to most of the 'What to do' tasks for the different apps. If you are an instructor using this package as part of a class, email me if you are interested in having access to these solutions.
* I heard that DSAIRM doesn't seem to work quite right using Docker. This is likely irrelevant for almost all users.

## Acknowledgements 
This R package is developed and maintained by [Andreas Handel](http://handelgroup.uga.edu/). A full list of contributors [can be found here](https://ahgroup.github.io/DSAIRM/authors.html). The development of this package was partially supported by NIH grant U19AI117891. 

