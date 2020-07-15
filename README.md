[![Travis-CI build Status](https://travis-ci.org/ahgroup/DSAIRM.svg?branch=master)](https://travis-ci.org/ahgroup/DSAIRM)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ahgroup/DSAIRM?branch=master&svg=true)](https://ci.appveyor.com/project/ahgroup/DSAIRM)
[![Coverage status](https://codecov.io/gh/ahgroup/DSAIRM/branch/master/graph/badge.svg)](https://codecov.io/github/ahgroup/DSAIRM?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/DSAIRM)](https://cran.r-project.org/package=DSAIRM)
[![CRAN checks](https://cranchecks.info/badges/summary/DSAIRM)](https://cran.r-project.org/web/checks/check_results_DSAIRM.html)
[![metacran monthly downloads](http://cranlogs.r-pkg.org/badges/DSAIRM)](https://cran.r-project.org/package=DSAIRM)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/DSAIRM?color=ff69b4)](https://cran.r-project.org/package=DSAIRM)


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

You can also give the package [a quick try online, without having to install it](https://shiny.ovpr.uga.edu/DSAIRM/). I still recommend you install it like any regular R package should you decide that you want to use it. It's only 3 commands to get it up and running! Note that the shiny server on which I host the online version of the package is not regularly monitored and the package is not updated that frequently. If the online app doesn't work, please let me know (through a Github Issue). 

For a quick introduction to the package, step-by-step instructions on getting started, and more information on the different ways you can use the package [see the tutorial (vignette)](https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html).


## Further information
* [I published a paper describing the package](https://doi.org/10.1186/s12865-019-0321-0).  
* The [package tutorial (vignette)](https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html) contains up-to-date and detailed instructions on the different ways the package can be used.
* If you want to take a deeper look at the package, see [this Markdown file](https://github.com/ahgroup/DSAIRM/blob/master/inst/docsfordevelopers/documentation.md) which provides further information on the details of the package structure. (If you plan to develop new apps, or make other substantial contributions, it is best to get in touch with me first.)
* A 'companion' package to this one, called Dynamical Systems Approaches for Infectious Disease Epidemiology (DSAIDE), focuses on models for infectious disease epidemiology (the population level). It has the same structure as DSAIRM. [See the DSAIDE site for more information.](https://ahgroup.github.io/DSAIDE/)
* I heard that DSAIRM doesn't seem to work quite right using Docker. This is likely irrelevant for almost all users.

## Citation and Contributors
If the DSAIRM package does in any way help you with your work such that it warrants citing it in one of your papers, please cite [the DSAIRM publication in BMC Immunology](https://doi.org/10.1186/s12865-019-0321-0). 

This R package is developed and maintained by [Andreas Handel](https://www.andreashandel.com/). A full list of contributors and a Bibtex entry for the citation [can be found here](https://ahgroup.github.io/DSAIRM/authors.html). The development of this package was partially supported by NIH grant U19AI117891. 

