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
2. The latest development version (potentially buggy) can be installed from github, using the devtools package. If you don't have it, install the devtools package. Then, install DSAIRM through devtools. The following commands will get you up and running:

```r
install.packages('devtools')
devtools::install_github('ahgroup/DSAIRM')
```

## Basic Use
After install (which you need to do only once), load the package by runing `library('DSAIRM')`. You should receive a short greeting. Now you can open the DSAIRM main menu by running `dsairmmenu()`. From the main menu, choose the different apps corresponding to different modeling topics and scenarios. Each app contains information on the model and topic that are covered. Each app also contains a list of recommeded tasks to work through in order to learn about a specific topic. Once done exploring, close the main menu to exit back to the `R` console.


## Advanced Use
You can call the underlying simulation functions directly from the `R` console. You can also access all functions and modify them to your own needs. To find the folder on your computer where the simulator functions are stored, use the following command:

```r
system.file("simulatorfunctions", package = "DSAIRM")
```

See [the package vignette](https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html) for more details on the different ways to use the package. 

## Contributing to the package
The package is on GitHub and you can use the usual GitHub process to contribute updated, bug fixes, etc. If you don't know how to do that or don't have the time, you can also file an issue on GitHub and let me know what should be changed. 

The package is built in a way that makes it (hopefully) easy for others to contribute new simulations/apps. To that end, the package contains [this Markdown file, documentation.md,](https://github.com/ahgroup/DSAIRM/blob/master/inst/docsfordevelopers/documentation.md) which provides further information on the details of the package structure. If you plan to develop new apps, or add other substantial updates, it's best to get in touch with me first via email or GitHub.


## Further information
* The [package vignette](https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html) provides details about the different ways the package can be used. I highly recommend going through it. 
* The [documentation.md](https://github.com/ahgroup/DSAIRM/blob/master/inst/docsfordevelopers/documentation.md) file described above contains more information about the package structure.
* For feedback, bug reports, feature requests, etc., file a [GitHub issue](https://github.com/ahgroup/DSAIRM/issues).
* A 'companion' package to this one, called Dynamical Systems Approaches for Infectious Disease Epidemiology (DSAIDE), focuses on models for infectious disease epidemiology (the population level). It has the same structure as DSAIRM. [See the DSAIDE site for more information.](https://ahgroup.github.io/DSAIDE/index.html)


## Contributors
This R package is developed and maintained by [Andreas Handel](http://handelgroup.uga.edu/). The following individuals have made contributions to this package: Spencer Hall, Sina Solaimanpour, Henok Woldu.

Hello
