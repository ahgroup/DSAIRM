# DSAIRM
Dynamical Systems Approach to Immune Response Modeling

**The package is under development and not yet on CRAN. Install with devtools from github (see instructions below). Some parts/apps of the package might not be functional yet.**

**Shiny 1.1.0 broke part of the package, specifically the communication between the main menu and each app. For the time being, onle use the 'alternative use' approach described below.**


## Description
This is an R package consisting of a set of Shiny Apps to learn within-host infection dynamics and immune response modeling from a dynamical system perspective. By manipulating the models through the Shiny UI interface and working through the instructions provided within the Shiny UI, you can learn about some important concepts of within-host and immmune response modeling. 

## Installing
1. **Not yet available -** Install the CRAN release in the usual way with `install.packages('DSAIRM')`.
2. The latest development version (potentially buggy) can be installed from github, using the devtools package. If you don't have it, install the devtools package. Then, install DSAIRM through devtools. The following commands should get you up and running:

```r
install.packages('devtools')
library('devtools')
install_github('ahgroup/DSAIRM')
```

## Basic Use
* After install (which you need to do only once), load the package by runing `library('DSAIRM')` 
* You should receive a short greeting. Now you can open the DSAIRM main menu by running `dsairmmenu()`. 
* From the main menu, choose the different apps corresponding to different within-host modeling topics and scenarios.
* Each app contains a description of the model and scenario that is implemented. Each app also contains a list of recommeded tasks to work through in order to learn about a specific topic.
* Once done exploring the apps, close the main menu to exit back to the R console.

## Alternative Use
* If you don't want or can't use the main menu (e.g. because of the shiny issue mentioned above), there is another way to run each app.
* Run the function `dsairmapps()` to get a list of all available apps.
* Run the same function specifying an app (with quotation marks), e.g. `dsairmapps('BasicBacteria')` to run that specific app. Once you close the app, you'll be back at the `R` console, then use the same function to run a different app. 

## Advanced Use
The vignette (currently only partly complete) illustrates ways to interact with and modify the underlying models without the use of the Shiny UI.

## Further information
The vignette provides additional details about the different ways the package can be used.
For feedback, bug reports etc., use the packages' github site https://github.com/ahgroup/DSAIRM
