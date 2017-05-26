# DSAIRM
Dynamical Systems Approach to Immune Response Modeling

## Description
This is an R package consisting of a set of Shiny Apps to teach within-host infection dynamics and immune response modeling from a dynamical system perspective.
By manipulating the models through the Shiny UI interface and working through the instructions provided within the Shiny UI, you can learn about some important concepts of within-host and immmune response modeling. 

## Installing
1. Install the CRAN release in the usual way with install.packages()
2. The latest development version (potentially buggy) can be installed from github, using the devtools package function via: install_github("ahgroup/DSAIRM")

## Basic Use
After loading the package, run `dsairmmenu()` to start the main menu. 
From there choose the different apps corresponding to different within-host modeling topics and scenarios.
Each app contains a description of the model and scenario that is implemented.
Each app also contains a list of recommeded tasks to work through in order to learn about a specific topic.
Once done exploring the apps, close the main menu to exit back to the R console.

## Advanced Use
The vignette illustrates ways to interact with and modify the underlying models without the use of the Shiny GUI.

## Further information
The vignette provides additional details about the different ways the package can be used.
For feedback, bug reports etc., use the packages' github site https://github.com/ahgroup/DSAIRM
