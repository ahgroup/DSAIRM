#This is a bit of code and instructions for deployment of the package to shinyappsio
#to deploy, follow these steps:
#1. go into the folder where this file resides
#2. copy the regular app.R file into this folder, add this bit of code on top of it
#3. install the package through CRAN or github if we want to use the github version
#devtools::install_github('ahgroup/DSAIRM')
#4. to deploy, run the following
#run rsconnect::deployApp()

#this line of code needs to be here for shinyappsio deployment
#should not be present for regular package use
library('DSAIRM')

#copy this code on top of the regular app.R file
#app.R file of package starts below