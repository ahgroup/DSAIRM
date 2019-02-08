############################################################
#This is a file for the Basic Model Fit App
#it contains additional information that helps properly process it
############################################################

#Title of app, to be displayed on top of analyze tab
apptitle = "Basic Fitting App"

#name of underlying simulation function(s) to be used in the app
#must be provided
simfunction = 'simulate_basicmodel_fit'

#name of underlying mbmodel - if exists
#if it exists, it will be used to build UI input elements
#if not exists, set to NULL
mbmodelfile = NULL

#number of plots to produce for output
nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
modeltype = '_fit_'

#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("usesimdata", "Fit to simulated data",c("Yes" = TRUE, "No" = FALSE), selected = FALSE),
  shiny::selectInput("plotscale", "log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
) #end taglist
