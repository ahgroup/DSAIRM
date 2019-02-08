############################################################
#This is a file for the Uncertainty and Sensitivy Analysis App
#it contains additional information that helps properly process it
############################################################

#Title of app, to be displayed on top of analyze tab
apptitle = "Uncertainty and Sensitivy Analysis"

#name of underlying simulation function(s) to be used in the app
#must be provided
simfunction = 'simulate_usanalysis'

#name of underlying mbmodel - if exists
#if it exists, it will be used to build UI input elements
#if not exists, set to NULL
mbmodelfile = NULL

#number of plots to produce for output
nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
modeltype = 'usanalysis'

#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("plottype", "Plot type for output", c("Boxplot", "Scatterplot"), selected = "Boxplot" ),
  shiny::selectInput("plotscale", "log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
) #end taglist
