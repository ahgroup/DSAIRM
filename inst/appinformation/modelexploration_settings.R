############################################################
#This is a file for the Model Exploration App
#it contains additional information that helps properly process it
############################################################

appsettings = list()

#Title of app, to be displayed on top of analyze tab
appsettings$apptitle = "Bacteria model exploration"

#name of underlying simulation function(s) to be used in the app
#must be provided
appsettings$simfunction = 'simulate_modelexploration'

#number of plots to produce for output
appsettings$nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
appsettings$modeltype = '_modelexploration_'

#additional input elements for app that are shown on UI
appsettings$otherinputs =   shiny::tagList(
  shiny::selectInput("samplepar", "Parameter to vary",c("g" = "g", 'Bmax' = 'Bmax', 'dB' = 'dB', 'k'='k','r'='r','dI'='dI')),
  shiny::selectInput("pardist", "Spacing of parameter values", c('linear' = 'lin', 'logarithmic' = 'log')),
  shiny::selectInput("plotscale", "log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("plotengine", "plot engine",c("ggplot" = "ggplot", "plotly" = "plotly"))
) #end taglist
