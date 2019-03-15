############################################################
#This is a file for the Uncertainty and Sensitivy Analysis App
#it contains additional information that helps properly process it
############################################################

appsettings = list()

#Title of app, to be displayed on top of analyze tab
appsettings$apptitle = "Uncertainty and sensitivity analysis"

#name of underlying simulation function(s) to be used in the app
#must be provided
appsettings$simfunction = 'simulate_usanalysis'

#number of plots to produce for output
appsettings$nplots = 3

#number of columns for a multi-panel plot
appsettings$ncols = 3

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
appsettings$modeltype = '_usanalysis_'

#additional input elements for app that are shown on UI
appsettings$otherinputs =   shiny::tagList(
  shiny::selectInput("plottype", "plot type", c("Boxplot", "Scatterplot"), selected = "Boxplot" ),
  shiny::selectInput("samplepar", "Parameter for scatterplot",c('B' = 'B', "I" = "I", "Bmax" = "Bmax", "dB" = "dB", 'k'='k','r'='r','dI'='dI','g'='g')),
  shiny::selectInput("plotscale", "log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("plotengine", "plot engine",c("ggplot" = "ggplot", "plotly" = "plotly"))
) #end taglist
