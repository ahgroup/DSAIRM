############################################################
#This is a file for the Virus and Tx App
#it contains additional information that helps properly process it
############################################################

#Title of app, to be displayed on top of analyze tab
apptitle = "Virus and drug treatment model"

#name of underlying simulation function(s)
simfunction = 'simulate_virusandtx_ode'

#name of underlying mbmodel - if exists
#if not exists, set to NULL
mbmodelfile = NULL

#number of plots to produce for output
nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
modeltype = "_ode_"

#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("plotscale", "log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("steadystate", "Start at steady state",c("yes" = TRUE, 'no' = FALSE), selected = FALSE)
) #end taglist


