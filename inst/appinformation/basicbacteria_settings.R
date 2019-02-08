############################################################
#This is a file for the Basic Bacteria App
#it contains additional information that helps properly process it
############################################################

#Title of app, to be displayed on top of analyze tab
apptitle = "Basic Bacteria Model"

#name of underlying simulation function(s) to be used in the app
#must be provided
simfunction = c('simulate_basicbacteria_ode','simulate_basicbacteria_discrete')

#name of underlying mbmodel - if exists
#if it exists, it will be used to build UI input elements
#if not exists, set to NULL
mbmodelfile = NULL

#number of plots to produce for output
nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
modeltype = NULL

#additional input elements for app that are shown on UI
otherinputs = list(
  shiny::selectInput("plotscale", "log-scale for plot ",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("modeltype", "Models to run ",c("ODE" = '_ode_', 'discrete' = '_discrete_', 'both' = '_ode_and_discrete_'), selected = '_ode_')
) #end list


