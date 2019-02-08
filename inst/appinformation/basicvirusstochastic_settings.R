############################################################
#This is a file for the Stochastic Basic Virus App
#it contains additional information that helps properly process it
############################################################

#Title of app, to be displayed on top of analyze tab
apptitle = "Stochastic Virus Model"

#name of underlying simulation function(s) to be used in the app
#must be provided
simfunction = c('simulate_basicvirus_stochastic', 'simulate_basicvirus_ode')

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
  shiny::selectInput("modeltype", "Models to run ",c("ODE" = '_ode_', 'stochastic' = '_stochastic_', 'both' = '_ode_and_stochastic_'), selected = '_ode_'),
  shiny::numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
) #end list
