############################################################
#This is a file for the Stochastic Basic Virus App
#it contains additional information that helps properly process it
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 1/1/2019
############################################################

#additional input elements for app that are shown on UI
otherinputs = list(
  shiny::selectInput("plotscale", "Log-scale for plot ",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("modeltype", "Models to run ",c("ODE" = '_ode_', 'stochastic' = '_stochastic_', 'both' = '_ode_and_stochastic_'), selected = '_ode_'),
  shiny::numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
) #end list

#additional setting elements that are not provided through UI for a given app
modeltype = NULL

#name of underlying simulation function(s) to be used in the app
#must be provided
simfunction = c('simulate_basicvirus_stochastic', 'simulate_basicvirus_ode')

#name of underlying mbmodel - if exists will be used to build UI and create
#model run call instead of sim function
#sim function still needs to exist
#right now, idea is to not use mbmodel for any app
#if not exists, set to NULL
#mbmodelfile = 'BasicVirus_model.Rdata'
mbmodelfile = NULL


#number of plots to produce for output
nplots = 1
