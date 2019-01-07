############################################################
#This is a file for the Basic Bacteria App
#it contains additional information that helps properly process it
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 12/16/2018
############################################################

#additional input elements for app that are shown on UI
otherinputs = list(
  shiny::selectInput("plotscale", "Log-scale for plot ",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("modeltype", "Models to run ",c("continuous time" = '_ode_', 'discrete time' = '_discrete_', 'both' = '_ode_and_discrete_'), selected = '_ode_')
) #end list

#additional setting elements that are not provided through UI for a given app
#if model type is provided as input above, it should be set to NULL here
modeltype = NULL

#name of underlying simulation function(s) to be used in the app
#must be provided
simfunction = c('simulate_basicbacteria_ode','simulate_basicbacteria_discrete')


#name of underlying mbmodel - if exists
#if it exists, it will be used to build UI input elements
#if not exists, set to NULL
mbmodelfile = 'BasicBacteria_model.Rdata'
#mbmodelfile = NULL

#number of plots to produce for output
nplots = 1
