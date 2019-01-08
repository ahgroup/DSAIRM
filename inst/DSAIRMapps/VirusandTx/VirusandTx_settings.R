############################################################
#This is a file for the Virus and Tx App
#it contains additional information that helps properly process it
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 12/21/2018
############################################################

#additional setting elements that are not provided through UI for a given app
modeltype = "_ode_"

#name of underlying simulation function
simfunction = 'simulate_virusandtx_ode'

#name of underlying mbmodel - if exists
#if not exists, set to NULL
mbmodelfile = NULL

#number of plots to produce for output
nplots = 1

#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("steadystate", "Start at steady state",c("yes" = TRUE, 'no' = FALSE), selected = FALSE)
) #end taglist
