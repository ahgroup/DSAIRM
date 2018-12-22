############################################################
#This is a file for the Basic Bacteria App
#it contains additional information that helps properly process it
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 12/16/2018
############################################################


#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  selectInput("modeltype", "Models to run:",c("continuous time" = 'ode', 'discrete time' = 'discrete', 'both' = 'ode_and_discrete'), selected = 'ode')
) #end taglist

#additional setting elements that are not provided through UI for a given app
#if they are provided by UI (should be an input above), set this to NULL
modeltype = NULL

#name of underlying simulation function
#can be set to NULL, in this case mbmodel Rdata file needs to be provided
#function still needs to be part of the package
simfilename = NULL

#name of underlying mbmodel - if exists
#if not exists, set to NULL
mbmodelfile = 'Basic_Bacteria_model.Rdata'


