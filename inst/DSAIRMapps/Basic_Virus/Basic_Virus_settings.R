############################################################
#This is a file for the Basic Virus App
#it contains additional information that helps properly process it
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 12/16/2018
############################################################


#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
  ) #end taglist

#additional setting elements that are not provided through UI for a given app
modeltype = "ode"

#name of underlying simulation function
#can be set to NULL, in this case mbmodel Rdata file needs to be provided
#function still needs to be part of the package
simfunction = 'simulate_Basic_Virus_model_ode'
#simfunction = NULL

#name of underlying mbmodel - if exists will be used to build UI and create
#model run call instead of sim function
#sim function still needs to exist
#right now, idea is to not use mbmodel for any app
#if not exists, set to NULL
#mbmodelfile = 'Basic_Virus_model.Rdata'
mbmodelfile = NULL
