############################################################
#This is a file for the Model Exploration App
#it contains additional information that helps properly process it
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 12/26/2018
############################################################

#additional input elements for app that are shown on UI
otherinputs =   shiny::tagList(
  shiny::selectInput("plottype", "Plot type for output", c("Boxplot", "Scatterplot"), selected = "Boxplot" ),
  shiny::selectInput("plotscale", "Log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
) #end taglist

#additional setting elements that are not provided through UI for a given app
#if model type is provided as input above, it should be set to NULL here
modeltype = 'usanalysis'

#name of underlying simulation function
#can be set to NULL, in this case mbmodel Rdata file needs to be provided
#simulator function still needs to be part of the package
simfunction = 'simulate_usanalysis'

#name of underlying mbmodel - if exists
#if not exists, set to NULL
mbmodelfile = NULL



#number of plots to produce for output
nplots = 1
