############################################################
#This is a file for the Basic Virus App
#it contains additional UI settings to be displayed that are not automatically generated
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 12/16/2018
############################################################




#additional input elements for  model
otherinputs =   tagList(
    selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
  ) #end taglist




#the server-side function with the main functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
# refresh <- function(input, output)
#   {
#
#   result <- reactive({
#     input$submitBtn
#
#
#     #save all results to a list for processing plots and text
#     listlength = 1; #here we do all simulations in the same figure
#     result = vector("list", listlength) #create empty list of right size for results
#     #shows a 'running simulation' message
#     withProgress(message = 'Running Simulation', value = 0,
#     {
#       simresult <- simulate_basicvirus(U0 = U0, I0 = I0, V0 = V0, tmax = tmax, n=n, dU = dU, dI = dI, dV = dV, b = b, p = p, g = g)
#     })
#
#     #data for plots and text
#     #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
#     #each variable listed in varnames will also be processed to produce text
#     result[[1]]$dat = simresult$ts
#
#     #Meta-information for each plot
#     result[[1]]$plottype = "Lineplot"
#     result[[1]]$xlab = "Time"
#     result[[1]]$ylab = "Numbers"
#     result[[1]]$legend = "Compartments"
#
#     result[[1]]$xscale = 'identity'
#     result[[1]]$yscale = 'identity'
#     if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
#     if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}
#
#     #the following are for text display for each plot
#     result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
#     result[[1]]$showtext = '' #text can be added here which will be passed through to generate_text and displayed for each plot
#     result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed for each plot
#
#   return(result)
#   })
#
#   #functions below take result saved in reactive expression result and produce output
#   #to produce figures, the function generate_plot is used
#   #function generate_text produces text
#   #data needs to be in a specific structure for processing
#   #see information for those functions to learn how data needs to look like
#   #output (plots, text) is stored in reactive variable 'output'
#
#   output$plot  <- renderPlot({
#     input$submitBtn
#     res=isolate(result()) #list of all results that are to be turned into plots
#     generate_plots(res) #create plots with a non-reactive function
#   }, width = 'auto', height = 'auto'
#   ) #finish render-plot statement
#
#   output$text <- renderText({
#     input$submitBtn
#     res=isolate(result()) #list of all results that are to be turned into plots
#     generate_text(res) #create text for display with a non-reactive function
#   })
#


