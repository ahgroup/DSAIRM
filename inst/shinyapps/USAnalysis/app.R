############################################################
#This file connects the Shiny UI to
#the Uncertainty and Sensitivity Analysis code
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 7/16/2018
############################################################


usanalysis <- function(input, output, model)
{

  #produce Shiny input UI elements for the model.
  generate_shinyinput(model, otherinputs = NULL, output)
  #set output to empty
  output$text = NULL
  output$plot = NULL

    find_modelsettings()
  result <- analyze_model(USanalysis)


      #reformat data to be in the right format for plotting
      #the structure for plotting is a nested list
      #the size of the outer list is the number of plots to create
      #each inner list contains all the information for a plot/text, again in list form
      #each list element for a plot contains some meta-information
      #specifically, the following fields: type, legend, xlab, ylab, title
      #each list also contains a data frame in form xvals, yvals, extra variables for stratifications for each plot

      #here, we want either a boxplot or a scatterplot for each parameter and all 3 outcomes
      #that means 8x3 plots
      #we thus need an outer list with 32 elements
      #each element of that list is another list with the above described metadata and the data frame

      #the first list element also needs to contain a variable for the number of columns in the grid plot
      #it's ok if all list elements/plots save that variable but only the information from the first will be used

    #pull the indicator for non-steady state out of the dataframe, process separately
    nosteady = simresult$dat$nosteady
    simresult$dat$nosteady <- NULL

    simdat = simresult$dat

    result <- vector("list", 24) #set up a list structure with as many elements as plots
    #loop over each outer list element corresponding to a plot and fill it with another list
    #of meta-data and data needed to create each plot
    #each parameter-output pair is its own plot, therefore its own list entry
    ct=1; #some counter
    result[[ct]]$ncol = 3 #number of columns for plot, needs to be stored in 1st sub-list element
    for (n in 1:8) #first loop over each parameter
    {
      for (nn in 1:3) #for each parameter, loop over outcomes
      {

      #data frame for each plot
      xvals = simdat[,3+n] #elements 4 to end end are parameters
      xvalname = colnames(simdat)[3+n]
      yvals = simdat[,nn] #first 3 elements are outcomes
      yvalname = colnames(simdat)[nn]
      dat = data.frame(xvals = xvals, yvals = yvals, varnames = yvalname)
      result[[ct]]$dat = dat

      #meta-data for each plot
      result[[ct]]$plottype = plottype
      result[[ct]]$xlab = xvalname
      result[[ct]]$ylab = yvalname
      result[[ct]]$legend = NULL #set to either false or provide the label for legends



      result[[ct]]$xscale = 'identity'
      result[[ct]]$yscale = 'identity'
      if (plotscale == 'x' | plotscale == 'both') { result[[ct]]$xscale = 'log10'}
      if (plotscale == 'y' | plotscale == 'both') { result[[ct]]$yscale = 'log10'}

      #the following are for text display for each plot
      result[[ct]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
      result[[ct]]$finaltext = paste("System might not have reached steady state", sum(nosteady), "times")

      ct = ct + 1
      } #inner loop
    } #outer loop

    #if we look at uncertainty/boxplots, we don't need results stratified by parameter
    #since all the plots and printout contain repeated information, we'll just retain the first 3 ones
    if (plottype == "Boxplot")
    {
      result <- result[c(1:3)]
    }

  return(result) #result returned as list structure
  })


  #functions below take result saved in reactive expression result and produce output
  #to produce figures, the function generate_plot is used
  #function generate_text produces text
  #data needs to be in a specific structure for processing
  #see information for those functions to learn how data needs to look like
  #output (plots, text) is stored in reactive variable 'output'

  output$plot  <- renderPlot({
    input$submitBtn
    res=isolate(result()) #list of all results that are to be turned into plots
    withProgress(message = 'Making Plots', value = 0,
   {
     modelbuilder::generate_plots(res) #create plots with a non-reactive function
   }) #finish progress wrapper
  }, width = 'auto', height = 'auto'
  ) #finish render-plot statement


  output$text <- renderText({
    input$submitBtn
    res=isolate(result()) #list of all results that are to be turned into plots
    modelbuilder::generate_text(res) #create text for display with a non-reactive function
  })


} #ends the 'refresh' shiny server function that runs the simulation and returns output

#main shiny server function
server <- function(input, output, session) {

  # Waits for the Exit Button to be pressed to stop the app and return to main menu
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = NULL)
  })

  # This function is called to refresh the content of the Shiny App
  refresh(input, output)

} #ends the main shiny server function


#This is the UI part of the shiny App
ui <- fluidPage(
  includeCSS("../../media/dsairm.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  tags$head(tags$style(".myrow{vertical-align: bottom;}")),
  div( includeHTML("../../media/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('Uncertainty and Sensitivity Analysis App', align = "center", style = "background-color:#123c66; color:#fff"),

  #section to add buttons
  fluidRow(
    column(6,
           actionButton("submitBtn", "Run Simulation", class="submitbutton")
    ),
    column(6,
           actionButton("exitBtn", "Exit App", class="exitbutton")
    ),
    align = "center"
  ), #end section to add buttons

  tags$hr(),

  ################################
  #Split screen with input on left, output on right
  fluidRow(
    #all the inputs in here
    column(4,
           #################################
           # Inputs section
           h2('Simulation Settings'),
           fluidRow( class = 'myrow',
             column(6,
                    numericInput("B0min", "Initial number of bacteria, B0 (lower bound)", min = 0, max = 1000, value = 1, step = 1)
             ),
             column(6,
                    numericInput("B0max", "Initial number of bacteria, B0 (upper bound)", min = 0, max = 1000, value = 10, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input


             fluidRow( class = 'myrow',
             column(6,
                    numericInput("I0min", "Initial number of immune cells, I0 (lower bound)", min = 0, max = 100, value = 1, step = 1)
             ),
             column(6,
                    numericInput("I0max", "Initial number of immune cells, I0 (upper bound)", min = 0, max = 100, value = 10, step = 1)
             ),
                   align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
               column(6,
                    numericInput("Bmaxmin", "carrying capacity, Bmax (10^Bmax, lower bound)", min = 1, max = 10, value = 4, step = 0.1)
             ),
             column(6,
                    numericInput("Bmaxmax", "carrying capacity, Bmax (10^Bmax, upper bound)", min = 1, max = 10, value = 5, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input

             fluidRow(class = 'myrow',
             column(6,
                    numericInput("dBmin", "bacteria death rate, dB (lower bound)", min = 0, max = 10, value = 1, step = 0.1)
             ),
             column(6,
                    numericInput("dBmax", "bacteria death rate, dB (upper bound)", min = 0, max = 100, value = 2, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("kmin", "immune response kill rate, k (10^k, lower bound)", min = -10, max = 2, value = -8, step = 0.5)
                    ),
                    column(6,
                           numericInput("kmax", "immune response kill rate, k (10^k, upper bound)", min = -10, max = 2, value = -7, step = 0.5)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
           column(6,
                  numericInput("rmin", "immune respone activation rate, r (10^r, lower bound)", min = -10, max = 2, value = -5, step = 0.5)
           ),
           column(6,
                  numericInput("rmax", "immune respone activation rate, r (10^r, upper bound)", min = -10, max = 2, value = -4, step = 0.5)
           ),
           align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
           column(6,
                  numericInput("dImin", "Immune response death rate, dI (lower bound)", min = 0, max = 10, value = 1, step = 0.1)
           ),
           column(6,
                  numericInput("dImax", "Immune response death rate, dI (upper bound)", min = 0, max = 10, value = 2, step = 0.1)
           ),
           align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
           column(6,
                  numericInput("gmean", "Rate of bacteria growth, g (mean)", min = 0, max = 10, value = 5, step = 0.1)
           ),
           column(6,
                  numericInput("gvar", "Rate of bacteria growth, g (variance)", min = 0, max = 10, value = 1, step = 0.1)
           ),
           align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("tmax", "Maximum simulation time", min = 10, max = 1000, value = 200, step = 10)
                    ),
                    column(4,
                           numericInput("samples", "Number of samples to run", min = 10, max = 10000, value = 20, step = 10)
                    ),
                    column(4,
                           numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 100, step = 1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input
           fluidRow(class = 'myrow',
                    column(6,
                           selectInput("plottype", "Plot type for output", c("Boxplot", "Scatterplot"), selected = "Boxplot" )
                    ),
                    column(6,
                           selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
                    ),
                    align = "center"
           ) #close fluidRow structure for input

    ), #end sidebar column for inputs

    #all the outcomes here
    column(8,

           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot", height = "800px"),
           #plotOutput(outputId = "plot"),
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),


           tags$head(tags$style("#warn{color: red;
                                font-style: italic;
                                }")),
           tags$hr()

           ) #end main panel column with outcomes
  ), #end layout with side and main panel

  #################################
  #Instructions section at bottom as tabs
  h2('Instructions'),
  #use external function to generate all tabs with instruction content
  do.call(tabsetPanel,generate_documentation()),
  div(includeHTML("../../media/footer.html"), align="center", style="font-size:small") #footer

) #end fluidpage function, i.e. the UI part of the app

shinyApp(ui = ui, server = server)
