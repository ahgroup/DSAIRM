############################################################
#This is the Shiny file for the Uncertainty and Sensitivity Analysis App
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 1/16/2018
############################################################

#the server-side function with the main functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output)
  {

  result <- reactive({
    input$submitBtn

  # Read all the input values from the UI
    B0min = isolate(input$B0min)
    B0max = isolate(input$B0max)
    I0min = isolate(input$I0min)
    I0max = isolate(input$I0max)
    bmin = isolate(input$bmin)
    bmax = isolate(input$bmax)
    Bmaxmin = 10^isolate(input$Bmaxmin);
    Bmaxmax = 10^isolate(input$Bmaxmax);
    dBmin = isolate(input$dBmin)
    dBmax = isolate(input$dBmax)
    kmin = 10^isolate(input$kmin)
    kmax = 10^isolate(input$kmax)
    rmin = 10^isolate(input$rmin)
    rmax = 10^isolate(input$rmax)
    dImin = isolate(input$dImin)
    dImax = isolate(input$dImax)
    gmean = isolate(input$gmean)
    gvar = isolate(input$gvar)
    tmax = isolate(input$tmax);
    samples = isolate(input$samples)
    plottype = isolate(input$plottype)
    plotscale = isolate(input$plotscale)


    sim_result <- simulate_usanalysis(B0min = B0min, B0max = B0max, I0min = I0min, I0max = I0max, Bmaxmin = Bmaxmin, Bmaxmax = Bmaxmax, dBmin = dBmin, dBmax = dBmax, kmin = kmin, kmax = kmax, rmin = rmin, rmax = rmax, dImin = dImin, dImax = dImax, gmean = gmean, gvar = gvar, tmax = tmax, samples = samples)

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

    result <- vector("list", 24) #set up a list structure with as many elements as plots
    #loop over each outer list element corresponding to a plot and fill it with another list
    #of meta-data and data needed to create each plot
    ct=1; #some counter
    result[[ct]]$ncol = 3 #number of columns for plot, needs to be stored in 1st sub-list element
    for (n in 1:8) #first loop over each parameter
    {
      for (nn in 1:3) #for each parameter, loop over outcomes
      {

      #data frame for each plot
      xvals = sim_result[,3+n] #elements 4 to end end are parameters
      xvalname = colnames(sim_result)[3+n]
      yvals = sim_result[,nn] #first 3 elements are outcomes
      yvalname = colnames(sim_result)[nn]
      dat = data.frame(xvals = xvals, yvals = yvals, varnames = yvalname)
      result[[ct]]$dat = dat

      #meta-data for each plot
      result[[ct]]$plottype = plottype
      result[[ct]]$xlab = xvalname
      result[[ct]]$ylab = yvalname
      result[[ct]]$legend = FALSE #set to either false or provide the label for legends


      result[[ct]]$xscale = 'identity'
      result[[ct]]$yscale = 'identity'
      if (plotscale == 'x' | plotscale == 'both') { result[[ct]]$xscale = 'log10'}
      if (plotscale == 'y' | plotscale == 'both') { result[[ct]]$yscale = 'log10'}



      ct = ct + 1
      } #inner loop
    } #outer loop
  return(result) #result returned as list structure
  })

  #functions that take result saved in reactive expression and produces output
  #for structure needed to generate plots and text, see above
  output$plot <- generate_plots(input,output, result)
  output$text <-  generate_text(input,output, result)

} #ends the 'refresh' shiny server function that runs the simulation and returns output

#main shiny server function
server <- function(input, output, session) {

  # Waits for the Exit Button to be pressed to stop the app and return to main menu
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = 0)
  })

  # This function is called to refresh the content of the Shiny App
  refresh(input, output)

  # Event handler to listen for the webpage and see when it closes.
  # Right after the window is closed, it will stop the app server and the main menu will
  # continue asking for inputs.
  session$onSessionEnded(function(){
    stopApp(returnValue = 0)
  })
} #ends the main shiny server function


#This is the UI part of the shiny App
ui <- fluidPage(
  includeCSS("../styles/dsairm.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  tags$head(tags$style(".myrow{vertical-align: bottom;}")),
  div( includeHTML("www/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('Basic Bacteria App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                    numericInput("B0min", "Initial number of bacteria, B0 (lower bound)", min = 0, max = 1000, value = 100, step = 50)
             ),
             column(6,
                    numericInput("B0max", "Initial number of bacteria, B0 (upper bound)", min = 0, max = 1000, value = 100, step = 50)
             ),
             align = "center"
           ), #close fluidRow structure for input


             fluidRow( class = 'myrow',
             column(6,
                    numericInput("I0min", "Initial number of immune cells, I0 (lower bound)", min = 0, max = 100, value = 10, step = 1)
             ),
             column(6,
                    numericInput("I0max", "Initial number of immune cells, I0 (upper bound)", min = 0, max = 100, value = 10, step = 1)
             ),
                   align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
               column(6,
                    numericInput("Bmaxmin", "carrying capacity, Bmax (10^Bmax, lower bound)", min = 1, max = 10, value = 5, step = 1)
             ),
             column(6,
                    numericInput("Bmaxmax", "carrying capacity, Bmax (10^Bmax, upper bound)", min = 1, max = 10, value = 5, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input

             fluidRow(class = 'myrow',
             column(6,
                    numericInput("dBmin", "bacteria death rate, dB (lower bound)", min = 0, max = 10, value = 1, step = 0.1)
             ),
             column(6,
                    numericInput("dBmax", "bacteria death rate, dB (upper bound)", min = 0, max = 10, value = 1, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("kmin", "immune response kill rate, k (10^k, lower bound)", min = -10, max = 2, value = -4, step = 0.5)
                    ),
                    column(6,
                           numericInput("kmax", "immune response kill rate, k (10^k, upper bound)", min = -10, max = 2, value = -4, step = 0.5)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
           column(6,
                  numericInput("rmin", "immune respone activation rate, r (10^r, lower bound)", min = -10, max = 2, value = -4, step = 0.5)
           ),
           column(6,
                  numericInput("rmax", "immune respone activation rate, r (10^r, upper bound)", min = -10, max = 2, value = -4, step = 0.5)
           ),
           align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
           column(6,
                  numericInput("dImin", "Immune response death rate, dI (lower bound)", min = 0, max = 10, value = 2, step = 0.1)
           ),
           column(6,
                  numericInput("dImax", "Immune response death rate, dI (upper bound)", min = 0, max = 10, value = 2, step = 0.1)
           ),
           align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
           column(6,
                  numericInput("gmean", "Rate of bacteria growth, g (mean)", min = 0, max = 10, value = 1.5, step = 0.1)
           ),
           column(6,
                  numericInput("gvar", "Rate of bacteria growth, g (variance)", min = 0, max = 10, value = 1.5, step = 0.1)
           ),
           align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("tmax", "Maximum simulation time", min = 10, max = 200, value = 100, step = 10)
                    ),
                    column(6,
                           numericInput("samples", "Number of samples to run", min = 10, max = 10000, value = 20, step = 10)
                    ),
                    align = "center"
           ), #close fluidRow structure for input
           fluidRow(class = 'myrow',
                    column(6,
                           selectInput("plottype", "Plot type for output", c("Boxplot", "Scatterplot"), selected = "Scatterplot" )
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
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),
           #Placeholder for any possible warning or error messages (this will be shown in red)
           htmlOutput(outputId = "warn"),

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
  do.call(tabsetPanel,generate_instruction_tabs()),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer

) #end fluidpage function, i.e. the UI part of the app

shinyApp(ui = ui, server = server)
