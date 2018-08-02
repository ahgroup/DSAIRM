############################################################
#This is the Shiny file for the Drug Resistance App
#written by Andreas Handel (ahandel@uga.edu)
#last updated 5/13/2018
############################################################


#the main function with all the functionality for the server
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output)
  {

    result <- reactive({
    input$submitBtn

    # Read all the input values from the UI
    U0 = isolate(input$U0);
    Is0 = isolate(input$Is0);
    Ir0 = isolate(input$Ir0);
    Vs0 = isolate(input$Vs0);
    Vr0 = isolate(input$Vr0);
    tmax = isolate(input$tmax);

    b = 10^isolate(input$b);
    dI = isolate(input$dI);
    e = isolate(input$e);
    m = isolate(input$m);
    p = 10^isolate(input$p);
    c = isolate(input$c);
    f = isolate(input$f);

    rngseed = isolate(input$rngseed);


    nreps = isolate(input$nreps)
    plotscale = isolate(input$plotscale)

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    dat = NULL
    # Call the adaptivetau simulator with the given parameters
    # simulation will be run multiple times based on value of nreps
    for (nn in 1:nreps)
    {
      #add number of rep to seed, otherwise it's exactly the same trajectory each time
      withProgress(message = 'Running Simulation', value = 0, {
        simresult <- simulate_drugresistance(U0 = U0, Is0 = Is0, Ir0 = Ir0, Vs0 = Vs0, Vr0 = Vr0, tmax = tmax, b = b, dI = dI, e = e, m = m, p = p, c = c, f = f, rngseed = rngseed+nn)
    }) #end progress wrapper
      simresult = simresult$ts #extract time series
      colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
      #reformat data to be in the right format for plotting
      datnew = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
      datnew$IDvar = paste(datnew$varnames,nn,sep='') #make a variable for plotting same color lines for each run in ggplot2
      datnew$nreps = nn
      dat = rbind(dat, datnew)
    }

    #code variable names as factor and level them so they show up right in plot
    mylevels = unique(dat$varnames)
    dat$varnames = factor(dat$varnames, levels = mylevels)


    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = dat

    #Meta-information for each plot
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"
    result[[1]]$linesize = 1


    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'

    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'; result[[1]]$xmin = 1e-6}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'; result[[1]]$ymin = 1e-2}

    #the following are for text display for each plot
    result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed for each plot

    return(result)
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
      generate_plots(res) #create plots with a non-reactive function
    }, width = 'auto', height = 'auto'
    ) #finish render-plot statement

    output$text <- renderText({
      input$submitBtn
      res=isolate(result()) #list of all results that are to be turned into plots
      generate_text(res) #create text for display with a non-reactive function
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


#This is the UI for the Stochastic Dynamics App
ui <- fluidPage(
  includeCSS("../../media/dsairm.css"),

  #add header and title

  div( includeHTML("../../media/header.html"), align = "center"),
  h1('Drug Resistance Emergence App', align = "center", style = "background-color:#123c66; color:#fff"),

  #start section to add buttons
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
    column(6,
           #################################
           # Inputs section
           h2('Simulation Settings'),
           fluidRow(
             column(4,
                    numericInput("U0", "initial number of target cells (U0)", min = 100, max = 30000, value = 10000, step = 50)
             ),
             column(4,
                    numericInput("Is0", "initial number of wild-type infected cells  (Is0)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("Ir0", "initial number of resistant infected cells  (Ir0)", min = 0, max = 100, value = 0, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input


           fluidRow(
             column(4,
                    numericInput("Vs0", "initial number of wild-type virus (Vs0)", min = 0, max = 1000, value = 10, step = 1)
             ),
             column(4,
                    numericInput("Vr0", "initial number of resistant virus (Vr0)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("tmax", "Maximum simulation time (tmax)", min = 1, max = 1200, value = 30, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input


           fluidRow(
             column(4,
                    numericInput("b", "Infection rate (10^b)", min = -10, max = 10, value = -4, step = 0.1  )
             ),
             column(4,
                    numericInput("dI", "Cell death rate (dI)", min = 0, max = 10, value = 1, step = 0.1  )
             ),
             column(4,
                    numericInput("p", "Virus production rate (10^p)", min = -10, max = 10, value = 1, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(
             column(4,
                    numericInput("e", "Drug efficacy (e)", min = 0, max = 1, value = 0.0, step = 0.05 )
             ),
             column(4,
                    numericInput("m", "Resistant generation fraction (m)", min = 0, max = 1, value = 0.0, step = 0.01)
             ),
             column(4,
                    numericInput("c", "Virus clearance rate (c)", min = 0, max = 10, value = 4, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input


           fluidRow(
             column(4,
                    numericInput("f", "Resistant fitness cost", min = 0, max = 1, value = 0.0, step = 0.01)
             ),
              column(4,
                         numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
              ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(
             column(4,
                    numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 100, step = 1)
             ),
             column(4,
                    selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"), selected = 'none')
             ),
             align = "center"
           ) #close fluidRow structure for input

    ), #end sidebar column for inputs

    #all the outcomes here
    column(6,

           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot", height = "500px"),
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
  do.call(tabsetPanel,generate_documentation()),
  div(includeHTML("../../media/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)

