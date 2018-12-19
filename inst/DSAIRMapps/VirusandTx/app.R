############################################################
#This is the Shiny file for the HCV model and treatment App
#inspired by a study on HCV and IFN treatment (Neumann et al. 1998, Science)
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 5/16/2018
############################################################

#the server-side function with the main functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output)
  {

  result <- reactive({
    input$submitBtn

    #Read all the input values from the UI
    U0 = 10^isolate(input$U0);
    I0 = isolate(input$I0);
    V0 = isolate(input$V0);
    b = 10^isolate(input$b)
    p = 10^isolate(input$p)
    n = isolate(input$n)
    dU = isolate(input$dU)
    dI = isolate(input$dI)
    dV = isolate(input$dV)
    g = isolate(input$g)
    f = isolate(input$f)
    e = isolate(input$e)
    txstart = isolate(input$txstart)
    tmax = isolate(input$tmax);
    plotscale = isolate(input$plotscale)
    steadystate = as.logical(as.numeric(isolate(input$steadystate)))

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    withProgress(message = 'Running Simulation', value = 0, {
    simresult <- simulate_virus_tx(U0 = U0, I0 = I0, V0 = V0, tmax = tmax, n=n, dU = dU, dI = dI, dV = dV, b = b, p = p, g = g, f = f, e = e, steadystate = steadystate, txstart = txstart)
    })

    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = simresult$ts

    #Meta-information for each plot
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$yscale = "log"
    result[[1]]$legend = "Compartments"

    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'
    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}

    #the following are for text display for each plot
    result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    result[[1]]$showtext = '' #text can be added here which will be passed through to generate_text and displayed for each plot
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


#This is the UI part of the shiny App
ui <- fluidPage(
  includeCSS("../../media/dsairm.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  tags$head(tags$style(".myrow{vertical-align: bottom;}")),
  div( includeHTML("../../media/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('Antiviral Treatment App', align = "center", style = "background-color:#123c66; color:#fff"),

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
    column(6,
           #################################
           # Inputs section
           h2('Simulation Settings'),
           fluidRow( class = 'myrow',
             column(4,
                    numericInput("U0", "Initial number of uninfected cells, U0 (10^U0)", min = 0, max = 10, value = 5, step = 0.1)
             ),
             column(4,
                    numericInput("I0", "Initial number of infected cells, I0", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("V0", "Initial number of virus, V0", min = 0, max = 100, value = 10, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
             column(4,
                    numericInput("dU", "uninfected cell death rate, dU", min = 0, max = 10, value = 0, step = 0.1)
             ),
             column(4,
                    numericInput("dI", "infected cell death rate, dI", min = 0, max = 10, value = 1, step = 0.1)
             ),
             column(4,
                    numericInput("dV", "virus death rate, dV", min = 0, max = 10, value = 2, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input



           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("n", "uninfected cell birth rate, n", min = 0, max = 1000, value = 0, step = 1)
                    ),
                    column(6,
                           numericInput("p", "virus production rate, (10^p)", min = -5, max = 5, value = 2, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("b", "infection rate, b (10^b)", min = -10, max = 10, value = -6, step = 0.1)
                    ),
                    column(6,
                           numericInput("g", "conversion factor, g", min = 0, max = 10, value = 1, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("f", "cell infection reduction by drug, f", min = 0, max = 1, value = 0, step = 0.01)
                    ),
                    column(4,
                           numericInput("e", "virus production reduction by drug, e", min = 0, max = 1, value = 0, step = 0.01)
                    ),
                    column(4,
                           numericInput("txstart", "Time of treatment start, txstart", min = 0, max = 100, value = 10)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("tmax", "Maximum simulation time", min = 10, max = 1000, value = 100, step = 10)
                    ),
                    column(4,
                           selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
                    ),
                    column(4,
                           selectInput("steadystate", "Start at steady state",c("yes" = 1, 'no' = 0), selected = 0)
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
           #plotOutput(outputId = "plot", height = "500px", click = "plot_click"),
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),
           #last one is meant to display the coordinates of a point clicked on the plot
           #currently not working
           #verbatimTextOutput(outputId = "info"),

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
