############################################################
#This is the Shiny file for the Virus and PkPd App
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 4/19/2018
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
    g = isolate(input$g)
    n = isolate(input$n)
    dU = isolate(input$dU)
    dI = isolate(input$dI)
    dV = isolate(input$dV)
    C0 = isolate(input$C0)
    dC = isolate(input$dC)
    C50 = isolate(input$C50)
    k = isolate(input$k)
    Emax = isolate(input$Emax)
    txstart = isolate(input$txstart)
    txinterval = isolate(input$txinterval)


    tmax = isolate(input$tmax);
    plotscale = isolate(input$plotscale)

    #save all results to a list for processing plots and text
    listlength = 2; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    withProgress(message = 'Running Simulation', value = 0, {
    simresult <- simulate_pkpdmodel(U0 = U0, I0 = I0, V0 = V0, tmax = tmax, n=n, dU = dU, dI = dI, dV = dV, b = b, p = p, g=g, C0=C0, dC=dC, C50 = C50, k = k, Emax = Emax, txstart = txstart, txinterval = txinterval)
    }) #end progress wrapper

    #data for plots and text
    #make 2 plots, one for U/I/V and one for drug

    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = simresult$ts[,1:4]

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
    result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed for each plot

    #plot for drug
    result[[2]]$dat = simresult$ts[,c(1,5)]
    #Meta-information for each plot
    result[[2]]$plottype = "Lineplot"
    result[[2]]$xlab = "Time"
    result[[2]]$ylab = "Drug Concentration"
    result[[2]]$yscale = "log"
    result[[2]]$legend = "Compartments"
    result[[2]]$xscale = 'identity'
    result[[2]]$yscale = 'identity'
    if (plotscale == 'x' | plotscale == 'both') { result[[2]]$xscale = 'log10'}
    if (plotscale == 'y' | plotscale == 'both') { result[[2]]$yscale = 'log10'}
    result[[2]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text


    #browser()

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
  h1('Virus and drug PkPd App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                           numericInput("n", "uninfected cell birth rate, n", min = 0, max = 100, value = 0, step = 1)
                    ),
                    column(6,
                           numericInput("p", "virus production rate, p (10^p)", min = -5, max = 5, value = 1, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("b", "infection rate, b (10^b)", min = -10, max = 10, value = -5, step = 0.1)
                    ),
                    column(6,
                           numericInput("g", "unit conversion factor, g", min = 0, max = 100, value = 1, step = 1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input



           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("C0", "drug dose given, C0", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("dC", "drug decay rate, dC ", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("C50", "50% drug efficacy level, C50", min = 0, max = 1000, value = 10, step = 1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

            fluidRow(class = 'myrow',
                    column(4,
                           numericInput("Emax", "max drug efficacy, Emax", min = 0, max = 1, value = 1, step = 0.1)
                    ),
                    column(4,
                           numericInput("txstart", "treatment start time, txstart ", min = 0, max = 100, value = 10, step = 1)
                    ),
                    column(4,
                           numericInput("txinterval", "time between doses, txinterval", min = 0, max = 100, value = 2, step = 1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input



              fluidRow(class = 'myrow',
                       column(4,
                              numericInput("k", "drug efficacy increase, k", min = 0, max = 10, value = 1, step = 1)
                       ),

                    column(4,
                           numericInput("tmax", "Maximum simulation time", min = 10, max = 1000, value = 30, step = 10)
                    ),
                    column(4,
                           selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
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
           htmlOutput(outputId = "text")

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
