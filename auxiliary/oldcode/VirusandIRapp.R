############################################################
#This is the Shiny file for the Virus and Immune response model
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 2/16/2018
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
    T0 = isolate(input$T0);
    B0 = isolate(input$B0);
    A0 = isolate(input$A0);
    Fmax = 10^isolate(input$Fmax)
    n = isolate(input$n)
    dU = isolate(input$dU)
    dI = isolate(input$dI)
    dV = isolate(input$dV)
    b = 10^isolate(input$b)
    p = 10^isolate(input$p)
    sF = 10^isolate(input$sF)
    kA = 10^isolate(input$kA)
    kT = 10^isolate(input$kT)
    pF = isolate(input$pF)
    dF = isolate(input$dF)
    gF = isolate(input$gF)
    hV = 10^isolate(input$hV)
    hF = 10^isolate(input$hF)
    gB = isolate(input$gB)
    gT = isolate(input$gT)
    rT = isolate(input$rT)
    rA = isolate(input$rA)
    dA = isolate(input$dA)

    tmax = isolate(input$tmax);
    plotscale = isolate(input$plotscale)

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    #shows a 'running simulation' message
    withProgress(message = 'Running Simulation', value = 0,
    {
      simresult <- simulate_virusandir(U0 = U0, I0 = I0, V0 = V0, T0=T0, B0=B0, A0=A0, tmax = tmax, n=n, dU = dU, dI = dI, dV = dV, b = b, p = p,sF=sF,kA=kA,kT=kT,pF=pF,dF=dF,gF=gF,Fmax=Fmax,hV=hV,hF=hF,gB=gB,rT=rT,gT=gT,rA=rA,dA=dA)
    })


    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = simresult$ts

    #Meta-information for each plot
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"

    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'
    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'; result[[1]]$xmin = 1e-6}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'; result[[1]]$ymin = 1e-6}


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
  h1('Virus and Immune Response App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                           numericInput("T0", "Initial number of T-cells, T0", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("B0", "Initial number of B-cells, B0", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("A0", "initial number of antibodies, A0", min = 0, max = 100, value = 1, step = 1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

            fluidRow(class = 'myrow',
                     column(4,
                            numericInput("n", "uninfected cell birth rate, n", min = 0, max = 100, value = 0, step = 1)
                     ),
                     column(4,
                            numericInput("dU", "uninfected cell death rate, dU", min = 0, max = 10, value = 0, step = 0.1)
                     ),
                     column(4,
                            numericInput("b", "infection rate, b (10^b)", min = -10, max = 10, value = -5, step = 0.1)
                     ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("dI", "infected cell death rate, dI", min = 0, max = 10, value = 1, step = 0.1)
                    ),
                    column(4,
                           numericInput("dV", "virus death rate, dV", min = 0, max = 10, value = 4, step = 0.1)
                    ),
                    column(4,
                           numericInput("p", "virus production rate, p (10^p)", min = -5, max = 5, value = 3, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("pF", "innate production rate, pF", min = 0, max = 100, value = 1, step = 0.1)
                    ),
                    column(4,
                           numericInput("dF", "innate removal rate, dF", min = 0, max = 100, value = 1, step = 0.1)
                    ),
                    column(4,
                           numericInput("gF", "innate growth rate, gF", min = 0, max = 10, value = 1, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("hV", "innate growth saturation, hV (10^hV)", min = -10, max = 10, value = -6, step = 0.1)
                    ),
                    column(4,
                           numericInput("Fmax", "max level of innate response, Fmax (10^Fmax)", min = 0, max = 6, value = 3, step = 0.1)
                    ),
                    column(4,
                           numericInput("sF", "virus reduction strength by innate, sF (10^sF)", min = -12, max = 6, value = -1.5, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("kA", "virus removal by antibodies, kA (10^kA)", min = -10, max = 10, value = -5, step = 0.1)
                    ),
                    column(4,
                           numericInput("kT", "infected cell killing by T cells, kT (10^kT)", min = -20, max = 10, value = -5, step = 0.1)
                    ),
                    column(4,
                           numericInput("hF", "B-cell growth saturation constant, hF (10^hF)", min = -10, max = 10, value = -5, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("gB", "B-cell growth rate, gB", min = 0, max = 10, value = 1, step = 0.1)
                    ),
                    column(4,
                           numericInput("rT", "T-cell expansion rate, rT", min = 0, max = 10, value = 0.5, step = 0.1)
                    ),
                    column(4,
                           numericInput("gT", "T-cell induction rate, gT", min = 0, max = 10, value = 0.5, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("rA", "antibody production rate, rA", min = 0, max = 1000, value = 10, step = 1)
                    ),
                    column(6,
                           numericInput("dA", "antibody removal rate, dA", min = 0, max = 10, value = 0.2, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(6,
                           numericInput("tmax", "Maximum simulation time", min = 1, max = 100, value = 20, step = 1)
                    ),
                    column(6,
                           selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"), selected = 'y')
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

) #end fluidpage function, i.e. the UI part of the app

shinyApp(ui = ui, server = server)
