############################################################
#This is the Shiny file for the Model Variants app
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
    F0 = isolate(input$F0);
    A0 = isolate(input$A0);

    n = isolate(input$n)
    dU = isolate(input$dU)
    b = 10^isolate(input$b)
    dI = isolate(input$dI)
    dV = isolate(input$dV)
    p = 10^isolate(input$p)

    pF = isolate(input$pF)
    dF = isolate(input$dF)

    f1 = isolate(input$f1)
    f2 = isolate(input$f2)
    f3 = isolate(input$f3)
    Fmax = 10^isolate(input$Fmax)
    sV = 10^isolate(input$sV)

    k1 = isolate(input$k1)
    k2 = isolate(input$k2)
    k3 = isolate(input$k3)

    a1 = isolate(input$a1)
    a2 = isolate(input$a2)
    a3 = isolate(input$a3)
    hV = 10^isolate(input$hV)

    k4 = isolate(input$k4)
    k5 = isolate(input$k5)
    k6 = isolate(input$k6)
    sA = 10^isolate(input$sA)
    dA = isolate(input$dA)

    tmax = isolate(input$tmax);
    plotscale = isolate(input$plotscale)

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    withProgress(message = 'Running Simulation', value = 0, {
    simresult <- simulate_modelvariants(U0 = U0, I0 = I0, V0 = V0, F0=F0, A0=A0, tmax = tmax, n=n, dU=dU, dI=dI,dV=dV,b=b,p=p,pF=pF,dF=dF,f1=f1,f2=f2,f3=f3,Fmax=Fmax,sV=sV,k1=k1,k2=k2,k3=k3,a1=a1,a2=a2,a3=a3,hV=hV,k4=k4,k5=k5,k6=k6,sA=sA,dA=dA)
    }) #end progress wrapper

    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = simresult$ts

    #Meta-information for each plot
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legendtitle = "Compartments"

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
  h1('Model Variants App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                           numericInput("F0", "Initial level of innate response, F0", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("A0", "initial level of adaptive response, A0", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("b", "infection rate, b (10^b)", min = -10, max = 10, value = -5, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("n", "rate of uninfected cell production, n", min = 0, max = 1000, value = 0, step = 1)
                    ),

                    column(4,
                           numericInput("dU", "rate of uninfected cell death, dU", min = 0, max = 100, value = 0, step = 1)
                    ),
                    column(4,
                           numericInput("dA", "rate of adaptive immune response decay, dA", min = 0, max = 100, value = 0.1, step = 1)
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
                           numericInput("p", "virus production rate, p (10^p)", min = -5, max = 5, value = 2, step = 0.1)
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
                           numericInput("Fmax", "max level of innate response, Fmax (10^Fmax)", min = 0, max = 6, value = 3, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("f1", "innate growth version 1, f1", min = 0, max = 100, value = 0.1, step = 0.001)
                    ),
                    column(4,
                           numericInput("f2", "innate growth version 2, f2", min = 0, max = 100, value = 0, step = 0.001)
                    ),
                    column(4,
                           numericInput("f3", "innate growth version 3, f3", min = 0, max = 100, value = 0, step = 0.001)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("k1", "innate action 1, k1", min = 0, max = 1000, value = 0.001, step = 0.001)
                    ),
                    column(4,
                           numericInput("k2", "innate action 2, k2", min = 0, max = 1000, value = 0, step = 0.001)
                    ),
                    column(4,
                           numericInput("k3", "innate action 3, k3", min = 0, max = 1000, value = 0, step = 0.001)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("a1", "adaptive growth version 1, a1", min = 0, max = 1000, value = 0.001, step = 0.001)
                    ),
                    column(4,
                           numericInput("a2", "adaptive growth version 2, a2", min = 0, max = 1000, value = 0, step = 0.001)
                    ),
                    column(4,
                           numericInput("a3", "adaptive growth version 3, a3", min = 0, max = 1000, value = 0, step = 0.001)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("k4", "adaptive action 1, k4", min = 0, max = 1000, value = 0.1, step = 0.001)
                    ),
                    column(4,
                           numericInput("k5", "adaptive action 2, k5", min = 0, max = 1000, value = 0, step = 0.001)
                    ),
                    column(4,
                           numericInput("k6", "adaptive action 3, k6", min = 0, max = 1000, value = 0, step = 0.001)
                    ),
                    align = "center"
           ), #close fluidRow structure for input



           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("sV", "innate growth saturation for version 2 and 3, sV (10^sV)", min = -10, max = 10, value = -10, step = 0.1)
                    ),
                    column(4,
                           numericInput("hV", "adaptive growth saturation for version 2 and 3, hV (10^hV)", min = -10, max = 10, value = -10, step = 0.1)
                    ),
                    column(4,
                           numericInput("sA", "saturation of adaptive action, version 2, sA (10^sA)", min = -10, max = 10, value = -10, step = 0.1)
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
