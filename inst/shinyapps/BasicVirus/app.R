############################################################
#This is the Shiny file for the Basic Virus App
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
    b = 10^isolate(input$b)
    p = 10^isolate(input$p)
    n = isolate(input$n)
    g = isolate(input$g)
    dU = isolate(input$dU)
    dI = isolate(input$dI)
    dV = isolate(input$dV)
    tmax = isolate(input$tmax);
    plotscale = isolate(input$plotscale)

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results
    simresult <- simulate_basicvirus(U0 = U0, I0 = I0, V0 = V0, tmax = tmax, n=n, dU = dU, dI = dI, dV = dV, b = b, p = p, g = g)
    colnames(simresult) = c('xvals','U','I','V')
    #reformat data to be in the right format for plotting
    #each plot/text output is a list entry with a data frame in form xvals, yvals, extra variables for stratifications for each plot
    dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")


    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = dat

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

  return(result)
  })

  #function that takes result saved in reactive expression called result and produces output
  #to produce figures, the function generate_plots needs the number of panels to produce
  #the resulting plot is returned in potential multi-panel ggplot/cowplot structure
  #output (plots, text) is stored in variable 'output'
  output$plot <- generate_plots(input, output, result)
  output$text <- generate_text(input, output, result)

  #for a single plot it is possible to include interactivity to click on plot and get value
  #not working, might be because it returns a line plot? not sure
  #output$info <- renderPrint({ nearPoints(result()[[1]]$dat, input$plot_click, xvar='xvals',yvar='yvals')  })

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
  h1('Basic Virus App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                    numericInput("dV", "virus death rate, dV", min = 0, max = 10, value = 4, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input



           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("n", "uninfected cell birth rate, n", min = 0, max = 100, value = 0, step = 1)
                    ),
                    column(4,
                           numericInput("p", "virus production rate, p (10^p)", min = -5, max = 5, value = 2, step = 0.1)
                    ),
                    column(4,
                           numericInput("b", "infection rate, b (10^b)", min = -10, max = 10, value = -6, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("g", "Unit conversion factor, g", min = 0, max = 100, value = 1, step = 1)
                    ),
                    column(4,
                           numericInput("tmax", "Maximum simulation time", min = 10, max = 1000, value = 100, step = 10)
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
  do.call(tabsetPanel,generate_instruction_tabs()),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer

) #end fluidpage function, i.e. the UI part of the app

shinyApp(ui = ui, server = server)
