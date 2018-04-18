############################################################
#This is the Shiny file for the Stochastic version of the Basic Virus App
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 3/16/2018
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
    tmax = isolate(input$tmax);
    plotscale = isolate(input$plotscale)
    models = as.numeric(isolate(input$models))
    rngseed = isolate(input$rngseed);
    nreps = isolate(input$nreps)

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    if (models == 1 | models == 3) #deterministic model
    {
      result_ode <- simulate_basicvirus(U0 = U0, I0 = I0, V0 = V0, tmax = tmax, n=n, dU = dU, dI = dI, dV = dV, b = b, p = p, g = 1)
      colnames(result_ode) = c('xvals','Udet','Idet','Vdet')
      dat_ode = tidyr::gather(as.data.frame(result_ode), -xvals, value = "yvals", key = "varnames")
      dat_ode$IDvar = paste(dat_ode$varnames,sep='')
    }

    # stochastic model
    if (models == 2 | models == 3)
    {

      datall = NULL
      # Call the adaptivetau simulator with the given parameters
      # simulation will be run multiple times based on value of nreps
      for (nn in 1:nreps)
      {
        #add number of rep to seed, otherwise it's exactly the same trajectory each time

        simresult <- simulate_stochasticvirus(U0 = U0, I0 = I0, V0 = V0, tmax = 100, n=n, dU=dU, b = b, dI = dI, p = p, dV = dV, rngseed = rngseed+nn)

        colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
        #reformat data to be in the right format for plotting
        dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
        dat$IDvar = paste(dat$varnames,nn,sep='') #trying to make a variable for plotting same color lines for each run in ggplot2. doesn't work yet.
        datall = rbind(datall,dat)
      }
    } #end stochastic model



    #depending on if user wants only 1 model or both
    if (models == 1) { dat = dat_ode}
    if (models == 2) { dat = datall}
    if (models == 3) { dat <- dplyr::full_join(dat_ode,datall)  }

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



    #set min and max for scales. If not provided ggplot will auto-set
    result[[1]]$ymin = 0
    result[[1]]$ymax = max(result[[1]]$dat$yvals)
    result[[1]]$xmin = 0
    result[[1]]$xmax = tmax

    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'; result[[1]]$xmin = 1e-6}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'; result[[1]]$ymin = 1e-2}




  return(result)
  })

  #function that takes result saved in reactive expression called res and produces output
  #to produce figures, the function generate_simoutput needs the number of panels to produce
  #the resulting plot is returned in potential multi-panel ggplot/ggpubr structure
  #inputs needed are: number of plots to create; for each plot, the type of plot to create; for each plot, X-axis, y-axis and aesthetics/stratifications.
  #for time-series, x-axis is time, y-axis is value, and aesthetics/stratification is the name of the variable (S/I/V/U, etc.) and/or the number of replicates for a given variable
  #output (plots, text) is stored in variable 'output'
  output$plot <- generate_plots(input, output, result)
  output$text <- generate_text(input, output, result)


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
  h1('Stochastic Virus App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                           numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 100, step = 1)
                    ),
                    column(4,
                           selectInput("models", "Models to run:",c("deterministic" = 1, 'stochastic' = 2, 'both' = 3), selected = '1')
                    ),
                    align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
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
