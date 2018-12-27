############################################################
#This is the Shiny file for the Basic model fitting app
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

    n = isolate(input$n)
    dU = isolate(input$dU)
    dI = isolate(input$dI)
    g = isolate(input$g)

    psim = 10^isolate(input$psim)
    p = 10^isolate(input$p)
    plow = p; phigh = p;
    #if parameter is not being fit, we set low bound equal high bound
    #the parameter is technically still being provided to the fitting routine as being fit
    #but in practice with upper bound = lower bound it is fixed
    if (isolate(input$fitp)) {    plow = 10^isolate(input$plow); phigh = 10^isolate(input$phigh)}

    bsim = 10^isolate(input$bsim)
    b = 10^isolate(input$b)
    blow=b; bhigh=b;
    if (isolate(input$fitb)) { blow = 10^isolate(input$blow); bhigh = 10^isolate(input$bhigh)}

    dVsim = isolate(input$dVsim)
    dV = isolate(input$dV)
    dVlow = dV; dVhigh = dV;
    if (isolate(input$fitdV)) { dVlow = isolate(input$dVlow); dVhigh = isolate(input$dVhigh)}

    iter = isolate(input$iter)
    noise = isolate(input$noise)
    usesimdata = as.logical(isolate(input$usesimdata))
    plotscale = isolate(input$plotscale)
    solvertype = as.numeric(isolate(input$solvertype))

    #save all results to a list for processing plots and text
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results

    #shows a 'running simulation' message
    withProgress(message = 'Running Simulation', value = 0, {
      #result is returned as list
      simresultlist <- simulate_fitbasicmodel(U0 = U0, I0 = I0, V0 = V0, n = n, dU = dU, dI = dI, g = g, p = p, plow = plow, phigh = phigh,  psim = psim,  b = b,  blow = blow, bhigh = bhigh, bsim = bsim,  dV = dV,  dVlow = dVlow, dVhigh = dVhigh,  dVsim = dVsim, usesimdata = usesimdata, iter = iter, noise = noise, solvertype = solvertype)
    })


    simresult = simresultlist$timeseries

    colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
    #reformat data to be in the right format for plotting
    #each plot/text output is a list entry with a data frame in form xvals, yvals, extra variables for stratifications for each plot
    dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
    dat$style = 'line'

    #next, add data that's being fit to data frame
    fitdata  = simresultlist$data
    colnames(fitdata) = c('xvals','yvals')
    fitdata$varnames = 'Data'
    fitdata$yvals = 10^fitdata$yvals #data is in log units, for plotting transform it
    fitdata$style = 'point'
    dat = rbind(dat,fitdata)

    #code variable names as factor and level them so they show up right in plot
    mylevels = unique(dat$varnames)
    dat$varnames = factor(dat$varnames, levels = mylevels)


    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = dat

    #Meta-information for each plot
    result[[1]]$plottype = "Mixedplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"


    #set min and max for scales. If not provided ggplot will auto-set
    result[[1]]$ymin = 0.1
    result[[1]]$ymax = max(simresult)
    result[[1]]$xmin = 1e-12
    result[[1]]$xmax = 9

    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'
    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'; result[[1]]$xmin = 1e-6}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10' }

    #add text for output, this will be used by the generate_text function

    #the following are for text display for each plot
    result[[1]]$maketext = FALSE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text

    #store values for each variable
    aicc = format(simresultlist$AICc, digits =2, nsmall = 2) #mean across simulations (for stochastic models)
    ssr = format(simresultlist$SSR, digits =2, nsmall = 2) #mean across simulations (for stochastic models)
    pfinal = format(log10(simresultlist$bestpars[1]), digits =2, nsmall = 2)
    bfinal = format(log10(simresultlist$bestpars[2]), digits =2, nsmall = 2)
    dVfinal = format(simresultlist$bestpars[3], digits =2, nsmall = 2)

    txt1 <- paste('Best fit values for parameters 10^p / 10^b / dV are ', pfinal, ' / ' ,bfinal,  ' / ' , dVfinal)
    txt2 <- paste('Final SSR is ',ssr)

    result[[1]]$finaltext = paste(txt1,txt2, sep = "<br/>")

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


  #observeEvent(input$submitBtn, {
  #  output$simrun <- renderText({ HTML('Simulation Running') })
  #})

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
  h1('Basic Fitting App', align = "center", style = "background-color:#123c66; color:#fff"),

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
                    numericInput("U0", "Initial number of uninfected cells, U0 (10^U0)", min = 0, max = 10, value = 6, step = 0.1)
             ),
             column(4,
                    numericInput("I0", "Initial number of infected cells, I0", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("V0", "Initial number of virus, V0", min = 0, max = 100, value = 1, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(3,
                           numericInput("n", "uninfected cell production, n", min = 0, max = 100, value = 0, step = 1)
                    ),
                    column(3,
                           numericInput("dU", "uninfected cell death rate, dU", min = 0, max = 100, value = 0, step = 1)
                    ),
                    column(3,
                           numericInput("dI", "infected cell death rate, dI", min = 0, max = 10, value = 2, step = 0.1)
                    ),
                    column(3,
                           numericInput("g", "unit conversion factor, g", min = 0, max = 10, value = 0, step = 0.1)
                    ),

                    align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
                    column(3,
                           numericInput("p", "virus production rate, p (10^p)", min = -7, max = 7, value =  -3, step = 0.1)
                    ),
                    column(3,
                           selectInput("fitp", "fitted",c("Yes" = TRUE, "No" = FALSE), selected = TRUE)
                    ),
                    column(3,
                           numericInput("plow", "lower bound, (10^plow)", min = -10, max = -7, value = -6, step = 0.1)
                    ),
                    column(3,
                           numericInput("phigh", "upper bound, (10^phigh)", min = 7, max = 10, value = 4, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input



           fluidRow(class = 'myrow',
                    column(3,
                           numericInput("b", "infection rate, b (10^b)", min = -8, max = 8, value = -1, step = 0.1)
                    ),
                    column(3,
                           selectInput("fitb", "fitted",c("Yes" = TRUE, "No" = FALSE), selected = TRUE)
                    ),
                    column(3,
                           numericInput("blow", "lower bound, (10^blow)", min = -12, max = -7, value = -8, step = 0.1)
                    ),
                    column(3,
                           numericInput("bhigh", "upper bound, (10^bhigh)", min = -1, max = 10, value = -1, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
                    column(3,
                           numericInput("dV", "virus decay rate, dV", min = 0.1, max = 10, value = 1, step = 0.1)
                    ),
                    column(3,
                           selectInput("fitdV", "fitted",c("Yes" = TRUE, "No" = FALSE), selected = TRUE)
                    ),
                    column(3,
                           numericInput("dVlow", "lower bound, dVlow", min = 0, max = 0.1, value = 0.1, step = 0.01)
                    ),
                    column(3,
                           numericInput("dVhigh", "bound, dVhigh", min = 10, max = 100, value = 10, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(3,
                           numericInput("psim", "value of p for simulated data, (10^psim)", min = -7, max = 6, value = 1, step = 0.1)
                    ),
                    column(3,
                           numericInput("bsim", "value of b for simulated data, (10^bsim)", min = -7, max = 6, value = -4, step = 0.1)
                    ),
                    column(3,
                           numericInput("dVsim", "value of dV for simulated data, dVsim", min = 0.1, max = 10, value = 2, step = 0.1)
                    ),
                    column(3,
                           numericInput("noise", "noise added to simulated data", min = 0, max = 1, value = 0, step = 0.1)
                    ),
                    align = "center"
           ), #close fluidRow structure for input


           fluidRow(class = 'myrow',
                    column(3,
                           selectInput("usesimdata", "Fit to simulated data",c("Yes" = TRUE, "No" = FALSE), selected = FALSE)
                    ),
                    column(3,
                           numericInput("iter", "Number of fitting steps, iter", min = 1, max = 10000, value = 1)
                    ),
                    column(3,
                           selectInput("solvertype", "Solver to use", c(1, 2, 3), selected = 1)
                    ),
                    column(3,
                           selectInput("plotscale", "Log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"), selected = 'y')
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
