############################################################
#This is the Shiny file for the Basic Bacteria App
#written and maintained by Andreas Handel (ahandel@uga.edu)
#last updated 5/23/2017
############################################################

#the server-side function with the main functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output){

  # This reactive takes the input data and sends it over to the simulator
  # Then it will get the results back and return it as the "res" variable
  res <- reactive({
    input$submitBtn


    # Read all the input values from the UI
    B0 = isolate(input$B0);
    I0 = isolate(input$I0);

    b = isolate(input$b);
    Bmax = 10^isolate(input$Bmax);
    dB = isolate(input$dB);
    k = 10^isolate(input$k);
    g = isolate(input$g);
    r = 10^isolate(input$r);
    dI = isolate(input$dI);
    tmax = isolate(input$tmax);

    # Call the ODE solver with the given parameters
    result <- simulate_basicbacteria(B0 = B0, I0 = I0, tmax = tmax, g=g, Bmax=Bmax, dB=dB, k=k, r=r, dI=dI)

    return(list(result)) #this is returned as the res variable
  })

  #function that takes result saved in res and produces output
  #output (plots, text, warnings) is stored in and modifies the global variable 'output'
  generate_simoutput(input,output,res)
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
  includeCSS("../shinystyle.css"),
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
    column(6,
           #################################
           # Inputs section
           h2('Simulation Settings'),
           fluidRow( class = 'myrow',
             column(4,
                    numericInput("B0", "Initial number of bacteria", min = 0, max = 1000, value = 100, step = 50)
             ),
             column(4,
                    numericInput("I0", "Initial number of immune cells", min = 0, max = 100, value = 10, step = 1)
             ),
             column(4,
                    numericInput("tmax", "Maximum simulation time", min = 10, max = 200, value = 100, step = 10)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
             column(4,
                    numericInput("g", "Rate of bacteria growth", min = 0, max = 10, value = 1, step = 0.1)
             ),
             column(4,
                    numericInput("Bmax", "carrying capacity (10^X)", min = 1, max = 10, value = 5, step = 1)
             ),
             column(4,
                    numericInput("dB", "bacteria death rate", min = 0, max = 10, value = 1, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
                    column(4,
                           numericInput("k", "immune response kill rate (10^X)", min = -10, max = 2, value = -4, step = 0.5)
                    ),
                    column(4,
                           numericInput("r", "immune respone activation rate (10^X)", min = -10, max = 2, value = -5, step = 0.5)
                    ),
                    column(4,
                           numericInput("dI", "Immune response death rate", min = 0, max = 10, value = 2, step = 0.1)
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
