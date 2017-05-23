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
    B0 = isolate(input$S0);
    I0 = isolate(input$I0);

    b = isolate(input$b);
    Bmax = isolate(input$Bmax);
    d = isolate(input$d);
    k = isolate(input$k);
    g = isolate(input$g);
    r = isolate(input$r);
    delta = isolate(input$delta);
    tmax = isolate(input$tmax);

    # Call the ODE solver with the given parameters
    result <- simulate_introduction(B0 = B0, I0 = I0, tmax = tmax, g=g, Bmax=Bmax, d=d, k=k, r=r, delta=delta)

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
                    sliderInput("B0", "Initial number of bacteria", min = 500, max = 5000, value = 1000, step = 500)
             ),
             column(4,
                    sliderInput("I0", "Initial number of immune cells", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time", min = 10, max = 1000, value = 300, step = 10)
             ),
             align = "center"
           ), #close fluidRow structure for input

           fluidRow(class = 'myrow',
             column(4,
                    sliderInput("g", "Rate of bacteria growth", min = 0, max = 0.01, value = 0, step = 0.0001, sep ='')
             ),
             column(4,
                    sliderInput("Bmax", "carrying capacity", min = 0, max = 2, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("d", "bacteria death rate", min = 0, max = 2, value = 0.5, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input

           g=1, Bmax=1e6, d=1e-1, k=1e-7, r=1e-3, delta=1

           fluidRow(class = 'myrow',
                    column(4,
                           sliderInput("k", "immune response kill rate", min = 0, max = 0.01, value = 0, step = 0.0001, sep ='')
                    ),
                    column(4,
                           sliderInput("r", "immune respone activation rate", min = 0, max = 2, value = 0.5, step = 0.1)
                    ),
                    column(4,
                           sliderInput("delta", "Immune response death rate", min = 0, max = 2, value = 0.5, step = 0.1)
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
