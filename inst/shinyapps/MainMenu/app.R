#This is the Shiny App for the main menu

#this function is the server part of the app
server <- function(input, output, session) {

  observeEvent(input$BasicBacteria, {
    input$BasicBacteria
    print ("Running BasicBacteria...")
    stopApp(returnValue = 'A')
  })

  observeEvent(input$CharacteristicsofID, {
    input$CharacteristicsofID
    print ("Running CharacteristicsofID...")
    stopApp(returnValue = 'B')
  })

  observeEvent(input$IDPatterns, {
    input$IDPatterns
    print ("Running IDPatterns...")
    stopApp(returnValue = 'C')
  })

  observeEvent(input$ReproductiveNumber, {
    input$ReproductiveNumber
    print ("Running ReproductiveNumber...")
    stopApp(returnValue = 'D')
  })

  observeEvent(input$DirectTransmission, {
    input$DirectTransmission
    print ("Running DirectTransmission...")
    stopApp(returnValue = 'E')
  })



  observeEvent(input$Exit, {
    input$Exit
    print ("Exiting")
    stopApp(returnValue = 'X')
  })

  session$onSessionEnded(function(){
    stopApp(returnValue = 'X')
  })

}


#This is the UI for the Main Menu of DSAIRM
ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  p(paste('This is DSAIRM version ',utils::packageVersion("DSAIDE"),' last updated ', utils::packageDescription('DSAIRM')$Date,sep=''), class='infotext'),

  #specify name of App below, will show up in title
  h1('DSAIRM - Main Menu', align = "center", style = "background-color:#123c66; color:#fff"),

  fluidRow(
    column(4,
           actionButton("BasicBacteria", "Basic Bacteria Models", class="mainbutton")
    ),
    column(4,
           actionButton("CharacteristicsofID", "Characteristics of ID", class="mainbutton")
    ),
    column(4,
           actionButton("IDPatterns", "ID Patterns", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("DirectTransmission", "Direct Transmission", class="mainbutton")
    ),
    column(4,
         actionButton("EnvironmentalTransmission", "Environmental Transmission", class="mainbutton")
    ),
    column(4,
           actionButton("VectorTransmission", "Vector Transmission", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("ReproductiveNumber", "Reproductive Number", class="mainbutton")
    ),
    column(4,
           actionButton("IDControl", "ID Control", class="mainbutton")
    ),
    column(4,
           actionButton("HostHeterogeneity", "Host Heterogeneity", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("StochasticDynamics", "Stochastic Dynamics", class="mainbutton")
    ),
    column(4,
           actionButton("EvolutionaryDynamics", "Evolutionary Dynamics", class="mainbutton")
    ),
    column(4,
            ""
    ),
    class = "mainmenurow"
  ),
  fluidRow(

    column(12,
           actionButton("Exit", "Exit", class="exitbutton")
    ),
    class = "mainmenurow"
    ), #close fluidRow structure for input

  p('This collection of Shiny apps is meant to illustrate and teach various topics of infectious disease epidemiology from a dynamical systems perspective. The simulation apps are meant to provide "learning by doing/exploring". Depending on you background knowledge, it is possible that exploring the apps on their own might not suffice to learn the material. Ideally, you would use these apps as part of a course on the topic. Alternatively, you can try to obtain the required background information to fill any gaps in understanding by reading the material listed in the "Further Information" section of the apps.',class='infotext', align="left"),
  p('The main way of interacting with the ID simulations is through the graphical interface. For information on more advanced uses of the simulations provided here, see the package vignette or the "Further Information" section of the apps.',class='infotext', align="left"),
    p('Have fun exploring the infectious disease models!', class='maintext'),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
