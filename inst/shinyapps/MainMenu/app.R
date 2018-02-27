#This is the Shiny App for the main menu

#this function is the server part of the app
server <- function(input, output, session) {

  observeEvent(input$BasicBacteria, {
    input$BasicBacteria
    stopApp(returnValue = 'BasicBacteria')
  })

  observeEvent(input$BasicVirus, {
    input$BasicVirus
    stopApp(returnValue = 'BasicVirus')
  })

  observeEvent(input$VirusandIR, {
    input$VirusandIR
    stopApp(returnValue = 'VirusandIR')
  })

  observeEvent(input$ModelExploration, {
    input$ModelExploration
    stopApp(returnValue = 'ModelExploration')
  })

  observeEvent(input$USAnalysis, {
    input$USAnalysis
    stopApp(returnValue = 'USAnalysis')
  })
  observeEvent(input$StochasticModel, {
    input$StochasticModel
    stopApp(returnValue = 'StochasticModel')
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
  includeCSS("../styles/dsairm.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  p(paste('This is DSAIRM version ',utils::packageVersion("DSAIRM"),' last updated ', utils::packageDescription('DSAIRM')$Date,sep=''), class='infotext'),

  #specify name of App below, will show up in title
  h1('DSAIRM - Main Menu', align = "center", style = "background-color:#123c66; color:#fff"),

  p('The Basics', class='mainsectionheader'),

  fluidRow(
    column(4,
           actionButton("BasicBacteria", "Basic Bacterium Model", class="mainbutton")
    ),
    column(4,
           actionButton("BasicVirus", "Basic Virus Model", class="mainbutton")
    ),
    column(4,
           actionButton("VirusandIR", "Virus and Immune Response Model", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input

  p('What influences model results', class='mainsectionheader'),
  fluidRow(
    column(4,
           actionButton("ModelVariants", "Model formulation", class="mainbutton")
    ),
    column(4,
           actionButton("USAnalysis", "Parameter Uncertainty", class="mainbutton")
    ),
    column(4,
           actionButton("StochasticModel", "Model Stochasticity", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input

  p('Using models to explore and predict', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("ModelExploration", "Bacteria Model Exploration", class="mainbutton")
    ),
    column(6,
           actionButton("InfluenzaResistance", "Influenza antivirals and drug resistance", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input


  p('Using models with data', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("HCVmodel", "Comparing an HCV model to data", class="mainbutton")
    ),
    column(6,
           actionButton("InfluenzaFitting", "Fitting an Influenza model to data", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  p('Further Topics', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("TBExperiments", "PKPD", class="mainbutton")
    ),
    column(6,
           actionButton("InfluenzaResistance", "Discrete time models and noise", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input


    fluidRow(

    column(12,
           actionButton("Exit", "Exit", class="exitbutton")
    ),
    class = "mainmenurow"
    ), #close fluidRow structure for input

  p('This collection of Shiny apps is meant to illustrate and teach topics of within-host and immune response modeling from a dynamical systems perspective. The simulation apps are meant to provide "learning by doing/exploring". Depending on you background knowledge, it is possible that exploring the apps on their own might not suffice to learn the material. Ideally, you would use these apps as part of a course on the topic. Alternatively, you can try to obtain the required background information to fill any gaps in understanding by reading the material listed in the "Further Information" section of the apps.',class='infotext', align="left"),
  p('The main way of interacting with the models is through the graphical interface. For information on more advanced uses of the simulations provided here, see the package vignette or the "Further Information" section of the apps.',class='infotext', align="left"),
    p('Have fun exploring the models!', class='maintext'),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
