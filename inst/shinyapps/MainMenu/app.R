#This is the Shiny App for the main menu

#this function is the server part of the app
server <- function(input, output, session) {

  observeEvent(input$BasicBacteria, {
    input$BasicBacteria
    stopApp(returnValue = 'A')
  })

  observeEvent(input$BasicVirus, {
    input$BasicVirus
    stopApp(returnValue = 'B')
  })

  observeEvent(input$BasicHIV, {
    input$BasicHIV
    stopApp(returnValue = 'C')
  })

  observeEvent(input$ComplexHIV, {
    input$ComplexHIV
    stopApp(returnValue = 'D')
  })

  observeEvent(input$HCVandIFN, {
    input$HCVandIFN
    stopApp(returnValue = 'E')
  })
  observeEvent(input$HCVandPKPD, {
    input$HCVandPKPD
    stopApp(returnValue = 'F')
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

  fluidRow(
    column(4,
           actionButton("BasicBacteria", "Basic Bacterium Model", class="mainbutton")
    ),
    column(4,
           actionButton("BasicVirus", "Basic Virus Model", class="mainbutton")
    ),
    column(4,
           actionButton("BasicHIV", "Simple HIV Model", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("ComplexHIV", "Complex HIV Model", class="mainbutton")
    ),
    column(4,
         actionButton("HCVandIFN", "HCV and IFN treatment", class="mainbutton")
    ),
    column(4,
           actionButton("HCVandPKPD", "HCV with PK/PD", class="mainbutton")
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
