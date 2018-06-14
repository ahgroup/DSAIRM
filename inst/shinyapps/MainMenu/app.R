#This is the Shiny App for the main menu

#this function is the server part of the app
server <- function(input, output, session) {


  appNames <- c(unlist(strsplit(DSAIRM::dsairmapps(),', ')),'Exit') #get list of all existing apps

  stopping <- FALSE

  lapply(appNames, function(appName) {
    observeEvent(input[[appName]], {
      stopping <<- TRUE
      stopApp(appName)
    })
  })

  session$onSessionEnded(function(){
    if (!stopping) {
      stopApp('Exit')
    }
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
           actionButton("ModelVariants", "Model variation", class="mainbutton")
    ),
    column(4,
           actionButton("USAnalysis", "Parameter Uncertainty", class="mainbutton")
    ),
    column(4,

         actionButton("BasicVirusStochastic", "Model stochasticity", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input


  p('Model use examples', class='mainsectionheader'),
  fluidRow(
    column(4,
           actionButton("ModelExploration", "Bacterium Model Exploration", class="mainbutton")
    ),
    column(4,
           actionButton("VirusandTx", "Antiviral treatment model", class="mainbutton")
    ),
    column(4,
           actionButton("BasicModelFit", "Basic model fitting", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input


  p('Model fitting topics', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("ConfIntFit", "Confidence Intervals", class="mainbutton")
    ),
    column(6,
           actionButton("ModelComparison", "Model comparison", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input


  p('Further topics', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("PkPdModel", "Pharacokinetics and Pharmacodynamics", class="mainbutton")
    ),
    column(6,
           actionButton("DrugResistance", "Influenza antivirals and resistance", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input

    fluidRow(

    column(12,
           actionButton("Exit", "Exit", class="exitbutton")
    ),
    class = "mainmenurow"
    ), #close fluidRow structure for input

    withTags({
      div(class="header", checked=NA, style = "text-align:left", class="infotext",

  p('This collection of Shiny apps provides you with a "learning by doing" way teach topics of within-host and immune response modeling from a dynamical systems perspective. Ideally, you would use these apps as part of a course on the topic. Alternatively, you should be able to obtain the needed background information by going through the materials listed in the "Further Information" section of the apps.'),
  p('The main way of using the simulations is through this graphical interface. You can also access the simulations directly. This requires a bit of R coding but gives you many more options of things you can try. See the package vignette or the "Further Information" section of the apps for more on that.'),
  p('You should start with the "Basic Bacteria Model" app and read all its instruction tabs since they contain information relevant for all apps.')

       )
    }),

   p('Have fun exploring the models!', class='maintext'),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
