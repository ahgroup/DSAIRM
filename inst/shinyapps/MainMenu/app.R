#This is the Shiny App for the main menu of DSAIRM

#this function is the server part of the app
server <- function(input, output, session) {


  #######################################################
  #start code blocks that contain the analyze functionality
  #######################################################

  appNames <- c(unlist(strsplit(DSAIRM::dsairmapps(),', ')),'Exit') #get list of all existing apps

  stopping <- FALSE

  lapply(appNames, function(appName) {
    observeEvent(input[[appName]], {

      apppath = system.file("shinyapps", package = "DSAIRM")

      filename = paste0(apppath,'/',appName,'/',appName,'_model.Rdata')
      #browser()

      #model <- reactive({
      #  load(filename)
      #  })
      model <- load(filename)

      modelbuilder::generate_shinyinput(model, output) #produce output elements for each variables, parameters, etc.
      output$analyzemodel <- renderUI({
        fluidPage(
          #section to add buttons
          fluidRow(column(
            12,
            actionButton("submitBtn", "Run Simulation", class = "submitbutton")
          ),
          align = "center"),
          #end section to add buttons
          tags$hr(),
          ################################
          #Split screen with input on left, output on right
          fluidRow(
            #all the inputs in here
            column(
              6,
              h2('Simulation Settings'),
              column(
                6,
                uiOutput("vars"),
                uiOutput("time")
              ),
              column(
                6,
                uiOutput("pars"),
                uiOutput("other")

              )),
            #end sidebar column for inputs

            #all the outcomes here
            column(
              6,
              #################################
              #Start with results on top
              h2('Simulation Results'),
              plotOutput(outputId = "plot", height = "500px"),
              # PLaceholder for results of type text
              htmlOutput(outputId = "text"),
              tags$hr()
            ) #end main panel column with outcomes
          ) #end layout with side and main panel
        ) #end fluidpage for analyze tab
      }) # End renderUI for analyze tab
      #make the UI for the model, saves those into the output elements
    }, priority = 100) #end observe for UI construction



    #runs model simulation when 'run simulation' button is pressed
    observeEvent(input$submitBtn, {
      result <- modelbuilder::analyze_model(modelsettings, mbmodel = model() )
      #create plot from results
      output$plot  <- renderPlot({
        generate_plots(result)
      }, width = 'auto', height = 'auto')
      #create text from results
      output$text <- renderText({
        generate_text(result)     #create text for display with a non-reactive function
      })
    }) #end observe-event for analyze model submit button
  }) #end lapply function surrounding observeEvent

  #######################################################
  #end code blocks that contain the analyze functionality
  #######################################################


  observeEvent(input$Exit, {
    stopping <<- TRUE
    stopApp('Exit')
  })

  session$onSessionEnded(function() {
    if (!stopping) {
      stopApp('Exit')
    }
  })

} #ends the server function for the app



#######################################################
#This is the UI for the Main Menu of DSAIRM
#######################################################

ui <- fluidPage(
  includeCSS("../../media/dsairm.css"),
  #add header and title
  div( includeHTML("../../media/header.html"), align = "center"),
  p(paste('This is DSAIRM version ',utils::packageVersion("DSAIRM"),' last updated ', utils::packageDescription('DSAIRM')$Date,sep=''), class='infotext'),

  navbarPage(title = "DSAIRM",
             tabPanel(title = "Menu",
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
                      }), #close withTags function
                      p('Have fun exploring the models!', class='maintext')

             ), #close "Menu" tabPanel tab

             tabPanel("Analyze",
                      fluidRow(
                        column(12,
                               #actionButton("analyzemodel", "Analyze current model", class = "mainbutton")
                               uiOutput('analyzemodel')
                        ),
                        class = "mainmenurow"
                      ) #close fluidRow structure for input
             ) #close "Analyze" tab
  ), #close navbarPage


  div(includeHTML("../../media/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
