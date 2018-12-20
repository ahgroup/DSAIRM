#This is the Shiny App for the main menu of DSAIRM

#this function is the server part of the app
server <- function(input, output, session) {


  #######################################################
  #start code blocks that contain the analyze functionality
  #######################################################

  #get names of all existing apps
  appdir = system.file("DSAIRMapps", package = "DSAIRM")
  appNames = list.dirs(path = appdir, full.names = FALSE, recursive = FALSE)

  currentApp = NULL #global server variable for currently loaded app
  currentsimfile = NULL #global server variable for current simulation function
  currentmbmodel = NULL #global server variable for

  lapply(appNames, function(appName) {
    observeEvent(input[[appName]], {

      #set output to empty
      output$text = NULL
      output$plot = NULL

      currentApp <<- appName #assign currently chosen app to global app variable

      #file that contains additional information for a given app
      #varaiable simfilename in the settings file os the name of the simulation function
      settingfilename = paste0(appdir,'/',currentApp,'/',currentApp,'_settings.R')
      source(settingfilename) #source the file with additional settings to load them

      currentsimfile <<- simfilename

      #other input is a variable in the setting file that contains additional shiny UI elements
      #one wants to display that are not generated automaticall by functions above
      #for instance all non-numeric inputs need to be provided separately
      #NULL if none are required
      output$other <- renderUI({  otherinputs }) #end renderuI

      #browser()

      #if a mbmodel file exists as .Rdata file in the app directory, use that file to create shiny inputs
      mbmodellocation = paste0(appdir,'/',currentApp,'/',mbmodelfile)
      if (file.exists(mbmodellocation))
      {
        load(mbmodellocation)
        DSAIRM::generate_shinyinput(mbmodel = mbmodel, output = output)
      }
      else
      #if no mbmodel Rdata file exists,  extract function inputs and turn them into shiny input elements
      #suing the underlying simulation R function/script
      #this only works for numeric inputs, any others will be removed and need to be set by hand
      {

        #produce Shiny input UI elements for the model.
        #not using the 'standard' UI elements here, instead specifying model specific ones below
        DSAIRM::generate_shinyinput(mbmodel = currentsimfile, output = output)
      }



      #display all extracted inputs on the analyze tab
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
                  uiOutput("vars")
                ),
                column(
                  6,
                  uiOutput("other")
                )
              ),
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
            ), #end layout with side and main panel

            #################################
            #Instructions section at bottom as tabs
            h2('Instructions'),
            #use external function to generate all tabs with instruction content
            do.call(tabsetPanel,generate_documentation(appName))

            ) #end fluidpage for analyze tab
        }) # End renderUI for analyze tab

      #once UI for the model in the analyze tab is created, switch to that tab
      updateNavbarPage(session, "DSAIRM", selected = "Analyze")

      }, priority = 100) #end observeEvent for the analyze tab


      #runs model simulation when 'run simulation' button is pressed
      observeEvent(input$submitBtn, {
        #extract current model settings from UI input elements
        x=isolate(reactiveValuesToList(input)) #get all shiny inputs
        x2 = x[! (names(x) %in% appNames)] #remove inputs that are action buttons for apps
        modelsettings = (x2[! (names(x2) %in% c('submitBtn','Exit','plotscale-selectized','modeltype-selectized','DSAIRM') ) ])
        #if there is no input for replicates, assume reps is 1
        if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1}
        #if no random seed is set, set it to 123. Only important for models that have a stochastic component
        if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}
        #if there is no input for model type, get it from settings file
        if (is.null(modelsettings$modeltype))
        {
          appdir = system.file("DSAIRMapps", package = "DSAIRM")
          settingfilename = paste0(appdir,'/',currentApp,'/',currentApp,'_settings.R')
          if (file.exists(settingfilename))
          {
            source(settingfilename) #source the file with additional settings to load them
            modelsettings$modeltype <- modeltype
          }
        }
        #run model with specified settings
        #run simulation, show a 'running simulation' message
        result <- withProgress(message = 'Running Simulation',
                               detail = "This may take a while", value = 0,
                               {
                                 modeltorun = currentsimfile #use name of function to run by default
                                 if (!is.null(mbmodel)) {runmodel = mbmodel} #if an mbmodel object is present, use and run that instead
                                 run_model(modelsettings = modelsettings, mbmodel = modeltorun)
                               })
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
    stopApp('Exit')
  })

} #ends the server function for the app



#######################################################
#This is the UI for the Main Menu of DSAIRM
#######################################################

ui <- fluidPage(
  includeCSS("../media/dsairm.css"),
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  #add header and title
  div( includeHTML("../media/header.html"), align = "center"),
  p(paste('This is DSAIRM version ',utils::packageVersion("DSAIRM"),' last updated ', utils::packageDescription('DSAIRM')$Date,sep=''), class='infotext'),

  navbarPage(title = "DSAIRM", id = "DSAIRM", selected = 'Menu',
             tabPanel(title = "Menu",
                      p('The Basics', class='mainsectionheader'),

                      fluidRow(
                        column(4,
                               actionButton("BasicBacteria", "Basic Bacterium Model", class="mainbutton")
                        ),
                        column(4,
                               actionButton("Basic_Virus", "Basic Virus Model", class="mainbutton")
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
                               uiOutput('analyzemodel')
                        )
                        #class = "mainmenurow"
                      ) #close fluidRow structure for input
             ) #close "Analyze" tab
  ), #close navbarPage

  div(includeHTML("../media/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
