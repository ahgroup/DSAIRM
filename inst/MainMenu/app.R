#This is the Shiny App for the main menu of DSAIRM

#this function is the server part of the app
server <- function(input, output, session) {


  #######################################################
  #start code blocks that contain the analyze functionality
  #######################################################

  #get names of all existing apps
  appdir = system.file("DSAIRMapps", package = "DSAIRM") #find path to apps
  appNames = list.dirs(path = appdir, full.names = FALSE, recursive = FALSE)

  currentApp = NULL #global server variable for currently loaded app
  currentmbmodel <<- NULL #global server variable for mbmodel structure
  currentsimfct <<- NULL #global server variable for current simulation function
  currentmodelnplots <<- NULL #global server variable for number of plots
  currentmbmodelfile <<- NULL #global server variable for mbmodel file name
  currentmodeltype <<- NULL #global server variable for model type to run
  currentotherinputs <<-  NULL

  #######################################################
  #start code that listens to model selection buttons and creates UI for a chosen model
  #######################################################

  lapply(appNames, function(appName) {
    observeEvent(input[[appName]], {

      currentApp <<- appName #assign currently chosen app to global app variable

      output$plot <- NULL
      output$text <- NULL

      #load/source an R settings file that contains additional information for a given app
      #variable simfilename in the settings file is the name of the simulation function or NULL
      #variable modeltype in the settings file is the type of the model to be run or NULL
      #variable mbmoddelfile is the name of the mbmodel Rdata file or NULL
      #variable otherinputs contains additional shiny UI elements
      #one wants to display that are not generated automaticall by functions above
      #for instance all non-numeric inputs need to be provided separately. If not needed, it is NULL
      settingfilename = paste0(appdir,'/',currentApp,'/',currentApp,'_settings.R')
      source(settingfilename) #source the file with additional settings to load them
      currentsimfct <<- simfunction
      currentmodelnplots <<- nplots
      currentmbmodelfile <<- mbmodelfile
      currentmodeltype <<- modeltype
      currentotherinputs <<-  otherinputs

      #produce Shiny input UI elements for the model
      #if a mbmodel file is not NULL, use
      #.Rdata file in the app directory, use that file to create shiny inputs
      if (!is.null(currentmbmodelfile))
      {
        mbmodellocation = paste0(appdir,'/',currentApp,'/',currentmbmodelfile)
        load(mbmodellocation) #this loads an mbmodel
        currentmbmodel <<- mbmodel
        DSAIRM::generate_shinyinput(mbmodel = currentmbmodel, otherinputs = currentotherinputs, output = output)
      }
      else
      #if no mbmodel Rdata file exists,  extract function inputs and turn them into shiny input elements
      #this uses the 1st function provided by the settings file and stored in crrentsimfct
      #this only works for numeric inputs, any others will be removed and need to be
      #added to shiny UI using the settings file
      {
        currentmbmodel <<- NULL
        DSAIRM::generate_shinyinput(mbmodel = currentsimfct[1], otherinputs = currentotherinputs, output = output) #indexing sim function in case there are multiple
      }


      #display all extracted inputs on the analyze tab
      output$analyzemodel <- renderUI({
            tagList(

              fluidRow(
                column(12,
                       actionButton("submitBtn", "Run Simulation", class = "submitbutton")
                ),
                class = "mainmenurow"
              ), #close fluidRow structure for input
            tags$hr(),
            ################################
            #Split screen with input on left, output on right
            fluidRow(
              #all the inputs in here
              column(
                6,
                h2('Simulation Settings'),
                wellPanel(
                    uiOutput("modelinputs")
                )
              ), #end sidebar column for inputs

              #all the outcomes here
              column(
                6,
                #################################
                #Start with results on top
                h2('Simulation Results'),
                plotOutput(outputId = "plot", height = "500px"),
                # PLaceholder for results of type text
                htmlOutput(outputId = "text")
              ) #end column with outcomes
            ), #end fluidrow containing input and output

            #################################
            #Instructions section at bottom as tabs
            h2('Instructions') ,
            #use external function to generate all tabs with instruction content
            withMathJax(do.call(tabsetPanel, generate_documentation(currentApp)))
          ) #end tag list
          }) # End renderUI for analyze tab
      #once UI for the model in the analyze tab is created, switch to that tab
      updateNavbarPage(session, "DSAIRM", selected = "Analyze")


      }, priority = 100) #end observeEvent for the analyze tab

    #######################################################
    #end code that listens to model selection buttons and creates UI for a chosen model
    #######################################################


    #######################################################
    #start code that listens to the 'run simulation' button and runs a model for the specified settings
    #######################################################
      observeEvent(input$submitBtn, {


        #run model with specified settings
        #run simulation, show a 'running simulation' message
        withProgress(message = 'Running Simulation',
            detail = "This may take a while", value = 0,
            {
              #extract current model settings from UI input elements
              x1=isolate(reactiveValuesToList(input)) #get all shiny inputs
              #x1=as.list(isolate(input)) #get all shiny inputs
              x2 = x1[! (names(x1) %in% appNames)] #remove inputs that are action buttons for apps
              x3 = (x2[! (names(x2) %in% c('submitBtn','Exit','DSAIRM') ) ]) #remove further inputs
              modelsettings = x3[!grepl("*selectized$", names(x3))] #remove any input with selectized
              if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1} #if there is no UI input for replicates, assume reps is 1
              #if no random seed is set in UI, set it to 123. Only important for models that have a stochastic component
              if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}
              #if there is a supplied model type from the settings file, use that one
              #note that input for model type might be still 'floating around' if a previous model was loaded
              #not clear how to get rid of old shiny input variables from previously loaded models
              if (!is.null(currentmodeltype)) { modelsettings$modeltype <- currentmodeltype}
              modelsettings$nplots <- currentmodelnplots
              result <- run_model(modelsettings = modelsettings, modelfunction  = currentsimfct)

              #create plot from results
              output$plot  <- renderPlot({
                generate_plots(result)
              }, width = 'auto', height = 'auto')
              #create text from results
              output$text <- renderText({
                generate_text(result) })
            }) #end with-progress wrapper
      }, #end the expression being evaluated by observeevent
      #once = TRUE
      ) #end observe-event for analyze model submit button

    #######################################################
    #end code that listens to the 'run simulation' button and runs a model for the specified settings
    #######################################################

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
  includeCSS("../media/dsairm.css"), #use custom styling
  div( includeHTML("../media/header.html"), align = "center"),   #add header and title
  p(paste('This is DSAIRM version ',utils::packageVersion("DSAIRM"),' last updated ', utils::packageDescription('DSAIRM')$Date,sep=''), class='infotext'),

  navbarPage(title = "DSAIRM", id = "DSAIRM", selected = 'Menu',
             tabPanel(title = "Menu",
                      p('The Basics', class='mainsectionheader'),

                      fluidRow(
                        column(4,
                               actionButton("BasicBacteria", "Basic bacteria model", class="mainbutton")
                        ),
                        column(4,
                               actionButton("BasicVirus", "Basic virus model", class="mainbutton")
                        ),
                        column(4,
                               actionButton("VirusandIR", "Virus and immune response model", class="mainbutton")
                        ),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      p('Model use examples', class='mainsectionheader'),
                      fluidRow(
                        column(4,
                               actionButton("ModelExploration", "Bacteria model exploration", class="mainbutton")
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
                               actionButton("USAnalysis", "Parameter uncertainty", class="mainbutton")
                        ),
                        column(4,

                               actionButton("BasicVirusStochastic", "Model stochasticity", class="mainbutton")
                        ),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input


                      p('Model fitting topics', class='mainsectionheader'),
                      fluidRow(
                        column(6,
                               actionButton("ConfIntFit", "Confidence intervals", class="mainbutton")
                        ),
                        column(6,
                               actionButton("ModelComparisonFit", "Model comparison", class="mainbutton")
                        ),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      p('Further topics', class='mainsectionheader'),
                      fluidRow(
                        column(6,
                               actionButton("PkPdModel", "Pharacokinetics and pharmacodynamics", class="mainbutton")
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

                            p('This collection of model simulations/apps covers within-host and immune response modeling from a dynamical systems perspective. The software is meant to provide you with a "learning by doing" approach. You will likely learn best and fastest by using this software as part of a course on the topic, taught by a knowledgable instructor who can provide any needed background information and help if you get stuck. Alternatively, you should be able to self-learn and obtain the needed background information by going through the materials listed in the "Further Information" section of the apps.'),
                            p('The main way of using the simulations is through this graphical interface. You can also access the simulations directly. This requires a bit of R coding but gives you many more options of things you can try. See the package vignette or the "Further Information" section of the apps for more on that.'),
                            p('The simulations are ordered in a sequence that makes sense for learning the material, so it is best o go in order (each section top to bottom, within each section left to right). Some simulations also build on earlier ones.')

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
