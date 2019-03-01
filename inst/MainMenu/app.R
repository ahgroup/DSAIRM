#This is the Shiny App for the main menu of DSAIRM

#get names of all existing apps
packagename = "DSAIRM"
appdir = system.file("appinformation", package = packagename) #find path to apps
fullappNames = list.files(path = appdir, pattern = "+.settings", full.names = FALSE)
appNames = gsub("_settings.R" ,"",fullappNames)
allsimfctfile = paste0(system.file("simulatorfunctions", package = packagename),"/simulatorfunctions.zip")


#this function is the server part of the app
server <- function(input, output, session)
{

  #to get plot engine be object to always be processed
  output$plotengine <- renderText('ggplot')
  outputOptions(output, "plotengine", suspendWhenHidden = FALSE)

  #######################################################
  #start code that listens to model selection buttons and creates UI for a chosen model
  #######################################################
  lapply(appNames, function(appName)
  {
    observeEvent(input[[appName]],
    {
      currentapp <<- appName #assign currently chosen app to global app variable
      #file name for documentation
      currentdocfilename <- paste0(appdir,'/',currentapp,'_documentation.html')
      settingfilename = paste0(appdir,'/',currentapp,'_settings.R')

      output$ggplot <- NULL
      output$plotly <- NULL
      output$text <- NULL

      #load/source an R settings file that contains additional information for a given app
      #the information is stored in a list called 'appsettings'
      #different models can have different variables
      #all models need the following:
      #variable apptitle - the name of the app
      #variable simfunction - the name of the simulation function(s)
      #variable modeltype - the type of the model to be run or NULL if set by UI
      #additional elements that can be provided:
      #variable otherinputs - contains additional shiny UI elements that are not generated automaticall by functions above
      #for instance all non-numeric inputs need to be provided separately.
      #If not needed, it is NULL
      source(settingfilename) #source the file with additional settings to load them

      #extract function and other inputs and turn them into a taglist
      #this uses the 1st function provided by the settings file and stored in currentsimfct
      #indexing sim function in case there are multiple
      modelinputs <- generate_shinyinput(mbmodel = appsettings$simfunction[1], otherinputs = appsettings$otherinputs, packagename = packagename)

      output$modelinputs <- renderUI({modelinputs})


      #display all inputs and outputs on the analyze tab
      output$analyzemodel <- renderUI({

          tagList(
            tags$div(id = "shinyapptitle", appsettings$apptitle),
            tags$hr(),
            #Split screen with input on left, output on right
            fluidRow(
              column(6,
                h2('Simulation Settings'),
                wellPanel(uiOutput("modelinputs"))
              ), #end sidebar column for inputs
              column(6,
                h2('Simulation Results'),
                conditionalPanel("output.plotengine == 'ggplot'", shiny::plotOutput(outputId = "ggplot") ),
                conditionalPanel("output.plotengine == 'plotly'", plotly::plotlyOutput(outputId = "plotly") ),
                htmlOutput(outputId = "text")
              ) #end column with outcomes
            ), #end fluidrow containing input and output
            #Instructions section at bottom as tabs
            h2('Instructions'),
            #use external function to generate all tabs with instruction content
            withMathJax(do.call(tabsetPanel, generate_documentation(currentdocfilename)))
          ) #end tag list
        }) # End renderUI for analyze tab

      #once UI for the model in the analyze tab is created, switch to that tab
      updateNavbarPage(session, packagename, selected = "Analyze")
    }) #end observeEvent for the analyze tab

  }) #end lapply function surrounding observeEvent to build app

    #######################################################
    #end code that listens to model selection buttons and creates UI for a chosen model
    #######################################################

  ###############
  #Code to reset the model settings
  ###############
  observeEvent(input$reset, {
    modelinputs <- generate_shinyinput(mbmodel = appsettings$simfunction[1], otherinputs = appsettings$otherinputs, packagename = packagename)
    output$modelinputs <- renderUI({modelinputs})
    output$plotly <- NULL
    output$ggplot <- NULL
    output$text <- NULL
  })

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
                     x2 = x1[! (names(x1) %in% appNames)] #remove inputs that are action buttons for apps
                     x3 = (x2[! (names(x2) %in% c('submitBtn','Exit') ) ]) #remove further inputs
                     modelsettings = x3[!grepl("*selectized$", names(x3))] #remove any input with selectized
                     #remove nested list of shiny input tags
                     appsettings$otherinputs <- NULL
                     #add settings information from appsettings list
                     modelsettings = c(appsettings, modelsettings)
                     if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1} #if there is no UI input for replicates, assume reps is 1
                     #if no random seed is set in UI, set it to 123.
                     if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}
                     #run model, process inside run_model function based on settings
                     result <- run_model(modelsettings)
                     #if things worked, result contains a list structure for processing with the plot and text functions
                     #if things failed, result contains a string with an error message
                     if (is.character(result))
                     {
                       output$ggplot <- NULL
                       output$plotly <- NULL
                       output$text <- renderText({ paste("<font color=\"#FF0000\"><b>", result, "</b></font>") })
                     }
                     else #create plots and text, for plots, do either ggplot or plotly
                     {
                        if (modelsettings$plotengine == 'ggplot')
                        {
                          output$plotengine <- renderText('ggplot')
                          output$ggplot  <- shiny::renderPlot({ generate_ggplot(result) })
                        }
                       if (modelsettings$plotengine == 'plotly')
                       {
                         output$plotengine <- renderText('plotly')
                         output$plotly  <- plotly::renderPlotly({ generate_plotly(result) })
                        }
                     #create text from results
                     output$text <- renderText({ generate_text(result) })
                     }
                   }) #end with-progress wrapper
    }, #end the expression being evaluated by observeevent
    #ignoreNULL = TRUE, ignoreInit = TRUE
    ) #end observe-event for analyze model submit button

    #######################################################
    #end code that listens to the 'run simulation' button and runs a model for the specified settings
    #######################################################

  #######################################################
  #code that allows download of all files
  output$modeldownload <- downloadHandler(
    filename <- function() {
      "simulatorfunctions.zip"
    },
    content <- function(file) {
      file.copy(allsimfctfile, file)
    },
    contentType = "application/zip"
  )

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
  includeCSS("packagestyle.css"), #use custom styling
  tags$div(id = "shinyheadertitle", "DSAIRM - Dynamical Systems Approach to Immune Response Modeling"),
  tags$div(id = "shinyheadertext",
    "A collection of Shiny/R Apps to explore and simulate infection and immune response dynamics.",
    br()),
  tags$div(id = "infotext", paste0('This is ', packagename,  ' version ',utils::packageVersion(packagename),' last updated ', utils::packageDescription(packagename)$Date,'.')),
  tags$div(id = "infotext", "Written and maintained by", a("Andreas Handel", href="http://handelgroup.uga.edu", target="_blank"), "with contributions from", a("others.",  href="https://github.com/ahgroup/DSAIRM#contributors", target="_blank")),
  tags$div(id = "infotext", "More information can be found", a("on the package website.",  href="https://ahgroup.github.io/DSAIRM/", target="_blank")),
  navbarPage(title = packagename, id = packagename, selected = 'Menu',
             tabPanel(title = "Menu",
                      tags$div(class='mainsectionheader', 'The Basics'),
                      fluidRow(
                               actionButton("basicbacteria", "Basic bacteria model", class="mainbutton"),
                               actionButton("basicvirus", "Basic virus model", class="mainbutton"),
                               actionButton("virusandir", "Virus and immune response model", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Model use examples'),
                      fluidRow(
                             actionButton("modelexploration", "Bacteria model exploration", class="mainbutton"),
                             actionButton("virusandtx", "Antiviral treatment model", class="mainbutton"),
                             actionButton("basicmodelfit", "Basic model fitting", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'What influences model results'),
                      fluidRow(
                               actionButton("modelvariants", "Model variation", class="mainbutton"),
                               actionButton("usanalysis", "Parameter uncertainty", class="mainbutton"),
                               actionButton("basicvirusstochastic", "Model stochasticity", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Model fitting topics'),
                      fluidRow(
                               actionButton("confintfit", "Confidence intervals", class="mainbutton"),
                               actionButton("modelcomparisonfit", "Model comparison", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Further topics'),
                      fluidRow(
                         actionButton("pkpdmodel", "Pharacokinetics and pharmacodynamics", class="mainbutton"),                             actionButton("drugresistance", "Influenza antivirals and resistance", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input
                      withTags({
                        div(style = "text-align:left", class="infotext",

                            p('This collection of model simulations/apps covers within-host and immune response modeling from a dynamical systems perspective. The software is meant to provide you with a "learning by doing" approach. You will likely learn best and fastest by using this software as part of a course on the topic, taught by a knowledgable instructor who can provide any needed background information and help if you get stuck. Alternatively, you should be able to self-learn and obtain the needed background information by going through the materials listed in the "Further Information" section of the apps.'),
                            p('The main way of using the simulations is through this graphical interface. You can also access the simulations directly. This requires a bit of R coding but gives you many more options of things you can try. See the a("package vignette/tutorial",  href="https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html", target="_blank") or the "Further Information" section of the apps for more on that.'),
                            p('The simulations are ordered in a sequence that makes sense for learning the material, so if you are completely new to this, it is best to go in order (each section top to bottom, within each section left to right). Some simulations also build on earlier ones.')
                        )
                      }), #close withTags function
                      p('Have fun exploring the models!', class='maintext'),
                      fluidRow(
                        downloadButton("modeldownload", "Download R code for all simulations", class="mainbutton"),
                        actionButton("Exit", "Exit", class="exitbutton"),
                        class = "mainmenurow"
                      ) #close fluidRow structure for input

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

  tagList( hr(),
           p('All text and figures are licensed under a ',
           a("Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.", href="http://creativecommons.org/licenses/by-nc-sa/4.0/", target="_blank"),
           'Software/Code is licensed under ',
           a("GPL-3.", href="https://www.gnu.org/licenses/gpl-3.0.en.html" , target="_blank")
           ,
           br(),
           "The development of this package was partially supported by NIH grant U19AI117891.",
           align = "center", style="font-size:small") #end paragraph
  )
) #end fluidpage

shinyApp(ui = ui, server = server)
