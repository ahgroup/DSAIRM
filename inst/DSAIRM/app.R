#The main use is as R package
#since it is a shiny app, it can also deployed to a shiny server
#The comments below explain how one can deploy to shinyappsio or a shiny server

#note that the UI loads the google analytics bit, which currently is for the UGA server.
#Shouldn't affect deployment as R package.
#Will not apply to loading to shinyappsio (would need to create a new google analytics property)

##############################################
#This is a bit of code and instructions for deployment of the package to a server
##############################################
#to deploy to shinyappsio, follow these steps:
#1. go into the folder where this file (app.R) resides
#2. install the package through CRAN or github if we want to use the github version
#3. #uncomment the library() command below
#4. with the above 'library' statement active, deploy with:
# run rsconnect::deployApp(account = 'epibiouga')
# as suitable, change the account to another one, e.g. handelgroup
# tokens need to be set up for the connection to work
# to set up an account, run setAccountInfo.
# Best way to do is to log into shinyappsio, go to
# 'tokens' and copy the command into the console
#5. comment out the library command again

##############################################
#to deploy to a self-hosted shiny server, steps are similar
#1. install package on server, either CRAN or Github version
#2. uncomment the library() command below
#3. save app.R, copy it and packagestyle.css to the server app folder
#4. comment out the library command again
#5. as needed, update package on server by running: sudo su - -c "R -e \"devtools::install_github('ahgroup/DSAIDE')\""

#library('DSAIRM')

##############################################
#This is the Shiny App for the main menu of DSAIRM
##############################################


##############################################
#Set up some variables, define all as global (the <<- notation)
#name of R package
packagename <<- "DSAIRM"
#find path to apps
appdir <<- system.file("appinformation", package = packagename) #find path to apps
modeldir <<- system.file("mbmodels", package = packagename) #find path to apps
simdir <<- system.file("simulatorfunctions", package = packagename) #find path to apps
#load app table that has all the app information
at <<- read.table(file = paste0(appdir,"/apptable.tsv"), sep = '\t', header = TRUE)
appNames <<- at$appid
#path to simulator function zip file
allsimfctfile <<- paste0(system.file("simulatorfunctions", package = packagename),"/simulatorfunctions.zip")
currentdocfilename <<- NULL


##############################################
#define functions
##############################################

#simple function that creates app buttons for UI
#specify data frame containing app info and the id of the app
make_button <- function(at,appid)
{
  id = which(at$appid == appid)
  actionButton(at$appid[id], paste0(at$apptitle[id]), class="mainbutton")
}


##############################################
#main server function of the app
##############################################
server <- function(input, output, session)
{
  #to get plot engine to be an object that is always be processed
  output$plotengine <- renderText('ggplot')
  outputOptions(output, "plotengine", suspendWhenHidden = FALSE)


  ###############
  #Code to reset the model settings for a given app
  ###############
  observeEvent(input$reset, {
    output$modelinputs <- NULL
    modelinputs <- generate_shinyinput(use_mbmodel = appsettings$use_mbmodel, mbmodel = appsettings$mbmodel,
                                       use_doc = appsettings$use_doc, model_file = appsettings$filepath,
                                       model_function = appsettings$simfunction[1],
                                       otherinputs = appsettings$otherinputs, packagename = packagename)
    output$modelinputs <- renderUI({modelinputs})
    output$plotly <- NULL
    output$ggplot <- NULL
    output$text <- NULL
  })


  #######################################################
  #code that allows download of all simulation files
  #is triggered when user clicks on "download R code" button on main screen
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
  #Button to create floating task list
  observeEvent(input$detachtasks, {
    x = withMathJax(generate_documentation(currentdocfilename))

    x1 = x[[2]][[3]] #task tab
    x2 = x1[[3]]
    x3 = x2[[1]][[3]] #pull out task list without buttons
    output$floattask <- renderUI({
      withMathJax(absolutePanel(x3, id = "taskfloat", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                                width = "30%", height = "auto"))
    })
  })

  #######################################################
  #Button to remove floating task list
  observeEvent(input$destroytasks, {
    output$floattask <- NULL
  })


  #######################################################
  #start code that listens to model selection buttons and creates UI for a chosen model
  #placing it here in hopes it can fix some weird error message on some systems about appName not found
  #not sure if that works might have to do with order in which app is built
  #######################################################
  lapply(appNames, function(appName)
  {
    observeEvent(input[[appName]],
                 {
                   #clear out anything that might be left over from previous app
                   output$ggplot <- NULL
                   output$plotly <- NULL
                   output$text <- NULL
                   output$floattask <- NULL
                   output$analyzemodel <- NULL
                   output$modelinputs <- NULL
                   appsettings <<- NULL
                   modelsettings <<- NULL

                   #each app has settings stored in apptable
                   #read and assign to list called 'appsettings'
                   #store in global variable
                   appsettings <<- as.list(at[which(at$appid == appName),])

                   #a few apps have 2 simulator functions, combine here into vector
                   if (nchar(appsettings$simfunction2) > 1)
                   {
                     appsettings$simfunction <<- c(appsettings$simfunction,appsettings$simfunction2)
                   }

                   #all columns are read in as characters, convert some
                   appsettings$use_mbmodel = as.logical(appsettings$use_mbmodel)
                   appsettings$use_doc = as.logical(appsettings$use_doc)
                   appsettings$nplots = as.numeric(appsettings$nplots)

                   #if an mbmodel should be used, check that it exists and load
                   appsettings$mbmodel <- NULL
                   if (appsettings$use_mbmodel)
                   {
                     appsettings$mbmodel = readRDS(paste0(modeldir,"/",appsettings$mbmodelname) )
                     if (! is.list(appsettings$mbmodel))  {return("mbmodel could not be loaded in app.R")}
                   }

                   #if the doc of a file should be parsed for UI generation, get it here
                   appsettings$filepath <- NULL
                   if (appsettings$use_doc)
                   {
                     filepath = paste0(simdir,'/',appsettings$simfunction[1],'.R')
                     if (! file.exists(filepath))  {return("file for function can't be found")}
                     appsettings$filepath = filepath
                   }

                   #file name for documentation
                   currentdocfilename <<- paste0(appdir,"/",appsettings$docname)

                   #make globally available
                   appsettings <<- appsettings

                   #the information is stored in a list called 'appsettings'
                   #different models can have different variables
                   #all models need the following:
                   #variable appid - ID (short name) of the app
                   #variable apptitle - the name of the app. Used to display.
                   #variable docname - name of documentation file for app
                   #variable modelfigname - name of figure file for app
                   #variable simfunction - the name of the simulation function(s)
                   #variable mbmodelname - if there is an mbmodel available, list its name
                   #variable modeltype - the type of the model to be run. if multiple, i.e. containing "_and_" it is set by UI.

                   #additional elements that can be provided:
                   #variable otherinputs - contains additional shiny UI elements that are not generated automatically by functions above
                   #for instance all non-numeric inputs need to be provided separately.
                   #this is provided as text
                   #If not needed, it is empty ""

                   #extract function and other inputs and turn them into a taglist
                   #this uses the 1st function provided by the settings file
                   #indexing sim function in case there are multiple

                   modelinputs <- generate_shinyinput(use_mbmodel = appsettings$use_mbmodel, mbmodel = appsettings$mbmodel,
                                                      use_doc = appsettings$use_doc, model_file = appsettings$filepath,
                                                      model_function = appsettings$simfunction[1],
                                                      otherinputs = appsettings$otherinputs, packagename = packagename)
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
                 },
                 priority = -100
    ) #end observeEvent for the analyze tab

  }) #end lapply function surrounding observeEvent to build app

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
                     #remove previous plots and text
                     output$ggplot <- NULL
                     output$plotly <- NULL
                     output$text <- NULL

                     #isolate UI inputs to make them non-reactive
                     app_input <- isolate(reactiveValuesToList(input))

                     # this function parses the inputs and app settings
                     # to generate a list of model settings
                     # these specify the settings for which a simulation should be run
                     modelsettings <- generate_modelsettings(app_input, appsettings, appNames)


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

    } #end the expression being evaluated by observeevent
    ) #end observe-event for run model button

    #######################################################
    #end code that listens to the 'run simulation' button and runs a model for the specified settings
    #######################################################


  #######################################################
  #Exit main menu
  observeEvent(input$Exit, {
    stopApp('Exit')
  })

} #ends the server function for the app

#######################################################
#This is the UI for the Main Menu of DSAIRM
#######################################################

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))), #this is only needed for Google analytics when deployed as app to the UGA server. Should not affect R package use.
  tags$head(tags$script('window.onbeforeunload = function() { return "Please use the button on the webpage"; };')), #warning message if user hits browser back button
  includeCSS("packagestyle.css"), #use custom styling
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), #this hides shiny warning messages in console which might be confusing. For debugging/development, one might want to turn this off. Doesn't seem to work?
  tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")), #meant to remove the selector arrows on numeric input boxes: https://community.rstudio.com/t/how-to-remove-numeric-inputs-spin-button-in-r-shiny/13769/3
  tags$div(id = "shinyheadertitle", "DSAIRM - Dynamical Systems Approach to Immune Response Modeling"),
  tags$div(id = "shinyheadertext", "A collection of Shiny/R Apps to explore and simulate infection and immune response dynamics."),
  br(),
  tags$div(id = "infotext", paste0('This is ', packagename,  ' version ',utils::packageVersion(packagename),' last updated ', utils::packageDescription(packagename)$Date,'.')),
  tags$div(id = "infotext", "Written and maintained by", a("Andreas Handel", href="http://handelgroup.uga.edu", target="_blank"), "with contributions from", a("others.",  href="https://github.com/ahgroup/DSAIRM#contributors", target="_blank")),
  tags$div(id = "infotext", "More information can be found", a("on the package website.",  href="https://ahgroup.github.io/DSAIRM/", target="_blank")),
  navbarPage(title = packagename, id = packagename, selected = 'Menu',
             tabPanel(title = "Menu",
                      tags$div(class='mainsectionheader', 'Basic Virus Models'),
                      fluidRow(
                        make_button(at,"basicvirus"),
                        make_button(at,"acutevirusir"),
                        make_button(at,"chronicvirusir"),
                        make_button(at,"virusandir"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input
                      tags$div(id = "shinyheadertext", "IR = Immune Response"),
                      tags$div(class='mainsectionheader', 'Basic Bacteria Models'),
                      fluidRow(
                        make_button(at,"basicbacteria"),
                        make_button(at,"extendedbacteria"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Model Exploration Examples'),
                      fluidRow(
                        make_button(at,"basicbacteriaexploration"),
                        make_button(at,"basicvirusexploration"),
                        make_button(at,"modelvariants"),
                        make_button(at,"virusandtx"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Uncertainty and Sampling'),
                      fluidRow(
                        make_button(at,"usanalysis"),
                        make_button(at,"basicvirusstochastic"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Model Fitting Topics'),
                      fluidRow(
                        make_button(at,"fitbasicmodel"),
                        make_button(at,"fitconfint"),
                        make_button(at,"fitmodelcomparison"),
                        make_button(at,"fitfludrug"),
                        make_button(at,"fitbacteria"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Further Topics'),
                      fluidRow(
                        make_button(at,"pkpdmodel"),
                        make_button(at,"drugresistance"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input
                      withTags({
                        div(style = "text-align:left", class="bottomtext",
                            tags$div(id = "bottomtext", 'This collection of model simulations/apps covers within-host and immune response modeling from a dynamical systems perspective. The software is meant to provide you with a "learning by doing" approach. You will likely learn best and fastest by using this software as part of a course on the topic, taught by a knowledgable instructor who can provide any needed background information and help if you get stuck. Alternatively, you should be able to self-learn and obtain the needed background information by going through the materials listed in the "Further Information" section of the apps.'),
                            tags$div(id = "bottomtext", "The main way of using the simulations is through this graphical interface. You can also access the simulations directly. This requires a bit of R coding but gives you many more options of things you can try. See the", a("package vignette/tutorial",  href="https://ahgroup.github.io/DSAIRM/articles/DSAIRM.html", target="_blank"), " or the Further Information section of the apps for more on that."),
                            tags$div(id = "bottomtext", 'The simulations are ordered in a sequence that makes sense for learning the material, so if you are completely new to this, it is best to go in order (each section top to bottom, within each section left to right). Some simulations also build on earlier ones.')
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
                               uiOutput('analyzemodel'),
                               uiOutput('floattask')
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
           "This project was/is partially supported by NIH grants U19AI117891, U01AI150747, R25AI147391 and R25GM089694.",
           align = "center", style="font-size:small") #end paragraph
  )
) #end fluidpage

shinyApp(ui = ui, server = server)
