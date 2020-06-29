#' @title A helper function that takes a model and generates shiny UI elements
#'
#' @description This function generates shiny UI inputs for a supplied model.
#' This is a helper function called by the shiny app.
#' @param use_mbmodel TRUE/FALSE if mbmodel list should be used to generate UI
#' @param mbmodel a valid mbmodel object
#' @param use_doc TRUE/FALSE if doc of a model file should be parsed to make UI
#' @param model_file name/path to function file for parsing doc
#' @param model_function name of function who's formals are parsed to make UI
#' @param otherinputs a text string that specifies a list of other shiny inputs to include in the UI
#' @param packagename name of package using this function
#' @return A renderUI object that can be added to the shiny output object for display in a Shiny UI
#' @details This function is called by the Shiny app to produce the Shiny input UI elements.
#' It produces UI by 3 different ways.
#' 1. If use_mbmodel is TRUE, an mbmodel list structure, which needs to be provided, is used
#' 2. If use_mbmodel is FALSE and use_doc is TRUE, the documentation header of the function is used.
#' For that approach, model_file needs to contain the name/path to the R script for the function
#' The doc needs to have a specific format for this.
#' 3. If both use_mbmodel and use_doc are FALSE, the function formals are parsed and used as UI.
#' For that approach, model_function needs to specify the name of the model
#' model_function is assumed to be the name of a function.
#' The formals of the function will be parsed to create UI elements.
#' Non-numeric arguments of functions are removed and need to be included in the otherinputs argument.
#' @export

generate_shinyinput <- function(use_mbmodel = FALSE, mbmodel = NULL,
                                use_doc = FALSE, model_file = NULL,
                                model_function = NULL,
                                otherinputs = NULL, packagename = NULL)
{

    #function to wrap input elements in specified class
    #allows further styling with CSS in the shiny app
    myclassfct = function (x) {
        tags$div(class="myinput", x)
    }

    ###########################################
    #for an mbmodel, make input elements from model list structure
    if (use_mbmodel)  {
      if (!is.list(mbmodel))  {return("Please provide a valid mbmodel list structure.")}

      #numeric input elements for all variable initial conditions
        allv = lapply(1:length(mbmodel$var), function(n)
        {
            myclassfct(numericInput(mbmodel$var[[n]]$varname, paste0(mbmodel$var[[n]]$vartext,' (',mbmodel$var[[n]]$varname,')'),
                                    value = mbmodel$var[[n]]$varval, min = 0,step = mbmodel$var[[n]]$varval/100)
                        )
        })
        allp = lapply(1:length(mbmodel$par), function(n)
        {
            myclassfct(numericInput(mbmodel$par[[n]]$parname, paste0(mbmodel$par[[n]]$partext,' (',mbmodel$par[[n]]$parname,')'),
                value = mbmodel$par[[n]]$parval, min = 0, step = mbmodel$par[[n]]$parval/100)
            )
        })
        allt = lapply(1:length(mbmodel$time), function(n) {
            myclassfct(numericInput(mbmodel$time[[n]]$timename, paste0(mbmodel$time[[n]]$timetext,' (',mbmodel$time[[n]]$timename,')'),
                value = mbmodel$time[[n]]$timeval, min = 0, step = mbmodel$time[[n]]$timeval/100)
            )
        })
    modelargs = c(allv,allp,allt)
    #end input construction for mbmodel
    } else if (use_doc) {

      if (!file.exists(model_file)) {return("Please provide path to a valid model R file.")}

      ###########################################
      #create UI elements from doc of file
      #get every line in documentation part of file that starts with @param
      x = readLines(model_file)
      x2 = grep('@param', x, value = TRUE)
      pattern = ".*[:](.+)[:].*" #regex for capturing text between colons
      x3 = gsub(pattern, "\\1",x2)
      x3 = substr(x3,2,nchar(x3)-1); #remove blanks in front and back
      ip = formals(model_function) #get model inputs
      #remove function arguments that are not numeric
      ip = ip[unlist(lapply(ip,is.numeric))]
      #build shiny numeric inputs for each numeric argument in function,
      #set the explanatory text from the file documentation as label, set the value to the function default
      modelargs = lapply(1:length(ip), function(n)
      {
          iplabel = paste0(names(ip[n]),', ', x3[n]) #text label for input
          myclassfct(

              shiny::numericInput(names(ip[n]), label = iplabel, value = ip[n][[1]], step = 0.01*ip[n][[1]])
          ) #close myclassfct
      }) #close lapply
    #end UI creation using file doc
    } else {
      #if neither mbmodel is present nor doc, use function name to generate UI
      if (is.null(model_function))  {return("Please provide a valid model function name.")}

      ###########################################
      #create UI elements for non-mbmodel done by parsing a function/R code
      ip = unlist(formals(model_function)) #get all input arguments for function

        # from input/argument vector, create the shiny inputs
        modelargs = lapply(1:length(ip), function(n)
        {
            #iplabel = paste0(names(ip[n]),', ', x3[n]) #text label for input
            myclassfct(
                shiny::numericInput(names(ip[n]), label = names(ip[n]), value = ip[n][[1]], step = 0.01*ip[n][[1]])
            ) #close myclassfct
        }) #close lapply
    } #finish UI creation for non-mbmodel


    ###########################################
    #no matter how UI is created, allow for additional elements
    #if the user provided otherinputs (which need to be in the form of a list of shiny input elements)
    #those will be added to the whole UI structure
    #the default is an empty string, then nothing will be added
    otherargs = shiny::tagList(
        shiny::selectInput("plotscale", "Log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
        shiny::selectInput("plotengine", "Plot engine",c("ggplot" = "ggplot", "plotly" = "plotly"))
    )
    otherargs = lapply(otherargs,myclassfct)

    if (!is.null(otherinputs) && nchar(otherinputs)>1)
    {
        moreargs = lapply(eval(str2expression(otherinputs)),myclassfct)
        otherargs = c(moreargs,otherargs)
    }

    #############################################################
    #for modelbuilder package, create additional UI elements
    if (packagename == "modelbuilder")
    {
      #standard additional input elements for each model
      standardui <- shiny::tagList(
        shiny::selectInput("modeltype", "Model to run",c("ODE" = "ode", 'stochastic' = 'stochastic', 'discrete time' = 'discrete'), selected = 'ode'),
        shiny::selectInput("plotscale", "Log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
        shiny::selectInput("plotengine", "Plot engine",c("ggplot" = "ggplot", "plotly" = "plotly"))
      ) #end taglist

      standardui = lapply(standardui,myclassfct)

      #standard additional input elements for each model
      stochasticui <- shiny::tagList(
        shiny::numericInput("nreps", "Number of simulations", min = 1, max = 500, value = 1, step = 1),
        shiny::numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 123, step = 1)
      ) #end taglist

      stochasticui = lapply(stochasticui,myclassfct)

      scanparui <- shiny::tagList(
        shiny::selectInput("scanparam", "Scan parameter", c("No" = 0, "Yes" = 1)),
        shiny::selectInput("partoscan", "Parameter to scan", sapply(mbmodel$par, function(x) x[[1]]) ),
        shiny::numericInput("parmin", "Lower value of parameter", min = 0, max = 1000, value = 1, step = 1),
        shiny::numericInput("parmax", "Upper value of parameter", min = 0, max = 1000, value = 10, step = 1),
        shiny::numericInput("parnum", "Number of samples", min = 1, max = 1000, value = 10, step = 1),
        shiny::selectInput("pardist", "Spacing of parameter values", c('linear' = 'lin', 'logarithmic' = 'log'))
      ) #end taglist

      scanparui = lapply(scanparui,myclassfct)

      otherargs = tagList(otherargs,
                    standardui,
                    p('Settings for stochastic model:'),
                    stochasticui,
                    p('Settings for optional parameter scan for ODE/discrete models:'),
                    scanparui)


    }  #finish extra UI elements for modelbuilder UI generation

    ###########################################
    #return structure
    modelinputs <- tagList(
            p(
                shiny::actionButton("submitBtn", "Run Simulation", class = "submitbutton"),
                shiny::actionButton(inputId = "reset", label = "Reset Inputs", class = "submitbutton"),
                #shiny::downloadButton(outputId = "download_code", label = "Download Code", class = "submitbutton"),
                align = 'center'),
            modelargs,
            otherargs
        ) #end tagList

    return(modelinputs)
} #end overall function


