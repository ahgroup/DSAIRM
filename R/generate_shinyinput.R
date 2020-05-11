#' @title A helper function that takes a model and generates shiny UI elements
#'
#' @description This function generates shiny UI inputs for a supplied model.
#' This is a helper function called by the shiny app.
#' @param mbmodel a string containing the name of a simulator function for which to build inputs
#' @param otherinputs a list of other shiny inputs to include in the UI
#' @param packagename name of package using this function (DSAIDE or DSAIRM)
#' @return A renderUI object that can be added to the shiny output object for display in a Shiny UI
#' @details This function is called by the Shiny app to produce the Shiny input UI elements.
#' mbmodel is assumed to be the name of a function. The file correpsonding to this function is assumed to live in the
#' simulatorfunctions subfolder and to be an exact copy of the same file in the /R folder of the source package.
#' This R file will be loaded and the documentation parsed to create UI elements.
#' Therefore, all simulator_ R functions/scripts need to follow a specific syntax.
#' Every argument needs to be of the form
#' param X : param explanation : param type
#' example:
#' b : transmission rate : numeric
#' Non-numeric arguments of functions are removed and need to be included in the otherinputs argument.
#' @export

generate_shinyinput <- function(mbmodel, otherinputs = NULL, packagename)
{

    #function to wrap input elements in specified class
    #allows further styling with CSS in the shiny app
    myclassfct = function (x) {
        tags$div(class="myinput", x)
    }

    ###########################################
    #create UI elements as input/output for shiny
	#done by parsing a function/R code
    #requires that function arguments are given in a vector
    #find R file that contains the simulator_ code of the specified name
    fcfile = paste0(system.file("simulatorfunctions", package = packagename),'/',mbmodel,'.R')
    #get every line in documentation part of file that starts with @param
    x = readLines(fcfile)
    x2 = grep('@param', x, value = TRUE)
    pattern = ".*[:](.+)[:].*" #regex for capturing text between colons
    x3 = gsub(pattern, "\\1",x2)
    x3 = substr(x3,2,nchar(x3)-1); #remove blanks in front and back
    ip = formals(mbmodel) #get model inputs
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

    #if the user provided otherinputs (which need to be in the form of a list of shiny input elements)
    #thos will be added to the whole UI structure
    otherargs = NULL
    if (!is.null(otherinputs))
    {
        otherargs = lapply(otherinputs,myclassfct)
    }

    #return structure
    modelinputs <- tagList(
            p(
                shiny::actionButton("submitBtn", "Run Simulation", class = "submitbutton"),
                shiny::actionButton(inputId = "reset", label = "Reset Inputs", class = "submitbutton"),
                shiny::downloadButton(outputId = "download_code", label = "Download Code", class = "submitbutton"),
                align = 'center'),
            modelargs,
            otherargs
        ) #end tagList

    return(modelinputs)
} #end overall function

