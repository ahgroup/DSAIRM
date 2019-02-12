#' @title A helper function that takes a model and generates the shiny UI elements for the analyze tab
#'
#' @description This function generates numeric shiny UI inputs for a supplied model.
#' This is a helper function called by the shiny app.
#' @param mbmodel a name of a function for which to build inputs
#' @param otherinputs a list of other shiny inputs to include
#' @param packagename name of package using this function
#' @return A renderUI object that can be added to the shiny output object for display in a Shiny UI
#' @details This function is called by the Shiny app to produce the Shiny input UI elements.
#' mbmodel is assumed to be the name of a function which will be parsed to create UI elements.
#' Non-numeric arguments of functions are removed.
#' @author Andreas Handel
#' @export

#not used in DSAIRM, might need to turn on again for modelbuilder
generate_shinyinput <- function(mbmodel, otherinputs = NULL, packagename)
{

    #function to wrap input elements in specified class
    #allows further styling with CSS
    myclassfct = function (x) {
        tags$div(class="myinput", x)
    }

    ###########################################
    #create UI elements as input/output for shiny by parsing a function/R code
    #currently requires that function arguments are given in a vector, not a list like mbmodel functions do
    ###########################################

    fcfile = paste0(system.file("simulatorfunctions", package = packagename),'/',mbmodel,'.R')
    #get every line in documentation part of file that starts with @param
    x = readLines(fcfile)
    x2 = grep('@param', x, value = TRUE)
    pattern = ".*[:](.+)[:].*" #regex for capturing text between colons
    x3 = gsub(pattern, "\\1",x2)
    x3 = substr(x3,2,nchar(x3)-1); #remove blanks in front and back
    #using stringr
    #pattern = ":.+:" #regex for capturing text between colons
    #x3 = stringr::str_extract_all(x2, pattern, simplify = TRUE)
    #x3=substr(x3,3,nchar(x3)-2); #remove : and blanks in front and back
    ip = formals(mbmodel) #get model inputs
    #remove function arguments that are not numeric
    ip = ip[unlist(lapply(ip,is.numeric))]
    modelargs = lapply(1:length(ip), function(n)
    {
        iplabel = paste0(names(ip[n]),', ', x3[n]) #text label for input
        myclassfct(

            numericInput(names(ip[n]), label = iplabel, value = ip[n][[1]])
        ) #close myclassfct
    }) #close lapply

    otherargs = NULL
    if (!is.null(otherinputs))
    {
        otherargs = lapply(otherinputs,myclassfct)
    }

    #return structure
    modelinputs <- tagList(
            p(
                actionButton("submitBtn", "Run Simulation", class = "submitbutton"), 
                actionButton(inputId = "reset", label = "Reset Inputs", class = "submitbutton"),
                align = 'center'),
            modelargs,
            otherargs
        ) #end tagList

    return(modelinputs)

} #end overall function

