#' @title A helper function that takes a model and generates the shiny UI elements for the analyze tab
#'
#' @description This function generates numeric shiny UI inputs for a supplied model.
#' This is a helper function called by the shiny app.
#' @param mbmodel a name of a file/function or a modelbuilder model structure
#' @param otherinputs a list of other inputs to include
#' @param output shiny output structure
#' @return No direct return. output structure is modified to contain text for display in a Shiny UI
#' @details This function is called by the Shiny app to produce the Shiny input UI elements.
#' If mbmodel is an object, it is assumed to be a mbmodel type and will be parsed to create the UI elements.
#' If mbmodel is a character, it is assumed to be the name of a function which will be parsed to create UI elements.
#' Non-numeric arguments of functions are removed.
#' @author Andreas Handel
#' @export

#not used in DSAIRM, might need to turn on again for modelbuilder
generate_shinyinput <- function(mbmodel, otherinputs, output)
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
    if (class(mbmodel)=="character" )
    {

        #file containing model
        fcfile = paste0(system.file("simulatorfunctions", package = "DSAIRM"),'/',mbmodel,'.R')
        #get every line in documentation part of file that starts with @param

        # turn each @param statement into a string for display

        ip = formals(mbmodel)
        #remove function arguments that are not numeric
        ip = ip[unlist(lapply(ip,is.numeric))]
        nvars = length(ip)  #number of variables/compartments in model
        modelargs = lapply(1:nvars, function(n) {
            myclassfct(
                numericInput(names(ip[n]), names(ip[n]), value = ip[n][[1]])
            ) #close myclassfct
        }) #close lapply
    } #end UI creation for an underlying function


    ###########################################
    #create UI elements as input/output for shiny by parsing a modelbuilder object
    ###########################################
    if (class(mbmodel)=="list")
    {

        nvars = length(mbmodel$var)  #number of variables/compartments in model
        npars = length(mbmodel$par)  #number of parameters in model
        ntime = length(mbmodel$time)  #number of time variables in model

        #numeric input elements for all variable initial conditions
        allv = lapply(1:nvars, function(n) {
            myclassfct(numericInput(mbmodel$var[[n]]$varname,
                             paste0(mbmodel$var[[n]]$vartext,' (',mbmodel$var[[n]]$varname,')'),
                             value = mbmodel$var[[n]]$varval,
                             min = 0, step = mbmodel$var[[n]]$varval/100))
                    })

        allp = lapply(1:npars, function(n) {
            myclassfct(numericInput(
                    mbmodel$par[[n]]$parname,
                    paste0(mbmodel$par[[n]]$partext,' (',mbmodel$par[[n]]$parname,')'),
                    value = mbmodel$par[[n]]$parval,
                    min = 0, step = mbmodel$par[[n]]$parval/100))
                    })

        allt = lapply(1:ntime, function(n) {
            myclassfct(numericInput(
                    mbmodel$time[[n]]$timename,
                    paste0(mbmodel$time[[n]]$timetext,' (',mbmodel$time[[n]]$timename,')'),
                    value = mbmodel$time[[n]]$timeval,
                    min = 0, step = mbmodel$time[[n]]$timeval/100))
                    })
        modelargs = c(allv,allp,allt)
    } #end mbmodel object parsing

if (!is.null(otherinputs))
{
    otherargs = lapply(otherinputs,myclassfct)
}

#return structure
output$modelinputs <- renderUI({
    tagList(modelargs, otherargs)
}) #end renderuI

} #end overall function

