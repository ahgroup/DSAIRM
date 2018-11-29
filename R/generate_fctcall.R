#' @title A helper function that takes a model and settings and generates the function cal needed to run it
#'
#' @description This function generates a text string that contains the function call for a given model
#' This is a helper function called by the shiny app.
#' @param input shiny input structure
#' @param model a modelbuilder model structure
#' @param modeltype type of model/code that should be run
#' @return a string that can be evaluated to run the model
#' @details This function is called by the Shiny server to produce a function text string.
#' @author Andreas Handel
#' @export

generate_fctcall <- function(input, model, modeltype)
{
    #process all variables, parameters and times from the model structure
    #to create the input string for the main function call
    nvars = length(model$var)
    varstring = "vars = c("
    for (n in 1:nvars)
    {
        varstring = paste0(varstring,
                           model$var[[n]]$varname,
                           " = ",
                           isolate(input[[model$var[[n]]$varname]]),
                           ', ')
    }
    varstring = substr(varstring, 1, nchar(varstring) - 2)
    varstring = paste0(varstring, '), ') #close parentheses

    npars = length(model$par)
    parstring = "pars = c("
    for (n in 1:npars)
    {
        parstring = paste0(parstring,
                           model$par[[n]]$parname,
                           " = ",
                           isolate(input[[model$par[[n]]$parname]]),
                           ', ')
    }
    parstring = substr(parstring, 1, nchar(parstring) - 2)
    parstring = paste0(parstring, '), ') #close parentheses

    ntime = length(model$time)
    timestring = "time = c("
    for (n in 1:ntime)
    {
        timestring = paste0(timestring,
                            model$time[[n]]$timename,
                            " = ",
                            isolate(input[[model$time[[n]]$timename]]),
                            ', ')
    }
    timestring = substr(timestring, 1, nchar(timestring) - 2)
    timestring = paste0(timestring, ') ')

    filename = paste0('simulate_',gsub(" ", "_", model$title), "_",modeltype) #name of function to be called
    fctargs = paste0(varstring, parstring, timestring) #arguments for function
    fctcall = paste0('simresult <- ', filename, '(', fctargs, ')') # complete string for function call


    return(fctcall)
}
