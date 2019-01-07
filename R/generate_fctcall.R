#' @title A helper function that takes a model and settings and generates the function call needed to run it
#'
#' @description This function generates a text string that contains the function call for a given model
#' This is a helper function.
#' @param modelsettings values for model settings
#' @param modelfunction a name of the model function to be run for the indicated settings
#' @return a string that can be evaluated to run the model
#' @details This function produces a function text string.
#' If modelfunction is a modelbuilder object, it will be parsed.
#' If modelfunction is a character string, this is the function to be run.
#' @author Andreas Handel
#' @export

generate_fctcall <- function(modelsettings, modelfunction)
{

    ###########################################
    #create UI elements as input/output for shiny by parsing the underlying function
    ###########################################
    #if (class(modelfunction)=="character" )
    #{
        ip = formals(modelfunction)
        fctargs = NULL
        for (n in 1:length(ip))
        {
            varind = match(names(ip[n]),  names(unlist(modelsettings))) #find index in modelsettings that contains value
            if (is.character(ip[[n]]) )
            {   #add extra quotation marks around value to preserve it as character
                fctargs=paste0(fctargs,names(ip[n]),' = "',modelsettings[[varind]],'",')
            }
            else
            {
                fctargs=paste0(fctargs,names(ip[n]),' = ',modelsettings[[varind]],',')
            }
        }
        fctargs = substr(fctargs, 1, nchar(fctargs) - 1) #chop off last comma
        fctcall = paste0('simresult <- ', modelfunction, '(', fctargs, ')') # complete string for function call
        return(fctcall)
}


    ###########################################
    #create UI elements as input/output for shiny by parsing a modelbuilder object
    #not used currently
    ###########################################
    # if (class(modelfunction)=="list")
    # {
    #
    # modeltype = modelsettings$currentmodel
    #
    # #process all variables, parameters and times from the model structure
    # #to create the input string for the main function call
    # nvars = length(modelfunction$var)
    # varstring = ""
    # #varstring = "vars = c("
    # for (n in 1:nvars)
    # {
    #     varstring = paste0(varstring,
    #                        modelfunction$var[[n]]$varname,
    #                        " = ",
    #                        modelsettings[modelfunction$var[[n]]$varname],
    #                        ', ')
    # }
    # varstring = substr(varstring, 1, nchar(varstring) - 2)
    # #varstring = paste0(varstring, '), ') #close parentheses
    #
    # npars = length(modelfunction$par)
    # #parstring = "pars = c("
    # parstring = ""
    # for (n in 1:npars)
    # {
    #     parstring = paste0(parstring,
    #                        modelfunction$par[[n]]$parname,
    #                        " = ",
    #                        modelsettings[modelfunction$par[[n]]$parname],
    #                        ', ')
    # }
    # parstring = substr(parstring, 1, nchar(parstring) - 2)
    # #parstring = paste0(parstring, '), ') #close parentheses
    #
    # ntime = length(modelfunction$time)
    # timestring = ""
    # #timestring = "time = c("
    # for (n in 1:ntime)
    # {
    #     timestring = paste0(timestring,
    #                         modelfunction$time[[n]]$timename,
    #                         " = ",
    #                         modelsettings[modelfunction$time[[n]]$timename],
    #                         ', ')
    # }
    # timestring = substr(timestring, 1, nchar(timestring) - 2)
    # #timestring = paste0(timestring, ') ')
    #
    # filename = paste0('simulate_',gsub(" ", "_", modelfunction$title), "_",modeltype) #name of function to be called
    # fctargs = paste0(varstring, parstring, timestring) #arguments for function
    # fctcall = paste0('simresult <- ', filename, '(', fctargs, ')') # complete string for function call
    #
    # }

    #browser()

