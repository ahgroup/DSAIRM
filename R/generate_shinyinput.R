#' @title A helper function that takes a model and generates the shiny UI elements for the analyze tab
#'
#' @description This function generates numeric shiny UI inputs for a supplied model.
#' This is a helper function called by the shiny app.
#' @param mbmodel a name of a file/function or a modelbuilder model structure
#' @param output shiny output structure
#' @return No direct return. output structure is modified to contain text for display in a Shiny UI
#' @details This function is called by the Shiny app to produce the Shiny input UI elements.
#' If mbmodel is an object, it is assumed to be a mbmodel type and will be parsed to create the UI elements.
#' If mbmodel is a character, it is assumed to be the name of a function which will be parsed to create UI elements.
#' Non-numeric arguments of functions are removed.
#' @author Andreas Handel
#' @export

generate_shinyinput <- function(mbmodel, output)
{

    ###########################################
    #create UI elements as input/output for shiny by parsing a function/R code
    ###########################################
    if (class(mbmodel)=="character" )
    {
        ip = formals(mbmodel)
        #remove function arguments that are not numeric
        ip = ip[unlist(lapply(ip,is.numeric))]

        nvars = length(ip)  #number of variables/compartments in model

        output$vars <- renderUI({
            allv = lapply(1:nvars, function(n) {
                numericInput(names(ip[n]), names(ip[n]), value = ip[n][[1]])
            })
            do.call(mainPanel, allv)
        })

    }

    ###########################################
    #create UI elements as input/output for shiny by parsing a modelbuilder object
    ###########################################
    if (class(mbmodel)!="character")
    {
        #creates title
        output$title <- renderUI({
            HTML(mbmodel$title)
        })

        #numeric input elements for all variable initial conditions
        output$vars <- renderUI({

            nvars = length(mbmodel$var)  #number of variables/compartments in model
            allv = lapply(1:nvars, function(n) {
                numericInput(mbmodel$var[[n]]$varname,
                             paste0(mbmodel$var[[n]]$vartext,' (',mbmodel$var[[n]]$varname,')'),
                             value = mbmodel$var[[n]]$varval,
                             min = 0,
                             step = mbmodel$var[[n]]$varval/100)
            })

            npars = length(mbmodel$par)  #number of parameters in model
            allp = lapply(1:npars, function(n) {
                numericInput(
                    mbmodel$par[[n]]$parname,
                    paste0(mbmodel$par[[n]]$partext,' (',mbmodel$par[[n]]$parname,')'),
                    value = mbmodel$par[[n]]$parval,
                    min = 0,
                    step = mbmodel$par[[n]]$parval/100
                )
            })

            ntime = length(mbmodel$time)  #number of time variables in model
            allt = lapply(1:ntime, function(n) {
                numericInput(
                    mbmodel$time[[n]]$timename,
                    paste0(mbmodel$time[[n]]$timetext,' (',mbmodel$time[[n]]$timename,')'),
                    value = mbmodel$time[[n]]$timeval,
                    min = 0,
                    step = mbmodel$time[[n]]$timeval/100
                )
            })

            do.call(mainPanel, c(allv,allp,allt))
        })

    } #end mbmodel object parsing

    #standard additional input elements for each model
    output$standard <- renderUI({
        tagList(
            numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1),
            selectInput("modeltype", "Model to run",c("ODE" = "ode", 'stochastic' = 'stochastic', 'discrete time' = 'discrete'), selected = 'ode'),
            numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 123, step = 1),
            selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
        ) #end taglist
    }) #end renderuI

} #end overall function

