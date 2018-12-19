#' @title A helper function that takes a model and generates the shiny UI elements for the analyze tab
#'
#' @description This function generates input buttons and sliders for a supplied model.
#' This is a helper function called by the shiny app.
#' @param mbmodel a modelbuilder model structure
#' @param otherinputs additional elements to be shown in the UI
#' @param output shiny output structure
#' @return No direct return. output structure is modified to contain text for display in a Shiny UI
#' @details This function is called by the Shiny app to produce the Shiny input UI elements.
#' @author Andreas Handel
#' @export

generate_shinyinput <- function(mbmodel, otherinputs = NULL, output)
{

    #if mbmodel is a filename, it is assumed that this is R function and the function is parsed to extract inputs
    #if mbmodel is an object, it is assumed that this is a modelbuilder object and inputs are based on parsing this object
    browser()
    ###########################################
    #create UI elements as input/output for shiny by parsing a function/R code
    ###########################################
    if (is.null(mbmodel$title) )
    {
        ip = formals(mbmodel)
        nvars = length(ip)  #number of variables/compartments in model



        output$vars <- renderUI({
            allv = lapply(1:nvars, function(n) {
                numericInput(names(ip[n]), names(ip[n]), value = ip[n][[1]]
)
            })
            do.call(mainPanel, allv)
        })

    }

    ###########################################
    #create UI elements as input/output for shiny by parsing a modelbuilder object
    ###########################################

    if (!is.null(mbmodel$title) )
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
        do.call(mainPanel, allv)
    })

    #numeric input elements for all parameter values
    output$pars <- renderUI({
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
        do.call(mainPanel, allp)
    })

    #numeric input elements for time
    output$time <- renderUI({
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
        do.call(mainPanel, allt)
    })

    #standard additional input elements for each model
    output$standard <- renderUI({
        tagList(
            numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1),
            selectInput("modeltype", "Model to run",c("ODE" = "ode", 'stochastic' = 'stochastic', 'discrete time' = 'discrete'), selected = 'ode'),
            numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 123, step = 1),
            selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
            ) #end taglist
    }) #end renderuI

    }

    # #additional input elements for a specific model
    # #should be supplied as a tagList
    # if (!is.null(otherinputs))
    # {
    #     output$other <- renderUI({
    #         otherinputs
    #     }) #end renderuI
    # }

} #end overall function

