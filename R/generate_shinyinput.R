#' @title A helper function that takes a model and generates the shiny UI elements for it output
#'
#' @description This function generates input buttons and sliders for a supplied model.
#' This is a helper function called by the shiny app.
#' @param model a modelbuilder model structure
#' @param output shiny output structure
#' @return HTML formatted text for display in a Shiny UI
#' @details This function is called by the Shiny server to produce the Shiny input UI elements.
#' @author Andreas Handel
#' @export

generate_shinyinput <- function(model, output)
{
    ###########################################
    #server part that dynamically creates the UI
    output$vars <- renderUI({
        nvars = length(model$var)  #number of variables/compartments in model
        allv = lapply(1:nvars, function(n) {
            numericInput(model$var[[n]]$varname,
                         paste0(model$var[[n]]$vartext,' (',model$var[[n]]$varname,')'),
                         value = model$var[[n]]$varval,
                         min = 0,
                         step = model$var[[n]]$varval/100)
        })
        do.call(mainPanel, allv)
    })

    output$pars <- renderUI({
        npars = length(model$par)  #number of parameters in model
        allp = lapply(1:npars, function(n) {
            numericInput(
                model$par[[n]]$parname,
                paste0(model$par[[n]]$partext,' (',model$par[[n]]$parname,')'),
                value = model$par[[n]]$parval,
                min = 0,
                step = model$par[[n]]$parval/100
            )
        })
        do.call(mainPanel, allp)
    })

    output$time <- renderUI({
        ntime = length(model$time)  #number of time variables in model
        allt = lapply(1:ntime, function(n) {
            numericInput(
                model$time[[n]]$timename,
                paste0(model$time[[n]]$timetext,' (',model$time[[n]]$timename,')'),
                value = model$time[[n]]$timeval,
                min = 0,
                step = model$time[[n]]$timeval/100
            )
        })
        do.call(mainPanel, allt)
    })

    output$other <- renderUI({
        tagList(
            numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1),
            selectInput("modeltype", "Model to run",c("ODE" = "ode", 'stochastic' = 'stochastic', 'discrete time' = 'discrete'), selected = 'ode'),
            numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 123, step = 1),
            selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
            ) #end taglist
    }) #end renderuI


    output$title <- renderUI({
        HTML(model$title)
    }) #creates title


}
