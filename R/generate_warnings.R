#' @title A helper function that takes result from the simulators and produces warnings
#'
#' @description This function generates warnings to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation
#' @param input the shiny app input structure
#' @param output the shiny app output structure
#' @return output warn elements for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the shiny UI
#' @author Andreas Handel
#' @export

generate_warnings <- function(input,output)
{

  # if we have any warnings or error from the simulator we can show them here
  # That text will be shown in red in the UI ("warn" placeholder will be used)
  output$warn <- renderUI({
    input$submitBtn

    warntxt <- ""

    if(length(utils::data()$warns) == 0){

    }else{
      warntxt <- paste(warntxt, "Warnings:", sep = "<br/>")
      for (i in 1:length(utils::data()$warns)){
        warntxt <- paste(warntxt, utils::data()$warns[[i]], sep = "<br/>")
      }
    }
    HTML(warntxt)
  })
}
