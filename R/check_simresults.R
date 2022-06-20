#' @title A function that checks if a call to simulation function worked
#'
#' @description This function takes the return from a call to a simulate_
#' function and checks that no errors occured.
#' If something goes wrong, an error message is returned
#'
#' @param simresult a list of results from a single call to the simulation functions
#' @return Ane error message string or NULL
#' @details This function checks simulation results generated from a single call to a simulate_ function.
#' @export

check_simresults <- function(simresult) {

  checkres = NULL
    if (class(simresult)!="list") #if the return from the simulator function is not a list, something went wrong
    {
      checkres <- 'Model run failed. Maybe unreasonable parameter values?'
      return(checkres)
    }
    #if simeresult is a list, check that no values in time-series are NaN or NA or Inf
    if (!is.null(simresult$ts))
    {
      if (   (sum(is.nan(unlist(simresult$ts)))>0) || (sum(is.na(unlist(simresult$ts)))>0) || (sum(is.infinite(unlist(simresult$ts)))>0) )
      {
        checkres <- 'Model run failed. Maybe unreasonable parameter values?'
        return(checkres)
      }
    }
    return(checkres)
}

