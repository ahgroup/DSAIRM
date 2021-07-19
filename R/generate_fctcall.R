#' @title A helper function that produces a call to a simulator function for specific settings
#'
#' @description This function takes a modelsettings structure and uses that information
#' to create an unevaluated function call that runs the simulator function with the specified settings
#'
#'
#' @param modelsettings a list with model settings. Required list elements are: \cr
#' List elements with names and values for all inputs expected by simulation function. \cr
#' modelsettings$simfunction - name of simulation function in variable  \cr
#' @return A string containing an unevaluated function call with the specified settings, or an error message
#' @details This function produces a function call for specific settings.
#' @export

generate_fctcall <- function(modelsettings)
{
    #extract modelsettings inputs needed for simulator function
    currentmodel = modelsettings$currentmodel
    #match values provided from UI with those expected by function
    settingsvec = unlist(modelsettings)

    #try to get input arguments for function
    #at some point one should implement error handling for this
    modelinput = formals(currentmodel)
    ip = unlist(modelinput) #get all input arguments for function

    #pull out the inputs required by the simulator function from those
    #provided by the user/UI/shiny inputs
    currentargs = settingsvec[match(names(ip), names(settingsvec))]

    #if users do not provide all inputs but leave some cells empty
    #the matching will produce NA
    #we need users to provide all inputs, thus if NA are detected
    #we return an error message
    if (sum(is.na(currentargs)>0))
    {
        return('Please provide values for all inputs.')
    }
    #this is an alternative, silently dropping non-supplied values and using defaults
    #not a good idea
    #currentargs <- currentargs[!is.na(currentargs)]
    #make a list, makes conversion to numeric easier
    arglist = as.list(currentargs)
    #convert arguments for function call to numeric if possible
    #preserve those that can't be converted
    numind = suppressWarnings(!is.na(as.numeric(arglist))) #find numeric values
    arglist[numind] = as.numeric(currentargs[numind])

    #add function name as first element to list
    fctlist = append(parse(text = currentmodel), arglist)

    #make fct call
    fctcall <- as.call(fctlist)
    return(fctcall)
}


