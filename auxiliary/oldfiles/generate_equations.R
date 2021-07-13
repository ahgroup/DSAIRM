#' Turn a model into a set of differential equations displayed as LaTeX/HTML object
#'
#' This function takes as input a model and produces output that displays ODE equations
#'
#' @description The model needs to adhere to the structure specified by the modelbuilder package
#' models built using the modelbuilder package automatically have the right structure
#' a user can also build a model list structure themselves following the specifications
#' if the user provides a file name, this file needs to contain an object called 'model'
#' and contain a valid modelbuilder model structure
#' @param mbmodel modelbuilder model structure, either as list object or file name
#' @return The function returns equations as an html object
#' @author Andreas Handel
#' @export

generate_equations <- function(mbmodel)
{
    #if the model is passed in as a file name, load it
    #otherwise, it is assumed that 'model' is a list structure of the right type
    if (is.character(mbmodel)) {load(mbmodel)}

    nvars = length(mbmodel$var)  #number of variables/compartments in model
    #text for equations
    seqs= "$$ \n\\begin{aligned} \n"
    for (n in 1:nvars)
    {
        seqs = paste0(seqs,'\\dot ', mbmodel$var[[n]]$varname,' & = ',paste(mbmodel$var[[n]]$flows, collapse = ' '), ' \\\\ \n' )
    }
    seqs=paste0(seqs,'\\end{aligned} \n$$')
    #strip any * signs from equation for display
    seqs = gsub("\\*","",seqs)
    #finish block that creates the ODE function
    ##############################################################################
    #return as html object
    returntext = shiny::HTML(seqs)
}
