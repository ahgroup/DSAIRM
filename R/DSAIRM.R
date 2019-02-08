#' DSAIRM: A package to learn about Dynamical Systems Approaches to
#' Immune Response Modeling
#'
#' The DSAIRM package provides a number of Shiny apps that simulate various
#' within-host infection and immune response dynamics models.
#' By manipulating the models and working
#' through the instructions provided within the Shiny UI, you can learn about
#' some important concepts in immune response modeling. You will also
#' learn how models can be used to study such concepts.
#'
#' @section Package Structure:
#'   The package is structured in a modular way. Each
#'   Shiny app calls an underlying function (which in turn might call other functions).
#'   The structure of the package allows you to interact with the models in 3 ways:
#'
#'   1. Start the main menu of the package by calling dsairmmenu(). Pick a Shiny
#'   app corresponding to a model/topic, explore it through the
#'   corresponding Shiny UI. The UI contains information about the model
#'   and a list of tasks to try. This is the main intended use of this package.
#'
#'   2. Call each simulator function directly from the R console, without going
#'   through the Shiny app. Each model simulator function is called simulate_XXX
#'   and is documented. See the 'Further Information' tab for a given Shiny app
#'   to find the names of the different simulation functions.
#'
#'   Calling the functions directly allows you more flexibility.
#'   For instance you could write a few
#'   lines of extra R code to loop over some model parameter, instead of the manual setting
#'   through the sliders in the Shiny app. This gives you more options, but requires
#'   being able to write a little bit of R code.
#'
#'   3. Find the code for a simulator function you are interested in and modify
#'   it to your needs. This provides the most flexibility in what you can do with
#'   this package, and you can end up with any model you need,
#'   but for that you need to know or learn some
#'   more R coding. To make it easy to get the source code for the simulator functions,
#'   they are located in a subdirectory called 'simulatorfunctions' inside the main
#'   package folder.
#'
#' @docType package
#' @name DSAIRM
NULL
