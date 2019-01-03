#' Basic Bacteria model
#'
#' A basic bacteria infection model with 2 compartments
#'
#' @details The model includes bacteria and an immune response. The processes are bacteria growth, death and killing, and immune response activation and decay. This is a predator-prey type model.
#' This code is based on a dynamical systems model created by the modelbuilder package.
#' The model is implemented here as a set of ordinary differential equations,
#' using the deSolve package.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param B : starting value for Bacteria
#' @param I : starting value for Immune Response
#' @param g : maximum rate of bacteria growth
#' @param Bmax : bacteria carrying capacity
#' @param dB : bacteria death rate
#' @param k : bacteria kill rate
#' @param r : immune response growth rate
#' @param dI : immune response decay rate
#' @param tstart : Start time of simulation
#' @param tfinal : Final time of simulation
#' @param dt : Times for which result is returned.
#' For ODE models, this is not the internal time step, the latter is set automatically by the ODE solver.
#' @return The function returns the output as a list.
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}.
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.
#' @examples
#' # To run the simulation with default parameters:
#' result <- simulate_basicbacteria_ode()
#' # To run the simulation with different parameter or starting values,
#' # supply the ones you want to change.
#' # all other parameters will be kept at their default values shown in the function call above
#' result <- simulate_basicbacteria_ode(B = 100, g = 0.5, dI = 2)
#' @section Warning: This function does not perform any error checking.
#' So if you try to do something nonsensical (e.g. have negative values for parameters),
#' the code will likely abort with an error message.
#' @section Model Author: Andreas Handel
#' @section Model creation date: 2018-12-21
#' @export

simulate_basicbacteria_ode <- function(B = 10, I = 1, g = 1, Bmax = 1e+05, dB = 0.1, k = 1e-06, r = 0.001, dI = 1, tstart = 0, tfinal = 30, dt = 0.05)
{

  #Block of ODE equations for deSolve
  basicbacteria_ode_fct <- function(t, y, parms)
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name
    #StartODES
    #Bacteria : bacteria growth : bacteria death : immune response killing :
    dB = +g*B*(1-B/Bmax) -dB*B -k*B*I
    #Immune Response : immune response growth : immune response decay :
    dI = +r*B*I -dI*I
    #EndODES
    list(c(dB,dI))
  } ) } #close with statement, end ODE function code block

  #Main function code block
  timevec=seq(tstart,tfinal,by=dt)
  vars = c(B = B, I = I)
  pars = c(g = g, Bmax = Bmax, dB =dB, k = k, r=r,dI=dI)
  odeout = deSolve::ode(y = vars, parms = pars, times = timevec,  func = basicbacteria_ode_fct)
  result <- list()
  result$ts <- as.data.frame(odeout)
  return(result)
}
