#' Basic Virus model
#'
#' A basic virus infection model with 3 compartments
#'
#' @details The model includes uninfected and infected target cells, as well as free virus. The processes that are modeled are infection, virus production, uninfected cell birth and death, infected cell and virus death.
#' This code is based on a dynamical systems model created by the modelbuilder package.
#' The model is implemented here as a set of ordinary differential equations,
#' using the deSolve package.
#' @param U : starting value for Uninfected cells
#' @param I : starting value for Infected cells
#' @param V : starting value for Virus
#' @param n : rate of new uninfected cell replenishment
#' @param dU : rate at which uninfected cells die
#' @param dI : rate at which infected cells die
#' @param dV : rate at which virus is cleared
#' @param b : rate at which virus infects cells
#' @param p : rate at which infected cells produce virus
#' @param g : possible conversion factor for virus units
#' @param tstart : Start time of simulation
#' @param tfinal : Final time of simulation
#' @param dt : Time step
#' @param ... other arguments for possible pass-through
#' @return The function returns the output as a list.
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}.
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.
#' @examples
#' # To run the simulation with default parameters:
#' result <- simulate_Basic_Virus_model_ode()
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @section Model Author: Andreas Handel
#' @section Model creation date: 2018-12-20
#' @export

simulate_Basic_Virus_model_ode <- function(U = 1e+05, I = 0, V = 1, n = 0, dU = 0, dI = 1, dV = 2, b = 2e-05, p = 5, g = 1, tstart = 0, tfinal = 30, dt = 0.1, ...)
{
  #Block of ODE equations for deSolve
  Basic_Virus_model_ode_fct <- function(t, y, parms)
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name
    #StartODES
    #Uninfected cells : cell birth : cell death : infection of cells :
    dU = n -dU*U -b*V*U
    #Infected cells : infection of cells : death of infected cells :
    dI = b*V*U -dI*I
    #Virus : virus production : virus removal : infection of cells :
    dV = p*I -dV*V -b*g*V*U
    #EndODES
    list(c(dU,dI,dV))
  } ) } #close with statement, end ODE code block

  #Main function code block
  timevec=seq(tstart,tfinal,by=dt)
  vars = c(U = U, I = I, V = V)
  pars = c(n = n, dU = dU, dI = dI, dV = dV, b = b, p = p, g = g)
  odeout = deSolve::ode(y = vars, parms = pars, times = timevec,  func = Basic_Virus_model_ode_fct)
  result <- list()
  result$ts <- as.data.frame(odeout)
  return(result)
}
