#' Basic Virus model - ODE
#'
#' @description A basic virus infection model with 3 compartments, implemented as ODEs. The model tracks uninfected and infected target cells and free virus. The processes modeled are infection, virus production, uninfected cell birth and death, infected cell and virus death.
#'
#' @details The model is implemented as a set of ordinary differential equations (ODE) using the deSolve package.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param U : Starting value for uninfected cells : numeric
#' @param I : Starting value for infected cells : numeric
#' @param V : Starting value for virus : numeric
#' @param n : Rate of new uninfected cell replenishment : numeric
#' @param dU : Rate at which uninfected cells die : numeric
#' @param dI : Rate at which infected cells die : numeric
#' @param dV : Rate at which virus is cleared : numeric
#' @param b : Rate at which virus infects cells : numeric
#' @param p : Rate at which infected cells produce virus : numeric
#' @param g : Possible conversion factor for virus units : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Times for which result is returned : numeric
#' @return The function returns the output as a list.
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}.
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.
#' @examples
#' # To run the simulation with default parameters:
#' result <- simulate_basicvirus_ode()
#' @section Notes: The parameter dt only determines for which times the solution is returned, it is not the internal time step. The latter is set automatically by the ODE solver.
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @export

simulate_basicvirus_ode <- function(U = 1e+05, I = 0, V = 10, n = 0, dU = 0, dI = 1, dV = 4, b = 1e-06, p = 100, g = 1, tstart = 0, tfinal = 50, dt = 0.1)
{
  #Block of ODE equations for deSolve
  basicvirus_ode_fct <- function(t, y, parms)
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
  odeout = deSolve::ode(y = vars, parms = pars, times = timevec,  func = basicvirus_ode_fct, atol=1e-12, rtol=1e-12)
  result <- list()
  result$ts <- as.data.frame(odeout)
  return(result)
}
