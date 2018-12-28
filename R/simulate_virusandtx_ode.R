#' Simulation of a basic viral infection model with drug treatment
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system in the presence of drug treatment.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a list containing time-series of each variable and time.
#' inspired by a study on HCV and IFN treatment (Neumann et al. 1998, Science)
#'
#' @param U0 initial number of uninfected target cells
#' @param I0 initial number of infected target cells
#' @param V0 initial number of infectious virions
#' @param n rate of new uninfected cell replenishment
#' @param dU rate at which uninfected cells die
#' @param dI rate at which infected cells die
#' @param dV rate at which infectious virus is cleared
#' @param b rate at which virus infects cells
#' @param p rate at which infected cells produce virus
#' @param g conversion between experimental and model virus units
#' @param f strength of cell infection reduction by drug (0-1)
#' @param e strength of virus production reduction by drug (0-1)
#' @param steadystate if this is set to TRUE, the starting values for U, I and V are set
#' to their steady state values. User supplied values for U0, I0, V0 are ignored.
#' @param txstart time at which treatment starts
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @param ... Other parameters that could be passed into function as potential pass-through
#' @return A list. The list has only one element called ts.
#' ts contains the time-series of the simulation.
#' The 1st column of ts is Time, the other columns are the model variables.
#' @details A simple compartmental model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_virusandtx_ode()
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_virusandtx_ode(V0 = 100, tmax = 100, n = 1e5, dU = 1e-2)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"Time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_virusandtx_ode <- function(U0 = 1e5, I0 = 0, V0 = 1, tmax = 30, n=1e4, dU = 0.1, dI = 1, dV = 2, b = 1e-5, p = 10, g = 1, f = 0, e = 0, steadystate = FALSE, txstart = 0, ...)
{


  #function that specificies the ode model
  virustxode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {

        enow = ifelse(t>txstart,e,0) #turn on drug at time txstart
        fnow = ifelse(t>txstart,f,0) #turn on drug at time txstart
        dUdt = n - dU*U - (1-fnow)*b*V*U
        dIdt = (1-fnow)*b*V*U - dI*I
        dVdt = (1-enow)*p*I - dV*V - g*b*U*V

        list(c(dUdt, dIdt, dVdt))
      }
    ) #close with statement
  } #end function specifying the ODEs



  #override user-supplied initial conditions and instead start with steady state values
  if (steadystate == TRUE) {U0 = max(0,dV*dI/(b*(p-dI*g))); I0 = max(0, (b*n*(dI*g-p)+dI*dU*dV) /(b * (dI^2*g - dI*p))); V0 = max(0,  - (b*n*(dI*g-p)+dI*dU*dV) /  (b*dV*dI))}

  Y0 = c(U = U0, I = I0, V = V0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b,p=p,g=g,f=f,e=e,txstart = txstart);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = virustxode, parms=pars, atol=1e-12, rtol=1e-12);

  colnames(odeoutput) = c('Time','U','I','V')

  #return result as list, with element ts containing the time-series
  result = list()
  result$ts = as.data.frame(odeoutput)
  return(result)
}
