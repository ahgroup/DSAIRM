############################################################
##a template for a new model
## currently contains the basic virus model implemented as ODE as example
## replace with your own code, or use another one of the simulate_ functions
## as starting point for yours
############################################################


#function that specificies the ode model
virusode <- function(t, y, parms)
{
   with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {

      dUdt = n - dU*U - b*V*U
      dIdt = b*V*U - dI*I
      dVdt = p*I - dV*V - b*g*V*U

	 	  list(c(dUdt, dIdt, dVdt))
    }
   ) #close with statement
} #end function specifying the ODEs


#' Simulation of a basic viral infection model without an immune response
#'
#' @description This function runs a simulation of a 3 compartment model
#' using a set of ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param U0 initial number of uninfected target cells
#' @param I0 initial number of infected target cells
#' @param V0 initial number of infectious virions
#' @param tmax maximum simulation time, units depend on choice of units for parameters
#' @param n rate of new uninfected cell replenishment
#' @param dU rate at which uninfected cells die
#' @param dI rate at which infected cells die
#' @param dV rate at which infectious virus is cleared
#' @param b rate at which virus infects cells
#' @param p rate at which infected cells produce virus
#' @param g possible conversion factor for virus units
#' @return A list. The list has only one element called ts.
#' ts contains the time-series of the simulation.
#' The 1st column of ts is Time, the other columns are the model variables
#' @details A simple compartmental model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_Basic_Virus_model_ode()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_Basic_Virus_model_ode(V0 = 100, tmax = 100, n = 1e5, dU = 1e-2)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result$ts[,"Time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_newmodel <- function(U0 = 1e7, I0 = 0, V0 = 1, tmax = 30, n=0, dU = 0, dI = 1, dV = 2, b = 2e-7, p = 5, g = 1)
{
  Y0 = c(U = U0, I = I0, V = V0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = sort(unique(c(seq(0, tmax, dt), 1:ceiling(tmax))))
  #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  #the additional inclusion of 1:celining(tmax) is so we ensure when this function is called by the fitting routines
  #it returns values at times at which data is available

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b,p=p,g=g);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = virusode, parms=pars, atol=1e-12, rtol=1e-12);
  colnames(odeoutput) = c('Time','U','I','V')

  #return result as list, with element ts containing the time-series
  #your function needs to return a list with either ts as time series or $dat as general data
  #depending on what you return, it might need to be processed a bit in the app.R file
  result = list()
  result$ts = as.data.frame(odeoutput)
  return(result)
}
