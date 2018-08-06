############################################################
##simulation of a simple viral infection model
# including simple drug Pk-Pd
# makes use of the deSolve eventfunction
##written by Andreas Handel (ahandel@uga.edu), last change 7/18/18
############################################################


#function that specificies the ode model
pkpdode <- function(t, y, parms)
{
   with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {
      e=Emax*C^k/(C^k + C50) #drug efficacy, based on level of C
      dUdt = n - dU*U - b*V*U
      dIdt = b*V*U - dI*I
      dVdt = (1-e)*p*I - dV*V - g*b*V*U
      dCdt = - dC*C

	 	  list(c(dUdt, dIdt, dVdt, dCdt))
    }
   ) #close with statement
} #end function specifying the ODEs

adddrug <- function(t,y,parms)
{
  y['C'] = y['C'] + parms['C0']
  return(y)
}


#' Simulation of a viral infection model with drug PK and PD
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
#' @param g unit conversion factor
#' @param p rate at which infected cells produce virus
#' @param C0 drug dose given at specified times
#' @param dC drug concentration decay rate
#' @param C50 drug concentration at which effect is half maximum
#' @param k steepness of concentration-dependent drug effect
#' @param Emax maximum drug effect (0-1)
#' @param txstart time of drug treatment start
#' @param txinterval time between drug doses
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
#' result <- simulate_pkpdmodel()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_pkpdmodel(V0 = 100, tmax = 100, n = 1e5, dU = 1e-2)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result$ts[,"Time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_pkpdmodel <- function(U0 = 1e7, I0 = 0, V0 = 1, tmax = 30, n=0, dU = 0, dI = 1, dV = 2, b = 2e-7, g = 1, p = 5, C0 = 2, dC = 0.5, C50 = 1, k = 2, Emax = 1, txstart = 10, txinterval = 5)
{
  Y0 = c(U = U0, I = I0, V = V0, C = 0);  #combine initial conditions into a vector - drug starts at zero in ODE
  dt = min(0.02, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b, g=g, p=p, C0 = C0, dC=dC,C50 = C50, k = k, Emax = Emax);

  drugtimes = seq(txstart,tmax,by=txinterval) #times at which drug is administered
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = pkpdode, events = list(func = adddrug, time = drugtimes), parms=pars, atol=1e-12, rtol=1e-12);

  colnames(odeoutput) = c('Time','U','I','V','C')

  #return result as list, with element ts containing the time-series
  result = list()
  result$ts = as.data.frame(odeoutput)
  return(result)
}
