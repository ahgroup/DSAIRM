#' PkPd Virus model
#'
#' A basic virus infection model with drug PkPd
#'
#' @description This function runs a simulation of the basic 3 compartment
#' virus infection model including the pharmacokinetics and pharmacodynamics
#' of a drug. The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' @details A simple compartmental model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#'
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param n : rate of new uninfected cell replenishment : numeric
#' @param dU : rate at which uninfected cells die : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param g : unit conversion factor : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param C0 : drug dose given at specified times : numeric
#' @param dC : drug concentration decay rate : numeric
#' @param C50 : drug concentration at which effect is half maximum : numeric
#' @param k : steepness of concentration-dependent drug effect : numeric
#' @param Emax : maximum drug efficacy (0-1) : numeric
#' @param txstart : time of drug treatment start : numeric
#' @param txinterval : time between drug doses : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Times for which result is returned : numeric
#' @return A list. The list has only one element called ts.
#' ts contains the time-series of the simulation.
#' The 1st column of ts is Time, the other columns are the model variables.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_pkpdmodel_ode()
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_pkpdmodel_ode(V = 100, txstart = 10, n = 1e5, dU = 1e-2)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_pkpdmodel_ode <- function(U = 1e5, I = 0, V = 10,
                                   n=0, dU = 0, dI = 1, dV = 2,
                                   b = 1e-5, g = 1, p = 10,
                                   C0 = 1, dC = 1, C50 = 1, k = 1, Emax = 0,
                                   txstart = 10, txinterval = 1, tstart = 0, tfinal = 20, dt = 0.01)
{

  #function that specificies the ode model
  pkpdode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {
        e=Emax*C^k/(C^k + C50) #drug efficacy, based on level of C
        dCdt = - dC*C
        dUdt = n - dU*U - b*V*U
        dIdt = b*V*U - dI*I
        dVdt = (1-e)*p*I - dV*V - g*b*V*U

        list(c(dCdt, dUdt, dIdt, dVdt))
      }
    ) #close with statement
  } #end function specifying the ODEs

  #function that specifies addition of drug at the indicated time
  adddrug <- function(t,y,parms)
  {
    y['C'] = y['C'] + parms['C0']
    return(y)
  }

  Y0 = c(C = 0, U = U, I = I, V = V);  #combine initial conditions into a vector - drug starts at zero in ODE
  timevec = seq(tstart, tfinal, by = dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b, g=g, p=p, C0 = C0, dC=dC,C50 = C50, k = k, Emax = Emax);

  drugtimes = seq(txstart, tfinal, by = txinterval) #times at which drug is administered
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = pkpdode, events = list(func = adddrug, time = drugtimes), parms=pars, atol=1e-12, rtol=1e-12);

  #return result as list, with element ts containing the time-series
  result = list()
  result$ts = as.data.frame(odeoutput)
  return(result)
}
