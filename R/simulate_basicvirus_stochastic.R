#' Stochastic simulation of a compartmental acute virus infection model
#'
#' @description  Simulation of a stochastic model with the following compartments:
#' Uninfected target cells (U), Infected cells (I), virus (V).
#'
#' @param U initial number of target cells. Needs to be an integer.
#' @param I initial number of wild-type infected cells. Needs to be an integer.
#' @param V initial number of resistant virus. Needs to be an integer.
#' @param n rate of uninfected cell production
#' @param dU rate of uninfected cell removal
#' @param b level/rate of infection of cells
#' @param dI rate of infected cell death
#' @param p virus production rate
#' @param dV virus removal rate
#' @param rngseed seed for random number generator to allow reproducibility
#' @param tstart : Start time of simulation
#' @param tfinal : Final time of simulation
#' @param dt : this is ignored since time step is determined automatically by simulator.
#' It is only provided to match the ODE and discrete solvers for easier calling.
#' @return A list. The list has only one element called ts.
#' ts contains the time-series of the simulation.
#' The 1st column of ts is Time, the other columns are the model variables.
#' @details A compartmental ID model with several states/compartments
#' is simulated as a stochastic model using the adaptive tau algorithm as implemented by ssa.adaptivetau()
#' in the adaptivetau package. See the manual of this package for more details.
#' The function returns the time series of the simulated disease as output matrix,
#' with one column per compartment/variable. The first column is time.
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have I0 > PopSize or any negative values or fractions > 1),
#' the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_basicvirus_stochastic()
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_basicvirus_stochastic(U = 1e3, dI = 0.1)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l')
#' @references See the manual for the adaptivetau package for details on the algorithm.
#'             See the app corresponding to this function in DSAIDE for more details on the model.
#' @author Andreas Handel
#' @export

simulate_basicvirus_stochastic <- function(U = 1E4, I = 0, V = 5, n = 0, dU = 0, b = 1e-4, dI = 1, p = 1e1, dV = 2, rngseed = 100, tstart = 0, tfinal = 30, dt = 0.05)
{

  #this specifies the rates used by the adapativetau routine
  #needs to be before main function so it's clear where description belongs to
  stochasticratefunc <- function(y, parms, t)
  {
    with(as.list(c(y, parms)),
         {

           #specify each rate/transition/reaction that can happen in the system
           rates=c(  n,
                     dU*U,
                     b*U*V,
                     dI*I,
                     p*I,
                     dV*V
           ) #end specification of each rate/transition/reaction
           return(rates)
         })
  } #end function specifying rates used by adaptivetau

  Y0 = c(U = U, I = I, V = V);  #combine initial conditions into a vector

  #combining parameters into a parameter vector
  pars = c(n = n, dU = dU, b = b, dI = dI, p = p, dV = dV);

  #specify for each reaction/rate/transition how the different variables change
  #needs to be in exactly the same order as the rates listed in the rate function
  transitions = list( c(U = +1),
                      c(U = -1),
                     c(U = -1, I = +1), #infection of U to I
                     c(I = -1), # cell death
                     c(V = +1), # virus produced
                     c(V = -1) # virus removed
  ) #end list of transitions

  #this line runs the simulation,
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd+ columns are the model variables
  set.seed(rngseed) # to allow reproducibility
  output = adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = stochasticratefunc, params = pars, tf = tfinal, tl.params = list(maxtau = 0.1))

  #return result as list, with element ts containing the time-series
  result = list()
  result$ts = as.data.frame(output)
  return(result)
}
