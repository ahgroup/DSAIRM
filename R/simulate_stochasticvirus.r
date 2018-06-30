############################################################
##a stochastic model for acute virus infection
##written by Andreas Handel (ahandel@uga.edu), last change 5/1/18
############################################################

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



#' Stochastic simulation of a compartmental acute virus infection model
#'
#' @description  Simulation of a stochastic model with the following compartments:
#' Uninfected target cells (U), Infected cells (I), virus (V)
#'
#' @param U0 initial number of taget cells. Needs to be an integer.
#' @param I0 initial number of wild-type infected cells. Needs to be an integer.
#' @param V0 initial number of resistant virus. Needs to be an integer.
#' @param n rate of uninfected cell production
#' @param dU rate of uninfected cell removal
#' @param b level/rate of infection of cells
#' @param dI rate of infected cell death
#' @param p virus production rate
#' @param dV virus removal rate
#' @param rngseed seed for random number generator to allow reproducibility
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return This function returns the simulation result as obtained from a call
#'   to the adaptivetau integrator
#' @details A compartmental ID model with several states/compartments
#' is simulated as a stochastic model using the adaptive tau algorithm as implemented by ssa.adaptivetau
#' in the adpativetau package. See the manual of this package for more details.
#' The function returns the time series of the simulated disease as output matrix,
#' with one column per compartment/variable. The first column is time.
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have I0 > PopSize or any negative values or fractions > 1),
#' the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_stochasticvirus()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_stochasticvirus(tmax = 20, dI = 0.5)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,1],result[,2],xlab='Time',ylab='Uninfected cells',type='l')
#' @references See the manual for the adaptivetau package for details on the algorithm.
#'             See the app corresponding to this function in DSAIDE for more details on the model
#' @author Andreas Handel
#' @export


#x = simulate_stochasticvirus(U0 = 1E4, I0 = 0, V0 = 5, tmax = 30, n = 0, dU = 0, b = 1e-4, dI = 1, p = 1e1, dV = 2, rngseed = 123)

simulate_stochasticvirus <- function(U0 = 1E4, I0 = 0, V0 = 5, tmax = 30, n = 0, dU = 0, b = 1e-4, dI = 1, p = 1e1, dV = 2, rngseed = 100)
{
  Y0 = c(U = U0, I = I0, V = V0);  #combine initial conditions into a vector

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
  output = adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = stochasticratefunc, params = pars, tf = tmax, tl.params = list(maxtau = 0.1))

  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(output)
}
