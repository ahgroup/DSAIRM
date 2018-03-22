############################################################
##simulating a stochastic version of the basic virus model
##written by Andreas Handel, ahandel@uga.edu, last change: 3/18/18
############################################################

#this specifies the rates used by the adapativetau routine
stochasticratefunc <- function(y, parms, t)
{
    with(as.list(c(y, parms)),
         {
            #specify each rate/transition/reaction that can happen in the system
             rates=c(  n,
                       dU * U,
                       b * V * U,
                       dI * I,
                       p * I,
                       dV * V
             ) #end specification of each rate/transition/reaction
             return(rates)
         })
} #end function specifying rates used by adaptivetau


#' Stochastic simulation of a virus infection model
#'
#' @description  Simulation of a stochastic simple virus infection model with the following
#'   compartments: Uninfected cells (U), Infected cells (I), free virus (V)
#'
#' @param U0 initial number of uninfected cells
#' @param I0 initial number of infected cells
#' @param V0 initial number of virions
#' @param tmax maximum simulation time, units depend on choice of units for parameters
#' @param n rate of uninfected cell production
#' @param dU rate of uninfected cell death (the inverse is the average lifespan)
#' @param b rate of cell infection
#' @param dI rate of infected cell death (the inverse is the average lifespan)
#' @param p rate of new virus production
#' @param dV rate of virus removal (the inverse is the average lifespan)
#' @return The function returns the time series of the simulated model as
#'   matrix, with one column per compartment/variable. The first column is time.
#' @details A compartmental, stochastic model with several states/compartments is
#'   simulated.  Units of time depend on the time units chosen for model parameters.
#'   The simulation runs as a stochastic model using the adaptive-tau algorithm as implemented
#'   by ssa.adaptivetau in the adpativetau package. See the manual of this
#'   package for more details. The function returns the time series of the
#'   simulated disease as output matrix, with one column per
#'   compartment/variable. The first column is time.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters
#' result <- simulate_basicvirus_stochastic()
#' # To choose parameter values other than the standard one, specify them e.g. like this
#' result <- simulate_basicvirus_stochastic(V0 = 100,  tmax = 200, dI = 2)
#' #You can display or further process the result, e.g. like this
#' plot(result[,'time'],result[,'V'],xlab='Time',ylab='Virus',type='l')
#' print(paste('Peak number of virus:',max(result[,'V'])))
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the adaptivetau
#' package for details on the stochastic algorithm.
#' @author Andreas Handel
#' @export




simulate_basicvirus_stochastic <- function(U0 = 1e4, I0 = 0, V0=10, tmax = 100, n = 0, dU = 0, b = 1e-3, dI = 1, p = 2, dV = 2)
{
    Y0 = c(U = U0, I = I0,  V = V0);  #combine initial conditions into a vector
    dt = tmax / 1000; #time step for which to get results back
    timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

    #combining parameters into a parameter vector
    pars = c(n = n, dU = dU, b = b,  dI = dI, p = p, dV = dV);

    #specify for each reaction/rate/transition how the different variables change
    #needs to be in exactly the same order as the rates listed in the rate function
    transitions = list(c(U = +1), #production of uninfected cells
                       c(U = -1), #removal of uninfected cells
                       c(U = -1, V = -1, I = +1), #cell infection
                       c(I = -1), #deaths of I
                       c(V = +1), #virus production
                       c(V = -1) #virus removal
    ) #end list of transitions

    #this line runs the simulation
    output = adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = stochasticratefunc, params = pars, tf = tmax)

    #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
    return(output)
}
