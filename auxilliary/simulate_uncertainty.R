############################################################
##simulation of the simple bacterial infection model
##this code does uncertainty analysis
##written by Andreas Handel (ahandel@uga.edu), last change 12/6/17
############################################################

#' Simulation to do an uncertainty analysis of a simple model
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
#' @param n rate of new uinfected cell replenishment
#' @param dU rate at which uninfected cells die
#' @param dI rate at which infected cells die
#' @param dV rate at which infectious virus is cleared
#' @param b rate at which virus infects cells
#' @param p rate at which infected cells produce virus
#'
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A simple compartmental model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_basicvirus()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_basicvirus(V0 = 100, tmax = 100, n = 1e5, dU = 1e-2)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,1],result[,4],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_uncertainty <- function(B0 = 10, I0 = 1, tmax = 30, Bmax=1e6, dB=1e-1, k=1e-7, r=1e-3, dIlow=1, dIhigh=2, gmean=0.5, gvar=0.1, samplemax = 10)
  {

    #this creates a LHS with samplemax samples for 2 parameters, drawn from a uniform distribution between zero and one
    lhssample=lhssample=randomLHS(samplemax,2);

    #transforming parameter dI to uniform between dIlow and dIhigh
    dIvec=qunif(lhssample[,2],min = dIlow, max=dIhigh)

    #transforming parameter g to a gamma distribution with mean gmean and variance gvar
    gvec=qgamma(lhssample[,2], shape = gmean^2/gvar, scale = gvar/gmean);

    Bpeak=rep(0,samplemax); #initialize vector that will contain the solution

    for (nsample in 1:samplemax)
    {
        #values for sampled parameters
        dI=dIvec[nsample];
        g=gvec[nsample];

        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeoutput <- simulate_basicbacteria(B0 = B0, I0 = I0, tmax = tmax, g=g, Bmax=Bmax, dB=dB, k=k, r=r, dI=dI)

        Bpeak[nsample]=max(odeoutput[,"B"]); #get the peak for B
    }
    return(Bpeak)
}
