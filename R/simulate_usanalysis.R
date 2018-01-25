############################################################
##simulation of the simple bacterial infection model
##this code does uncertainty analysis
##written by Andreas Handel (ahandel@uga.edu), last change 12/6/17
############################################################



#' Simulation to do an uncertainty analysis using the simple bacteria model
#'
#'
#' @description This function runs latin hypercube sampling of parameters
#' for the simple bacteria model underlying the app by the same name
#' The user provides ranges for the initial conditions and parameter values and the number of samples.
#' The function does LHS of the ODE using an ODE solver from the deSolve package.
#' The function returns a data frame containing values for each sample and results
#'
#' @param gmean
#' @param gvar
#' @param dIlow lower bound for immune response death rate
#' @param dIhigh upper bound for immune response death rate
#' @param samples number of LHS samples to run
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output as a data frame,
#' with one column per sampled parameter and result.
#' @details A simple 2 compartment ODE model is simulated for different parameter values
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_uncertainty()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_uncertainty(dIlow = 0.1, dIhigh = 10, tmax = 100)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,"d"],result[,"Bpeak"],xlab='values for parameter d',ylab='Peak Bacteria Number',type='l')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model.
#' @author Andreas Handel
#' @export


simulate_uncertainty <- function(B0 = 10, I0 = 1, tmax = 30, Bmax=1e6, dB=1e-1, k=1e-7, r=1e-3, dIlow=1, dIhigh=2, gmean=0.5, gvar=0.1, samplemax = 10)
  {

    #this creates a LHS with samplemax samples for 2 parameters, drawn from a uniform distribution between zero and one
    lhssample=lhssample=randomLHS(samplemax,2);

    #transforming parameter dI to uniform between dIlow and dIhigh
    dIvec=qunif(lhssample[,1],min = dIlow, max=dIhigh)

    #transforming parameter g to a gamma distribution with mean gmean and variance gvar
    gvec=qgamma(lhssample[,2], shape = gmean^2/gvar, scale = gvar/gmean);


    Bpeak=rep(0,samplemax); #initialize vectors that will contain the solution
    Bsteady=rep(0,samplemax);
    Isteady=rep(0,samplemax);

    for (nsample in 1:samplemax)
    {
        #values for sampled parameters
        dI=dIvec[nsample];
        g=gvec[nsample];

        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeoutput <- simulate_basicbacteria(B0 = B0, I0 = I0, tmax = tmax, g=g, Bmax=Bmax, dB=dB, k=k, r=r, dI=dI)

        Bpeak[nsample]=max(odeoutput[,"B"]); #get the peak for B
        Bsteady[nsample]=tail(odeoutput[,"B"],1);
        Isteady[nsample]=tail(odeoutput[,"I"],1);
    }

    results = data.frame(d = dvec, g = gvec, Bpeak = Bpeak, Bsteady = Bsteady, Isteady = Isteady)

    return(results)
}
