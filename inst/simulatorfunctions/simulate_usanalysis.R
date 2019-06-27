#' Simulation to illustrate uncertainty and sensitivity analysis
#'
#' @description This function performs uncertainty and sensitivity analysis
#' using the simple, continuous-time basic bacteria model.
#' @details A simple 2 compartment ODE model (the simple bacteria model introduced in the app of that name)
#' is simulated for different parameter values.
#' The user provides ranges for the initial conditions and parameter values and the number of samples.
#' The function does Latin Hypercube Sampling (LHS) of the parameters
#' and runs the basic bacteria ODE model for each sample.
#' Distribution for all parameters is assumed to be uniform between the min and max values.
#' The only exception is the bacteria growth parameter,
#' which is assumed to be gamma distributed with the specified mean and variance.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param Bmin : lower bound for initial bacteria numbers : numeric
#' @param Bmax : upper bound for initial bacteria numbers : numeric
#' @param Imin : lower bound for initial immune response : numeric
#' @param Imax : upper bound for initial immune response : numeric
#' @param Bmaxmin : lower bound for maximum bacteria load : numeric
#' @param Bmaxmax : upper bound for maximum bacteria load : numeric
#' @param dBmin : lower bound for bacteria death rate : numeric
#' @param dBmax : upper bound for bacteria death rate : numeric
#' @param kmin : lower bound for immune response kill rate : numeric
#' @param kmax : upper bound for immune response kill rate : numeric
#' @param rmin : lower bound for immune response growth rate : numeric
#' @param rmax : upper bound for immune response growth rate : numeric
#' @param dImin : lower bound for immune response death rate : numeric
#' @param dImax : upper bound for immune response death rate : numeric
#' @param gmean : mean for bacteria growth rate : numeric
#' @param gvar : variance for bacteria growth rate : numeric
#' @param samples : number of LHS samples to run : numeric
#' @param rngseed : seed for random number generator : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : times for which result is returned : numeric
#' @return The function returns the output as a list.
#' The list element 'dat' contains a data frame.
#' The simulation returns for each parameter sample the peak and final value for B and I.
#' Also returned are all parameter values as individual columns
#' and an indicator stating if steady state was reached.
#' A final variable 'steady' is returned for each simulation.
#' It is TRUE if the simulation did reach steady state, otherwise FALSE.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' \dontrun{result <- simulate_usanalysis()}
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_usanalysis(dImin = 0.1, dImax = 10, samples = 5, tfinal = 50)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$dat[,"dI"],result$dat[,"Bpeak"],xlab='values for d',ylab='Peak Bacteria',type='l')
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model.
#' @author Andreas Handel
#' @export


simulate_usanalysis <- function(Bmin = 1, Bmax = 10, Imin = 1, Imax = 10, Bmaxmin=1e5, Bmaxmax=1e6, dBmin=1e-1, dBmax = 1e-1, kmin=1e-7, kmax=1e-7, rmin=1e-3, rmax=1e-3, dImin=1, dImax=2, gmean=0.5, gvar=0.1, samples = 10, rngseed = 100, tstart = 0, tfinal = 200, dt = 0.1)
  {

    #this creates a LHS with the specified number of samples for all 8 parameters
    #drawn from a uniform distribution between zero and one
    #if a parameter should be kept fixed, simply set min and max to the same value
    set.seed(rngseed)
    lhssample=lhs::randomLHS(samples,8);

    #transforming parameters to be  uniform between their low and high values
    Bvec = stats::qunif(lhssample[,1],min = Bmin, max = Bmax)
    Ivec = stats::qunif(lhssample[,2],min = Imin, max= Imax)
    Bmaxvec = stats::qunif(lhssample[,3],min = Bmaxmin, max = Bmaxmax)
    dBvec   = stats::qunif(lhssample[,4],min = dBmin, max = dBmax)
    kvec = stats::qunif(lhssample[,5],min = kmin, max = kmax)
    rvec = stats::qunif(lhssample[,6],min = rmin, max = rmax)
    dIvec = stats::qunif(lhssample[,7],min = dImin, max = dImax)

    #transforming parameter g to a gamma distribution with mean gmean and variance gvar
    #this is just to illustrate how different assumptions of parameter distributions can be implemented
    gvec = stats::qgamma(lhssample[,8], shape = gmean^2/gvar, scale = gvar/gmean);

    Bpeak=rep(0,samples) #initialize vectors that will contain the solution
    Bsteady=rep(0,samples)
    Isteady=rep(0,samples)

    steady = rep(TRUE,samples) #indicates if steady state has not been reached
    for (n in 1:samples)
    {
        #values for sampled parameters
        B=Bvec[n]
        I=Ivec[n]
        Bmax=Bmaxvec[n]
        dB=dBvec[n];
        k=kvec[n];
        r=rvec[n];
        dI=dIvec[n];
        g=gvec[n];

        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeout <- simulate_basicbacteria_ode(B = B, I = I, g = g, Bmax = Bmax, dB = dB, k = k, r = r, dI = dI, tstart = tstart, tfinal = tfinal, dt = dt)

        timeseries = odeout$ts

        Bpeak[n]=max(timeseries[,"B"]); #get the peak for B
        Bsteady[n] = utils::tail(timeseries[,"B"],1)
        Isteady[n] = utils::tail(timeseries[,"I"],1)


        #a quick check to make sure the system is at steady state,
        #i.e. the value for B at the final time is not more than
        #1% different than B several time steps earlier
        vl=nrow(timeseries);
        if ((abs(timeseries[vl,"B"]-timeseries[vl-10,"B"])/timeseries[vl,"B"])>1e-2)
        {
          steady[n] = FALSE
        }
    }

    simresults = data.frame(Bpeak = Bpeak, Bsteady = Bsteady, Isteady = Isteady, B = Bvec, I = Ivec, Bmax = Bmaxvec, dB = dBvec, k = kvec, r = rvec, dI = dIvec, g = gvec, steady = steady)

    result = list()
    result$dat = simresults
    return(result)
}
