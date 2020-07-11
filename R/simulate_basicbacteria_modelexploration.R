#' Simulation to illustrate parameter scan of the basic bacteria model
#'#'
#' @description This function simulates the simple bacteria model ODE for a range of parameters.
#' The function returns a data frame containing the parameter that has been varied and the outcomes (see details).
#'
#' @param B : Starting value for bacteria : numeric
#' @param I : Starting value for immune response : numeric
#' @param g : Maximum rate of bacteria growth : numeric
#' @param Bmax : Bacteria carrying capacity : numeric
#' @param dB : Bacteria death rate : numeric
#' @param k : Bacteria kill rate : numeric
#' @param r : Immune response growth rate : numeric
#' @param dI : Immune response decay rate : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Times for which result is returned : numeric
#' @param samples : Number of values to run between pmin and pmax : numeric
#' @param parmin : Lower value for varied parameter : numeric
#' @param parmax : Upper value for varied parameter : numeric
#' @param samplepar : Name of parameter to be varied : character
#' @param pardist : spacing of parameter values, can be either 'lin' or 'log' : character
#' @return The function returns the output as a list,
#' list element 'dat' contains the data frame with results of interest.
#' The first column is called xvals and contains the values of the
#' parameter that has been varied as specified by 'samplepar'.
#' The remaining columns contain peak and steady state values of bacteria and immune response,
#' Bpeak, Ipeak, Bsteady and Isteady.
#' A final boolean variable 'steady' is returned for each simulation.
#' It is TRUE if the simulation reached steady state, otherwise FALSE.
#' @details ##this code illustrates how to do analyze a simple model.
#' A simple 2 compartment ODE model (the simple bacteria model introduced in the app of that name)
#' is simulated for different parameter values.
#' This function runs the simple bacterial infection model for a range of parameters.
#' The user can specify which parameter is sampled, and
#' the simulation returns for each parameter sample the peak and final value for B and I.
#' Also returned is the varied parameter and an indicator if steady state was reached.
#' @section Notes: The parameter dt only determines for which times the solution is returned,
#' it is not the internal time step. The latter is set automatically by the ODE solver.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' \dontrun{res <- simulate_basicbacteria_modelexploration()}
#' # To choose parameter values other than the standard one, specify them, like such:
#' res <- simulate_basicbacteria_modelexploration(samples=5, samplepar='dI', parmin=1, parmax=10)
#' # You should then use the simulation result returned from the function, like this:
#' plot(res$dat[,"xvals"],res$data[,"Bpeak"],xlab='Parameter values',ylab='Peak Bacteria',type='l')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model.
#' @author Andreas Handel
#' @export


simulate_basicbacteria_modelexploration <- function(B = 100, I = 10, g=2, Bmax=1e5, dB=1, k=1e-4, r=1e-4, dI=2, tstart = 0, tfinal = 300, dt = 0.1, samples = 10, parmin=2, parmax=10, samplepar='g',  pardist = 'lin')
  {


    #initialize vectors that will contain the outcomes of interest
    Bpeak=rep(0,samples)
    Ipeak=rep(0,samples)
    Bsteady=rep(0,samples)
    Isteady=rep(0,samples)

    #create values for the parameter of interest to sample over
    #do equal spacing in log space
    if (pardist == 'lin') {parvec=seq(parmin,parmax,length=samples)}
    if (pardist == 'log') {parvec=10^seq(log10(parmin),log10(parmax),length=samples)}


    steady = rep(TRUE,samples) #indicates if steady state has not been reached
    for (n in 1:samples)
    {
        #replace value of parameter we want to vary
        if (samplepar == 'g') {g = parvec[n]}
        if (samplepar == 'Bmax') {Bmax = parvec[n]}
        if (samplepar == 'dB') {dB = parvec[n]}
        if (samplepar == 'k') {k = parvec[n]}
        if (samplepar == 'r') {r = parvec[n]}
        if (samplepar == 'dI') {dI = parvec[n]}


        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeout <- simulate_basicbacteria_ode(B = B, I = I, g = g, Bmax = Bmax, dB = dB, k = k, r = r, dI = dI, tstart = tstart, tfinal = tfinal, dt = dt)

        timeseries = odeout$ts

        Bpeak[n]=max(timeseries[,"B"]); #get the peak for B
        Ipeak[n]=max(timeseries[,"I"]);
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

    #final list structure containing all results that are returned
    result = list()
    dat = data.frame(xvals = parvec, Bpeak = Bpeak, Ipeak=Ipeak, Bsteady = Bsteady, Isteady = Isteady, steady = steady)
    result$dat = dat

    return(result)
}
