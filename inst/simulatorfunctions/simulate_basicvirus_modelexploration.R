#' Simulation to illustrate parameter scan of the basic virus model
#'#'
#' @description This function simulates the basic virus model ODE for a range of parameters.
#' The function returns a data frame containing the parameter that has been varied and the outcomes (see details).
#'
#' @param U : Starting value for uninfected cells : numeric
#' @param I : Starting value for infected cells : numeric
#' @param V : Starting value for virus : numeric
#' @param n : Rate of new uninfected cell replenishment : numeric
#' @param dU : Rate at which uninfected cells die : numeric
#' @param dI : Rate at which infected cells die : numeric
#' @param dV : Rate at which virus is cleared : numeric
#' @param b : Rate at which virus infects cells : numeric
#' @param p : Rate at which infected cells produce virus : numeric
#' @param g : Possible conversion factor for virus units : numeric
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
#' Upeak, Ipeak, Vpeak, Usteady, Isteady and Vsteady.
#' A final boolean variable 'steady' is returned for each simulation.
#' It is TRUE if the simulation reached steady state, otherwise FALSE.
#' @details ##this code illustrates how to do analyze a simple model.
#' A simple 3 compartment ODE model (the basic virus model introduced in the app of that name)
#' is simulated for different parameter values.
#' This function runs the model for a range of values for any one parameter,
#' while holding all other paramter values fixed.
#' The user can specify which parameter is sampled, and
#' the simulation returns for each parameter sample the peak and final value for U, I and V.
#' Also returned is the varied parameter and an indicator if steady state was reached.
#' @section Notes: The parameter dt only determines for which times the solution is returned,
#' it is not the internal time step. The latter is set automatically by the ODE solver.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' \dontrun{res <- simulate_basicvirus_modelexploration()}
#' # To choose parameter values other than the standard one, specify them, like such:
#' res <- simulate_basicvirus_modelexploration(samples=5, samplepar='dI', parmin=1, parmax=10)
#' # You should then use the simulation result returned from the function, like this:
#' plot(res$dat[,"xvals"],res$data[,"Vpeak"],xlab='Parameter values',ylab='Virus Peak',type='l')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model.
#' @author Andreas Handel
#' @export


simulate_basicvirus_modelexploration <- function(U = 1e+05, I = 0, V = 1, n = 10000, dU = 0.1, dI = 1, dV = 2, b = 2e-05, p = 5, g = 1, tstart = 0, tfinal = 100, dt = 0.1, samples = 10, parmin=1, parmax=10, samplepar='p', pardist = 'lin')
  {

    #initialize vectors that will contain the outcomes of interest
    Upeak=rep(0,samples)
    Ipeak=rep(0,samples)
    Vpeak=rep(0,samples)
    Usteady=rep(0,samples)
    Isteady=rep(0,samples)
    Vsteady=rep(0,samples)

    #create values for the parameter of interest to sample over
    #do equal spacing in log space
    if (pardist == 'lin') {parvec=seq(parmin,parmax,length=samples)}
    if (pardist == 'log') {parvec=10^seq(log10(parmin),log10(parmax),length=samples)}


    steady = rep(TRUE,samples) #indicates if steady state has not been reached
    for (nn in 1:samples)
    {
        #replace value of parameter we want to vary
        if (samplepar == 'n') {n = parvec[nn]}
        if (samplepar == 'dU') {dU = parvec[nn]}
        if (samplepar == 'dI') {dI = parvec[nn]}
        if (samplepar == 'dV') {dV = parvec[nn]}
        if (samplepar == 'b') {b = parvec[nn]}
        if (samplepar == 'p') {p = parvec[nn]}
        if (samplepar == 'g') {g = parvec[nn]}


        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed

        odeout <- simulate_basicvirus_ode(U = U, I = I, V = V, n = n, dU = dU, dI = dI, dV = dV, b = b, p = p, g = g, tstart = tstart, tfinal = tfinal, dt = dt)

        timeseries = odeout$ts

        #get peak and steady state for variables
        Upeak[nn]=max(timeseries[,"U"]);
        Ipeak[nn]=max(timeseries[,"I"]);
        Vpeak[nn]=max(timeseries[,"V"]);
        Usteady[nn] = utils::tail(timeseries[,"U"],1)
        Isteady[nn] = utils::tail(timeseries[,"I"],1)
        Vsteady[nn] = utils::tail(timeseries[,"V"],1)

        #a quick check to make sure the system is at steady state,
        #i.e. the value for B at the final time is not more than
        #1% different than B several time steps earlier
        vl=nrow(timeseries);
        if ((abs(timeseries[vl,"U"]-timeseries[vl-10,"U"])/timeseries[vl,"U"])>1e-2)
        {
          steady[nn] = FALSE
        }
    }

    #final list structure containing all results that are returned
    result = list()
    dat = data.frame(xvals = parvec, Upeak = Upeak, Ipeak=Ipeak, Vpeak=Vpeak, Usteady = Usteady, Isteady = Isteady, Vsteady = Vsteady, steady = steady)
    result$dat = dat

    return(result)
}
