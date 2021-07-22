#' Stochastic simulation of a compartmental acute virus infection model
#' with treatment and drug resistant strain
#'
#' @description  Simulation of a stochastic model with the following compartments:
#' Uninfected target cells (U), Infected with wild-type/sensitive and untreated (Is),
#' infected with resistant (Ir), wild-type virus (Vs), resistant virus (Vr).
#'
#' @param U : initial number of target cells : numeric
#' @param Is : initial number of wild-type infected cells : numeric
#' @param Ir : initial number of resistant infected cells : numeric
#' @param Vs : initial number of wild-type virus : numeric
#' @param Vr : initial number of resistant virus : numeric
#' @param b : level/rate of infection of cells : numeric
#' @param dI : rate of infected cell death : numeric
#' @param e : efficacy of drug : numeric
#' @param m : fraction of resistant mutants created : numeric
#' @param p : virus production rate : numeric
#' @param c : virus removal rate : numeric
#' @param f : fitness cost of resistant virus : numeric
#' @param rngseed : seed for random number generator to allow reproducibility : numeric
#' @param tfinal : maximum simulation time : numeric
#' @return A list. The list has only one element called ts.
#' ts contains the time-series of the simulation.
#' The 1st column of ts is Time, the other columns are the model variables.
#' @details A compartmental ID model with several states/compartments
#' is simulated as a stochastic model using the adaptive tau algorithm as implemented by ssa.adaptivetau
#' in the adpativetau package. See the manual of this package for more details.
#' The function returns the time series of the simulated disease as output matrix,
#' with one column per compartment/variable. The first column is time.
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have I0 > PopSize or any negative values or fractions > 1),
#' the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_drugresistance_stochastic()
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_drugresistance_stochastic(tfinal = 100, e = 0.5)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"time"],result$ts[,"Vs"],xlab='Time',ylab='Uninfected cells',type='l')
#' @references See the manual for the adaptivetau package for details on the algorithm.
#'             The implemented model is loosely based on: Handel et al 2007 PLoS Comp Bio
#'            "Neuraminidase Inhibitor Resistance in Influenza: Assessing the Danger of Its
#'            Generation and Spread"
#' @author Andreas Handel
#' @export



simulate_drugresistance_stochastic <- function(U = 1E5, Is = 0, Ir = 0, Vs = 10, Vr =0,
                                               b = 1e-5, dI = 1, e = 0, m = 1e-3, p = 1e2,
                                               c = 4, f = 0.1, rngseed = 100, tfinal = 30)
{


  #this function specifies the rates used by the adapativetau routine
  #needs to be before main function so it's clear where description belongs to
  evolutionratefunc <- function(y, parms, t)
  {
    with(as.list(c(y, parms)),
         {

           #specify each rate/transition/reaction that can happen in the system
           rates=c(  b*U*Vs,
                     b*U*Vr,
                     dI*Is,
                     dI*Ir,
                     (1-e)*(1-m)*p*Is,
                     c*Vs,
                     (1-e)*m*p*Is+(1-f)*p*Ir,
                     c*Vr
           ) #end specification of each rate/transition/reaction
           return(rates)
         })
  } #end function specifying rates used by adaptivetau


  #main code
  Y0 = c(U = U, Is = Is,  Ir = Ir, Vs = Vs, Vr = Vr);  #combine initial conditions into a vector

  #combining parameters into a parameter vector
  pars = c(b = b, dI = dI, e = e, m = m, p = p, c = c, f = f);

  #specify for each reaction/rate/transition how the different variables change
  #needs to be in exactly the same order as the rates listed in the rate function
  transitions = list(c(U = -1, Is = +1), #infection of U to Is
                     c(U = -1, Ir = +1), #infection of U to Ir
                     c(Is = -1), #Is cell death
                     c(Ir = -1), #Ir cell death
                     c(Vs = +1), #susceptible virus produced
                     c(Vs = -1), #susceptible virus removed
                     c(Vr = +1), #resistant virus produced
                     c(Vr = -1) #resistant virus removed
  ) #end list of transitions


  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  set.seed(rngseed) # to allow reproducibility
  output = adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = evolutionratefunc, params = pars, tf = tfinal)

  #return result as list, with element ts containing the time-series
  result = list()
  result$ts = as.data.frame(output)
  return(result)
}
