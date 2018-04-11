############################################################
##a simple model for a simple bacteria infection model, implemented as ODE
##written by Andreas Handel (ahandel@uga.edu), last change 12/6/17
############################################################


#function that specificies the ode model
bacteriaode <- function(t, y, parms)
{
   with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {

      #these are the differential equations
      dBdt = g*B*(1-B/Bmax) - dB*B - k*B*I;
      dIdt = r*B*I / (B+s) - dI*I;

	 	  list(c(dBdt, dIdt))
    }
   ) #close with statement
} #end function specifying the ODEs


#' Simulation of a basic model with bacteria and an immune response
#' illustrating a simple within-host predator-prey model
#'
#' @description This function runs a simulation of a basic 2 compartment model
#' using a set of 2 ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param B0 initial number of bacteria
#' @param I0 initial number/strength of immune response
#' @param g rate of bacteria growth
#' @param Bmax carrying capacity for bacteria
#' @param dB death rate of bacteria
#' @param k rate at which bacteria are killed by immune response
#' @param r rate at which immune response is induced by bacteria
#' @param dI death rate of immune response
#' @param s immune response growth saturation parameter
#'
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A simple 2 compartment model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- mysimulator()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- mysimulator(B0 = 100, I0 = 10, tmax = 100, g = 10)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,1],result[,2],xlab='Time',ylab='Bacteria Number',type='l')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

mysimulator <- function(B0 = 10, I0 = 1, tmax = 30, g=1, Bmax=1e6, dB=1e-1, k=1e-7, r=1e3, dI=1, s=1E3)
{
  Y0 = c(B = B0, I = I0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (note that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(g=g,Bmax=Bmax,dB=dB,k=k,r=r,dI=dI,s=s);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = bacteriaode, parms=pars, atol=1e-12, rtol=1e-12);

  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(odeoutput)
}
