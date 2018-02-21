############################################################
##simulation of a viral infection model including components of the immune response
##written by Andreas Handel (ahandel@uga.edu), last change 2/16/18
############################################################

#function that specificies the ode model
virusandirode <- function(t, y, parms)
{
   with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {

      dUdt = n - dU*U - b*V*U
      dIdt = b*V*U - dI*I - kT*T*I
      dVdt = p*I/(1+sF*F) - dV*V - b*V*U - kA*A*V
      dFdt = pF - dF*F + V / (V + hV) * gF * (Fmax - F)
      dTdt = F * V / (F * V + hF) + gT * T
      dBdt = F * V / (F * V + hF) * gB * B
      dAdt = rA*B  - dA*A - kA*A*V

	 	  list(c(dUdt, dIdt, dVdt, dFdt, dTdt, dBdt, dAdt))
    }
   ) #close with statement
} #end function specifying the ODEs

#' Simulation of a viral infection model with an immune response
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param U0 initial number of uninfected target cells
#' @param I0 initial number of infected target cells
#' @param V0 initial number of infectious virions
#' @param B0 initial number of B cells
#' @param n rate of new uninfected cell replenishment
#' @param dU rate at which uninfected cells die
#' @param dI rate at which infected cells die
#' @param dV rate at which infectious virus is cleared
#' @param b rate at which virus infects cells
#' @param p rate at which infected cells produce virus
#' @param sF strength of innate response at reducing virus production
#' @param kA rate of virus removal by antibodies
#' @param kT rate of infected cell killing by T cells
#' @param pF rate of innate response production in absence of infection
#' @param dF rate of innate response removal in absence of infection
#' @param gF rate of innate response growth during infection
#' @param Fmax maximum level of innate response
#' @param hV strength of innate growth induction by virus
#' @param hF strength of adaptive response growth induced by innate response and virus
#' @param gB maximum growth rate of B cells
#' @param gT clonal expansion rate of T cells
#' @param rA rate of antibody production by B cells
#' @param dA rate of antibody decay
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A compartmental infection model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_virusandir()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_virusandir(V0 = 100, tmax = 100, n = 1e5, dU = 1e-2, kT=1e-7)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,"time"],result[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export


simulate_virusandir <- function(U0 = 1e5, I0 = 0, V0 = 10, B0 = 1, tmax = 20, n=0, dU = 0, dI = 1, dV = 4, b = 1e-5, p = 1e3,sF=1e-2,kA=1e-5,kT=1e-5,pF=1,dF=1,gF=1,Fmax=1e3,hV=1e-6,hF=1e-5,gB=1,gT=0.5,rA=10,dA=0.2)
{
  #combine initial conditions into a vector
  #some initial conditions are set to fixed values and can't be adjusted in the app
  Y0 = c(U = U0, I = I0, V = V0, F=pF/dF, T=0, B=B0, A=0);
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b,p=p,sF=sF,kA=kA,kT=kT,pF=pF,dF=dF,gF=gF,Fmax=Fmax,hV=hV,hF=hF,gB=gB,gT=gT,rA=rA,dA=dA);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = virusandirode, parms=pars, atol=1e-12, rtol=1e-12);

  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(odeoutput)
}
