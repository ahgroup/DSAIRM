#' Simulation of a viral infection model with an immune response
#'
#' @description This function runs a simulation of a compartment model
#' which tracks uninfected and infected cells, virus, innate immune response, T-cells, B-cells and antibodies.
#' The model is implemented as set of ordinary differential equations using the deSolve package.
#' @details A compartmental infection model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param T : initial number of T cells : numeric
#' @param B : initial number of B cells : numeric
#' @param A : initial number of antibodies : numeric
#' @param n : rate of new uninfected cell replenishment : numeric
#' @param dU : rate at which uninfected cells die : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param sF : strength of innate response at reducing virus production : numeric
#' @param kA : rate of virus removal by antibodies : numeric
#' @param kT : rate of infected cell killing by T cells : numeric
#' @param pF : rate of innate response production in absence of infection : numeric
#' @param dF : rate of innate response removal in absence of infection : numeric
#' @param gF : rate of innate response growth during infection : numeric
#' @param Fmax : maximum level of innate response : numeric
#' @param hV : innate growth saturation constant : numeric
#' @param hF : B-cell growth saturation constant : numeric
#' @param gB : maximum growth rate of B cells : numeric
#' @param gT : T-cell induction rate : numeric
#' @param rT : T-cell expansion rate : numeric
#' @param rA : rate of antibody production by B cells : numeric
#' @param dA : rate of antibody decay : numeric
#' @param tstart : start time of simulation : numeric
#' @param tfinal : final time of simulation : numeric
#' @param dt : times for which result is returned : numeric
#' @return A list. The list has only one element, called ts.
#' ts contains the time-series of the simulation.
#' The 1st column of ts is time, the other columns are the model variables.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_virusandir_ode()
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_virusandir_ode(V = 100, tfinal = 50, n = 1e5, dU = 1e-2, kT=1e-7)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export


simulate_virusandir_ode <- function(U = 1e5, I = 0, V = 10, T = 0, B = 0, A = 0, n=0, dU = 0, dI = 1, dV = 4, b = 1e-5, p = 1e3,sF=1e-2,kA=1e-5,kT=1e-5,pF=1,dF=1,gF=1,Fmax=1e3,hV=1e-6,hF=1e-5,gB=1,gT=1e-4,rT=0.5,rA=10,dA=0.2, tstart = 0, tfinal = 30, dt = 0.05)
{

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
        dTdt = F * V * gT + rT * T
        dBdt = F * V / (F * V + hF) * gB * B
        dAdt = rA*B  - dA*A - kA*A*V

        list(c(dUdt, dIdt, dVdt, dFdt, dTdt, dBdt, dAdt))
      }
    ) #close with statement
  } #end function specifying the ODEs


  #combine initial conditions into a vector
  #some initial conditions are set to fixed values and can't be adjusted in the app
  Y0 = c(U = U, I = I, V = V, F=pF/dF, T=T, B=B, A=A);
  timevec = seq(tstart, tfinal, by = dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b,p=p,sF=sF,kA=kA,kT=kT,pF=pF,dF=dF,gF=gF,Fmax=Fmax,hV=hV,hF=hF,gB=gB,gT=gT,rT=rT,rA=rA,dA=dA);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = virusandirode, parms=pars, atol=1e-12, rtol=1e-12);

  #return result as list, with element ts containing the time-series
  result = list()
  result$ts = as.data.frame(odeoutput)
  return(result)
}
