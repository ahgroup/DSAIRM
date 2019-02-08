#' Simulation of a viral infection model with immune response
#' The simulation illustrates different alternative models.
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param F : initial level of innate response : numeric
#' @param A : initial level of adaptive response : numeric
#' @param n : rate of uninfected cell production : numeric
#' @param dU : rate of natural death of uninfected cells : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param pF : rate of innate response production in absence of infection : numeric
#' @param dF : rate of innate response removal in absence of infection : numeric
#' @param f1 : growth of innate response alternative 1 : numeric
#' @param f2 : growth of innate response alternative 2 : numeric
#' @param f3 : growth of innate response alternative 3 : numeric
#' @param Fmax : maximum level of innate response in alternative 1 : numeric
#' @param sV : saturation of innate response growth in alternative 2 and 3 : numeric
#' @param k1 : action of innate response alternative 1 : numeric
#' @param k2 : action of innate response alternative 2 : numeric
#' @param k3 : action of innate response alternative 3 : numeric
#' @param a1 : growth of adaptive response alternative 1 : numeric
#' @param a2 : growth of adaptive response alternative 2 : numeric
#' @param a3 : growth of adaptive response alternative 3 : numeric
#' @param hV : saturation of adaptive response growth in alternative 2 and 3 : numeric
#' @param k4 : action of adaptive response alternative 1 : numeric
#' @param k5 : action of adaptive response alternative 2 : numeric
#' @param k6 : action of adaptive response alternative 3 : numeric
#' @param sA : saturation of adaptive response killing for alternative action 2 : numeric
#' @param dA : adaptive immune response decay : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Times for which result is returned : numeric
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A compartmental infection model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_modelvariants_ode()
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_modelvariants_ode(V = 100, k1 = 0 , k2 = 0, k3 = 1e-4)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"time"],result$ts[,"V"],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export


simulate_modelvariants_ode <- function(U = 1e5, I = 0, V = 10, F=0, A=0, n = 0, dU = 0, dI = 1, dV = 4, b = 1e-5, p = 1e3,pF=1,dF=1, f1 = 1e-4, f2 = 0, f3 = 0, Fmax = 1e3, sV = 1e-10, k1 = 1e-3, k2 = 0, k3 = 0, a1 = 1e3, a2 = 0, a3 = 0, hV = 1e-10, k4 = 1e-3, k5 = 0, k6 = 0, sA = 1e-10, dA = 0.1, tstart = 0, tfinal = 30, dt = 0.05)
{


  ##############################
  #function that specificies the ode model
  modelvariantode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {

        dUdt = n - dU*U - b*V*U - k1*F*U
        dIdt = b*V*U - dI*I - k2*F*I - k4*A*I - k5*A*I/(A+sA)
        dVdt = p*I/(1+k3*F) - dV*V - b*V*U - k6*A*V
        dFdt = pF - dF*F + f1 *V*(Fmax - F) + f2 * V / (V+sV)*F + f3 *V*I / (V*I+sV)*F
        dAdt = a1*F*A + a2*V/(V+hV)*F + a3 * F * V / (F * V + hV) * A - dA * A

        list(c(dUdt, dIdt, dVdt, dFdt, dAdt))
      }
    ) #close with statement
  } #end function specifying the ODEs

  ##############################
  #main part of code
  #combine initial conditions into a vector
  #some initial conditions are set to fixed values and can't be adjusted in the app
  Y0 = c(U = U, I = I, V = V, F = F,  A = A);
  timevec = seq(tstart, tfinal, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(n=n,dU=dU,dI=dI,dV=dV,b=b,p=p,pF=pF,dF=dF,f1=f1,f2=f2,f3=f3,Fmax=Fmax,sV=sV,k1=k1,k2=k2,k3=k3,a1=a1,a2=a2,a3=a3,hV=hV,k4=k4,k5=k5,k6=k6,sA=sA, dA = dA);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = modelvariantode, parms=pars, atol=1e-12, rtol=1e-12);

  result = list()
  result$ts = as.data.frame(odeoutput)

  #return result as list, with element ts containing the time-series
  return(result)
}
