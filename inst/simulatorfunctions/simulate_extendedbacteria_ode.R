#' Extended Bacteria model - ODE
#'
#' @description A bacteria infection model with 3 compartments, implemented as set of ODEs.
#' The model tracks bacteria, and one compartment each for innate and adaptive immune response.
#' The processes modeled are bacteria growth, death and killing by the immune response,
#' and growth and decay of each immune response component.
#'
#' @details
#' The model tracks bacteria, and one compartment each for innate and adaptive immune response.
#' The processes modeled are bacteria growth, death and killing by the immune response,
#' and growth and decay of each immune response component.
#' The model is implemented as a set of ordinary differential equations (ODE) using the deSolve package.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param B : starting value for bacteria : numeric
#' @param I : starting value for innate immune response : numeric
#' @param A : starting value for adaptive immune response : numeric
#' @param g : maximum rate of bacteria growth : numeric
#' @param Bmax : bacteria carrying capacity : numeric
#' @param dB : bacteria death rate : numeric
#' @param kI : rate of bacteria killing by innate response : numeric
#' @param kA : rate of bacteria killing by adaptive response : numeric
#' @param rI : innate response growth rate : numeric
#' @param Imax : innate response carrying capacity : numeric
#' @param dI : innate response decay rate : numeric
#' @param rA : adaptive response growth rate : numeric
#' @param h : adaptive response half-growth : numeric
#' @param dA : adaptive response decay rate : numeric
#' @param tstart : start time of simulation : numeric
#' @param tfinal : final time of simulation : numeric
#' @param dt : times for which result is returned : numeric
#' @return The function returns the output as a list.
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}.
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.
#' @examples
#' # To run the simulation with default parameters:
#' result <- simulate_extendedbacteria_ode()
#' # To run the simulation with different parameter or starting values,
#' # supply the ones you want to change.
#' # all other parameters will be kept at their default values shown in the function call above
#' result <- simulate_extendedbacteria_ode(B = 100, g = 0.5, dI = 2)
#' @section Notes: The parameter dt only determines the times the solution is returned and plotted,
#' it is not the internal time step for the differential equation solver.
#' The latter is set automatically by the ODE solver.
#' @section Warning: This function does not perform any error checking.
#' So if you try to do something nonsensical (e.g. have negative values for parameters),
#' the code will likely abort with an error message.
#' @export

simulate_extendedbacteria_ode <- function(B = 100, I = 1, A = 1,
                                          g = 1, Bmax = 1e+05, dB = 0.5, kI = 1e-4, kA = 1e-4,
                                          rI = 1, Imax = 1e5, dI = 1,
                                          rA = 1, h = 1e3, dA = 0.1,
                                          tstart = 0, tfinal = 100, dt = 0.01)
{

  #Block of ODE equations for deSolve
  extendedbacteria_ode_fct <- function(t, y, parms)
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name
    #StartODES
    #Bacteria : bacteria growth : bacteria death : immune response killing :
    dB = g*B*(1-B/Bmax) - dB*B - kI*B*I - kA*B*A
    #Innate Immune Response : innate response growth : innate response decay :
    dI = rI*B*(1-I/Imax) - dI*I
    #Adaptive Immune Response : adaptive response growth : adaptive response decay :
    #extra max statement ensures no negative values on log scale
    dA = rA*A*log(max(I,1))/(log(max(I,1)) + h) - dA*A
    #EndODES
    list(c(dB,dI,dA))
  } ) } #close with statement, end ODE function code block



  #Main function code block
  timevec=seq(tstart,tfinal,by=dt)
  vars = c(B = B, I = I, A = A)
  pars = c(g = g, Bmax = Bmax, dB =dB, kI = kI, kA = kA, rI = rI, Imax = Imax, dI = dI, rA = rA, h = h, dA = dA)
  odeout = deSolve::ode(y = vars, parms = pars, times = timevec,  func = extendedbacteria_ode_fct, atol = 1e-12, rtol = 1e-12, method = "vode")
  result <- list()
  result$ts <- as.data.frame(odeout)
  return(result)
}
