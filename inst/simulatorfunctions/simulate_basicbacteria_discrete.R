#' Basic Bacteria model - discrete
#'
#' @description A basic bacteria infection model with 2 compartments, implemented as discrete time simulation.
#' The model tracks bacteria and an immune response dynamics.
#' The processes modeled are bacteria growth, death and killing by the immune response,
#' and immune response activation and decay.
#'
#' @details The model includes bacteria and an immune response. The processes are bacteria growth,
#' death and killing by the immune response, and immune response activation and decay.
#' This is a predator-prey type model.
#' The model is implemented as a set of discrete-time, deterministic equations,
#' coded as a for-loop.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param B : starting value for bacteria : numeric
#' @param I : starting value for immune response : numeric
#' @param g : maximum rate of bacteria growth : numeric
#' @param Bmax : bacteria carrying capacity : numeric
#' @param dB : bacteria death rate : numeric
#' @param k : rate of bacteria killing by immune reesponse : numeric
#' @param r : immune response growth rate : numeric
#' @param dI : immune response decay rate : numeric
#' @param tstart : start time of simulation : numeric
#' @param tfinal : final time of simulation : numeric
#' @param dt : time step : numeric
#' @return The function returns the output as a list.
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}.
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.
#' @examples
#' # To run the simulation with default parameters:
#' result <- simulate_basicbacteria_discrete()
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @export

simulate_basicbacteria_discrete <- function(B = 10, I = 1, g = 1, Bmax = 1e+06, dB = 0.1, k = 1e-07, r = 0.001, dI = 1, tstart = 0, tfinal = 30, dt = 0.01)
{
      tvec = seq(tstart,tfinal,by=dt)
      ts = data.frame(cbind(tvec, matrix(0,nrow=length(tvec),ncol=2)))
      colnames(ts) = c("time","B","I")
      ct=1 #a counter to index array
      for (t in tvec)
      {
        ts[ct,] = c(t,B,I)
        Bp = B + dt*(+g*B*(1-B/Bmax) -dB*B -k*B*I)
        Ip = I + dt*(+r*B*I -dI*I)
        B = Bp
        I = Ip
        ct = ct + 1
      } #finish loop
  result <- list()
  result$ts <- ts
  return(result)
}
