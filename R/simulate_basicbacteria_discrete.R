#' Basic Bacteria model
#'
#' A basic bacteria infection model with 2 compartments
#'
#' @details The model includes bacteria and an immune response. The processes are bacteria growth, death and killing, and immune response activation and decay. This is a predator-prey type model.
#' This code is based on a dynamical systems model created by the modelbuilder package.
#' The model is implemented here as a set of discrete-time, deterministic equations,
#' coded as a simple for-loop.
#' @param B : starting value for Bacteria
#' @param I : starting value for Immune Response
#' @param g : maximum rate of bacteria growth
#' @param Bmax : bacteria carrying capacity
#' @param dB : bacteria death rate
#' @param k : bacteria kill rate
#' @param r : immune response growth rate
#' @param dI : immune response decay rate
#' @param tstart : Start time of simulation
#' @param tfinal : Final time of simulation
#' @param dt : Time step
#' @return The function returns the output as a list.
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}.
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.
#' @examples
#' # To run the simulation with default parameters:
#' result <- simulate_basicbacteria_discrete()
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @section Model Author: Andreas Handel
#' @section Model creation date: 2018-12-21
#' @export

simulate_basicbacteria_discrete <- function(B = 10, I = 1, g = 1, Bmax = 1e+06, dB = 0.1, k = 1e-07, r = 0.001, dI = 1, tstart = 0, tfinal = 30, dt = 0.05)
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
