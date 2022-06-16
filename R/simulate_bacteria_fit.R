#' Fitting a simple bacteria infection models to Streptococcus pneumoniae infection data
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple bacteria infection system.

#' @param B : initial number of bacteria : numeric
#' @param I : initial number of neutrophils (immune response) : numeric
#' @param g : maximum rate of bacteria growth : numeric
#' @param glow : lower bound for g : numeric
#' @param ghigh : upper bound for g : numeric
#' @param Bmax : bacteria carrying capacity : numeric
#' @param Bmaxlow : lower bound for Bmax : numeric
#' @param Bmaxhigh : upper bound for Bmax : numeric
#' @param dB : bacteria death rate : numeric
#' @param dBlow : lower bound for dB : numeric
#' @param dBhigh : upper bound for dB : numeric
#' @param kI : rate of bacteria killing by immune response : numeric
#' @param kIlow : lower bound for k : numeric
#' @param kIhigh : upper bound for k : numeric
#' @param rI : immune response growth rate : numeric
#' @param rIlow : lower bound for r : numeric
#' @param rIhigh : upper bound for r : numeric
#' @param Imax : immune response carrying capacity : numeric
#' @param Imaxlow : lower bound for Imax : numeric
#' @param Imaxhigh : upper bound for Imax : numeric
#' @param dI : immune response decay rate : numeric
#' @param dIlow : lower bound for dI : numeric
#' @param dIhigh : upper bound for dI : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @param solvertype : the type of solver/optimizer to use (1-3) : numeric
#' @return The function returns a list containing as elements the best fit time series data frame, the best fit parameters,
#' the data and the final SSR
#' @details A simple compartmental ODE model for a bacterial infection
#' is fitted to data.
#' The fitting is done using solvers/optimizers from the nloptr package (which is a wrapper for the nlopt library).
#' The package provides access to a large number of solvers.
#' Here, we only implement 3 solvers, namely 1 = NLOPT_LN_COBYLA, 2 = NLOPT_LN_NELDERMEAD, 3 = NLOPT_LN_SBPLX
#' For details on what those optimizers are and how they work, see the nlopt/nloptr documentation.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_bacteria_fit()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_bacteria_fit(iter = 5)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select
#' @importFrom nloptr nloptr
#' @export


simulate_bacteria_fit <- function(B = 1, I = 1,
                                  g = 1e-1, glow = 1e-4, ghigh = 1e2,
                                  Bmax = 1e8, Bmaxlow = 1e4, Bmaxhigh = 1e10,
                                  dB = 1e-2, dBlow = 1e-4, dBhigh = 1e2,
                                  kI = 1e-3, kIlow = 1e-4, kIhigh = 1e2,
                                  rI = 1e-1, rIlow = 1e-4, rIhigh = 1e2,
                                  Imax = 1e4, Imaxlow = 1, Imaxhigh = 1e10,
                                  dI = 1e-1, dIlow = 1e-3, dIhigh = 1e2,
                                  iter = 10, solvertype = 1)
{


  ###################################################################
  #function that fits the ODE model to data
  ###################################################################
  fitfunction <- function(params, fitdata, Y0, xvals, fitparnames, fixedpars)
  {

    names(params) = fitparnames #for some reason nloptr strips names from parameters
    #all parameters, those being fitted and those being fixed

    allpars = c(Y0, params, fixedpars, tfinal = max(xvals), dt = 0.1, tstart = 0)

    #this function catches errors
    odeout <- try(do.call(DSAIRM::simulate_extendedbacteria_ode, as.list(allpars)));

    #extract values for bacteria load at time points where data is available
    Bvals = fitdata[fitdata$varnames=="B",]
    Bpred = odeout$ts[match(Bvals$xvals,odeout$ts[,"time"]),"B"];
    #extract values for neutrophils
    Ivals = fitdata[fitdata$varnames=="I",]
    Ipred = odeout$ts[match(Ivals$xvals,odeout$ts[,"time"]),"I"];


    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, values might become negative, leading to problems when log-transforming.
    #Therefore, we enforce a minimum value of 1e-10 before log-transforming
    logBpred = log10(pmax(1e-10,Bpred))
    logIpred = log10(pmax(1e-10,Ipred))

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    Btot = sum( ((logBpred-Bvals$yvals)/max(Bvals$yvals))^2)
    Itot = sum( ((logIpred-Ivals$yvals)/max(Ivals$yvals))^2)
    return(Btot+Itot)
  } #end function that fits the ODE model to the data


  # start main part of function

  #will contain final result
  result <- list()

  #some settings for ode solver and optimizer
  #those are hardcoded here, could in principle be rewritten to allow user to pass it into function
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine
  maxsteps = iter #number of steps/iterations for algorithm

  #load data
  fitdata =  schirm20strep
  # assign 0 entries to a small non-zero number so we can log-transform
  fitdata$value[fitdata$value == 0] = 1e-10

  #transform to log scale - change
  fitdata$value = log10(fitdata$value)
  colnames(fitdata) = c("xvals",'yvals','varnames')

  Y0 = c(B = B, I = I, A = 0);  #combine initial conditions into a vector
  xvals = seq(0, max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)


  #defining parameters to fit

  par_ini = as.numeric(c(g, Bmax, dB, kI, rI, Imax, dI))
  lb = as.numeric(c(glow, Bmaxlow, dBlow, kIlow, rIlow, Imaxlow, dIlow))
  ub = as.numeric(c(ghigh, Bmaxhigh, dBhigh, kIhigh, rIhigh, Imaxhigh, dIhigh))
  fitparnames = c('g', 'Bmax', 'dB','kI','rI','Imax','dI')

  # h at 1 to avoid divide by 0
  fixedpars = c(rA = 0, h = 1, dA = 0, kA = 0)

  if (solvertype == 1) {algname = "NLOPT_LN_COBYLA"}
  if (solvertype == 2) {algname = "NLOPT_LN_NELDERMEAD"}
  if (solvertype == 3) {algname = "NLOPT_LN_SBPLX"}

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=fitfunction,lb=lb,ub=ub,opts=list("algorithm"=algname,xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fitparnames=fitparnames,fixedpars=fixedpars)

  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters

  allpars = c(Y0,params,tfinal = max(fitdata$xvals),fixedpars)

  #doe one final run of the ODE to get a time-series to report back
  odeout <- do.call(simulate_extendedbacteria_ode, as.list(allpars))

  #extract values for bacteria load at time points where data is available
  Bvals = fitdata[fitdata$varnames=="B",]
  Bpred = odeout$ts[match(Bvals$xvals,odeout$ts[,"time"]),"B"];
  #extract values for neutrophils
  Ivals = fitdata[fitdata$varnames=="I",]
  Ipred = odeout$ts[match(Ivals$xvals,odeout$ts[,"time"]),"I"];


  #compute SSR for final fit. See comments inside of fitting function for explanations.
  logBpred = log10(pmax(1e-10,Bpred))
  logIpred = log10(pmax(1e-10,Ipred))

  #return the objective function, the sum of squares,
  #which is being minimized by the optimizer
  Btot = sum( ((logBpred-Bvals$yvals)/max(Bvals$yvals))^2)
  Itot = sum( ((logIpred-Ivals$yvals)/max(Ivals$yvals))^2)
  ssrfinal= Btot + Itot


  #list structure that contains all output
  result$ts = odeout$ts
  # since we didn't run/include the A variable, we remove it from output
  result$ts$A <- NULL
  result$bestpars = params
  result$SSR = ssrfinal

  #return the data not on a log scale for consistency
  fitdata$yvals = 10^fitdata$yvals
  #label with _data to distinguish from model
  fitdata$varnames = paste0(fitdata$varnames,"_data")
  result$data = fitdata

  #The output produced by the fitting routine
  return(result)
}
