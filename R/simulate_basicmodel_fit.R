#' Fitting a simple viral infection models to influenza data
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system.
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param X : initial level of immune response : numeric
#' @param n : rate of uninfected cell production : numeric
#' @param dU : rate at which uninfected cells die : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param g : unit conversion factor : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param plow : lower bound for p : numeric
#' @param phigh : upper bound for p : numeric
#' @param psim : rate at which infected cells produce virus for simulated data : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param bsim : rate at which virus infects cells for simulated data : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param dVlow : lower bound for virus clearance rate : numeric
#' @param dVhigh : upper bound for virus clearance rate : numeric
#' @param dVsim : rate at which infectious virus is cleared for simulated data : numeric
#' @param usesimdata : set to TRUE if simulated data should be fitted, FALSE otherwise : logical
#' @param noise : noise to be added to simulated data : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @param solvertype : the type of solver/optimizer to use (1-3) : numeric
#' @return The function returns a list containing as elements the best fit time series data frame, the best fit parameters,
#' the data and the final SSR
#' @details A simple compartmental ODE model mimicking acute viral infection
#' is fitted to data.
#' Data can either be real or created by running the model with known parameters and using the simulated data to
#' determine if the model parameters can be identified.
#' The fitting is done using solvers/optimizers from the nloptr package (which is a wrapper for the nlopt library).
#' The package provides access to a large number of solvers.
#' Here, we only implement 3 solvers, namely 1 = NLOPT_LN_COBYLA, 2 = NLOPT_LN_NELDERMEAD, 3 = NLOPT_LN_SBPLX
#' For details on what those optimizers are and how they work, see the nlopt/nloptr documentation.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_basicmodel_fit()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_basicmodel_fit(iter = 5)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select
#' @importFrom nloptr nloptr
#' @export


simulate_basicmodel_fit <- function(U = 1e5, I = 0, V = 1, X = 1, n = 0, dU = 0, dI = 1, g = 1, p = 10, plow = 1e-3, phigh = 1e3,  psim = 10, b = 1e-5, blow = 1e-6, bhigh = 1e-3,  bsim = 1e-4, dV = 2, dVlow = 1e-3, dVhigh = 1e3,  dVsim = 10, usesimdata = TRUE, noise = 1e-3, iter = 100, solvertype = 1)
{

  ###################################################################
  #function that fits the ODE model to data
  ###################################################################
  basicfitfunction <- function(params, fitdata, Y0, xvals, fixedpars, fitparnames)
  {

    names(params) = fitparnames #for some reason nloptr strips names from parameters
    allpars = c(Y0,params, tfinal = max(xvals), dt = 0.1, tstart = 0, fixedpars)

    #this function catches errors
    odeout <- try(do.call(DSAIRM::simulate_basicvirus_ode, as.list(allpars)));

    #extract values for virus load at time points where data is available
    modelpred = odeout$ts[match(fitdata$xvals,odeout$ts[,"time"]),"V"];

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming.
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,modelpred)));

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    return(sum((logvirus-fitdata$outcome)^2))

  } #end function that fits the ODE model to the data


  #will contain final result
  output <- list()

  #some settings for ode solver and optimizer
  #those are hardcoded here, could in principle be rewritten to allow user to pass it into function
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine
  maxsteps = iter #number of steps/iterations for algorithm

  #load data
  #This data is from Hayden et al 1996 JAMA
  #We only use some of the data here
  filename = system.file("extdata", "hayden96data.csv", package = "DSAIRM")
  alldata = utils::read.csv(filename)
  fitdata =  subset(alldata, Condition == 'notx', select=c("DaysPI", "LogVirusLoad")) #only fit some of the data
  colnames(fitdata) = c("xvals",'outcome')


  Y0 = c(U = U, I = I, V = V);  #combine initial conditions into a vector
  xvals = seq(0, max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #if we want to fit simulated data
  if (usesimdata == 1)
  {

    #combining fixed parameters and to be estimated parameters into a vector
    modelpars = c(n=n,dU=dU,dI=dI,dV=dVsim,b = bsim,p=psim,g=g);

    allpars = c(Y0,tfinal = max(fitdata$xvals), tstart = 0, dt = 0.1, modelpars)
    #simulate model with known parameters to get artifitial data
    #not sure why R needs it in such a weird form
    #but supplying vector of values to function directly doesn't work
    odeout <- do.call(DSAIRM::simulate_basicvirus_ode, as.list(allpars))
    simres = odeout$ts

    #extract values for virus load at time points where data is available
    simdata = data.frame(simres[match(fitdata$xvals,simres[,"time"]),])
    simdata$simres = log10(simdata$V)
    simdata = subset(simdata, select=c('time', 'simres'))
    colnames(simdata) = c('xvals','outcome')
    fitdata$outcome = simdata$outcome + noise*stats::runif(length(simdata$outcome),-1,1)*simdata$outcome
  }


  #combining fixed parameters and to be estimated parameters into a vector
  fixedpars = c(n=n,dU=dU,dI=dI,g=g)

  par_ini = as.numeric(c(p, b, dV))
  lb = as.numeric(c(plow, blow, dVlow))
  ub = as.numeric(c(phigh, bhigh, dVhigh))
  fitparnames = c('p', 'b', 'dV')

  if (solvertype == 1) {algname = "NLOPT_LN_COBYLA"}
  if (solvertype == 2) {algname = "NLOPT_LN_NELDERMEAD"}
  if (solvertype == 3) {algname = "NLOPT_LN_SBPLX"}

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=basicfitfunction,lb=lb,ub=ub,opts=list("algorithm"=algname,xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fixedpars=fixedpars,fitparnames=fitparnames)


  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters
  modelpars = c(params,fixedpars)

  allpars = c(Y0,modelpars,tfinal = max(fitdata$xvals))

  #doe one final run of the ODE to get a time-series to report back
  odeout <- do.call(simulate_basicvirus_ode, as.list(allpars))
  simres = odeout$ts
  #extract values for virus load at time points where data is available
  modelpred = simres[match(fitdata$xvals,simres[,"time"]),"V"];

  logvirus=c(log10(pmax(1e-10,modelpred)));
  ssrfinal=(sum((logvirus-fitdata$outcome)^2))

  #list structure that contains all output
  output$timeseries = odeout$ts
  output$bestpars = params
  output$SSR = ssrfinal

  output$data = fitdata

  #The output produced by the fitting routine
  return(output)
}
