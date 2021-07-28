#' Fit a simple viral infection model and compute confidence intervals
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system.
#' @details A simple compartmental ODE model mimicking acute viral infection
#' is fitted to data.
#' Confidence intervals are computed by simple bootstrapping of the data
#' using the boot R package. Confidence intervals are computed using the
#' percentage method in boot.ci. See the boot package for more information.
#' This code is part of the DSAIRM R package.
#' For additional model details, see the corresponding app in the DSAIRM package.
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param n : rate of uninfected cell production : numeric
#' @param dU : rate at which uninfected cells die : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param g : unit conversion factor : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param dVlow : lower bound for virus clearance rate : numeric
#' @param dVhigh : upper bound for virus clearance rate : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @param nsample : number of samples for conf int determination : numeric
#' @param rngseed : seed for random number generator to allow reproducibility : numeric
#' @param parscale : 1 for linear, 2 for log space parameter fitting : numeric
#' @return The function returns a list containing the best fit time series, the best fit parameters for
#' the data, the final SSR, and the bootstrapped 95 percent confidence intervals.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_confint_fit()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_confint_fit(iter = 5, nsample = 5)
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select
#' @importFrom nloptr nloptr
#' @export

simulate_confint_fit <- function(U = 1e5, I = 0, V = 10,
                                 n = 0, dU = 0, dI = 2,
                                 p = 0.01, g = 0,
                                 b = 1e-2, blow = 1e-6, bhigh = 1e3,
                                 dV = 2, dVlow = 1e-3, dVhigh = 1e3,
                                 iter = 20, nsample = 10, rngseed = 100, parscale = 1)
{

  ###################################################################
  #specifying sub-functions first, main function code is below
  ###################################################################

  ###################################################################
  #function that fits the ODE model to data
  ###################################################################
  cifitfunction <- function(params, fitdata, Y0, xvals, fixedpars, fitparnames, parscale, LOD)
  {

    if (parscale == 2) {params = exp(params)} #for simulation, need to move parameters back to original scale
    names(params) = fitparnames #for some reason nloptr strips names from parameters
    allpars = c(Y0,params, tfinal = max(xvals), dt = 0.1, tstart = 0, fixedpars)

    #this function catches errors
    odeout <- try(do.call(simulate_Basic_Virus_Model_ode, as.list(allpars)));

    simres = odeout$ts

    #extract values for virus load at time points where data is available
    modelpred = simres[match(fitdata$xvals,simres[,"time"]),"V"];

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming.
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming
    logvirus=c(log10(pmax(1e-10,modelpred)));

    #since the data is censored,
    #set model prediction to LOD if it is below LOD
    #this means we do not penalize model predictions below LOD
    logvirus[(fitdata$outcome<=LOD & (fitdata$outcome-logvirus)>0)] = LOD

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    return(sum((logvirus-fitdata$outcome)^2))

  } #end function that fits the ODE model to the data

  ###################################################################
  #function to do the bootstraps
  ###################################################################
  #this extra function is needed for the bootstrap routine.
  #it basically calls the optimization routine and returns the best fit parameter values (stored in finalparams) to the bootstrap function
  #the bootstrap routine is called in the main program below
  bootfct <- function(fitdata,indi, par_ini, lb, ub, Y0, xvals, fixedpars, fitparnames, maxsteps, parscale)
  {
    fitdata = fitdata[indi,] #get samples
    bestfit = nloptr::nloptr(x0=par_ini, eval_f=cifitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_NELDERMEAD",xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fixedpars=fixedpars,fitparnames=fitparnames, parscale =parscale, LOD = LOD)
    #extract best fit parameter values and from the result returned by the optimizer
    finalparams=bestfit$solution;
    return(finalparams)
  }

  ###################################################################
  #code for main function
  ###################################################################

  set.seed(rngseed) # to allow reproducibility

  #some settings for ode solver and optimizer
  #those are hardcoded here, could in principle be rewritten to allow user to pass it into function
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine
  maxsteps = iter #number of steps/iterations for algorithm

  #load data
  #This data is from Hayden et al 1996 JAMA
  #We only use the data for the no-drug condition here
  LOD = hayden96flu$LOD[1] #limit of detection, log scale
  fitdata =  subset(hayden96flu, txtime == 200, select=c("HoursPI", "LogVirusLoad")) #only fit some of the data
  colnames(fitdata) = c("xvals",'outcome')
  #convert to days
  fitdata$xvals = fitdata$xvals / 24

  Y0 = c(U = U, I = I, V = V);  #combine initial conditions into a vector
  xvals = seq(0, max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining fixed parameters and to be estimated parameters into a vector
  fixedpars = c(n=n,dU=dU,dI=dI,p=p,g=g);

  par_ini = as.numeric(c(b, dV))
  lb = as.numeric(c(blow, dVlow))
  ub = as.numeric(c(bhigh, dVhigh))
  fitparnames = c('b', 'dV')

  if (parscale == 2) #fitting parameters log scale
  {
    par_ini = log(par_ini)
    lb = log(lb)
    ub = log(ub)
  }

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=cifitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_NELDERMEAD",xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fixedpars=fixedpars,fitparnames=fitparnames,parscale = parscale,LOD=LOD)

  #extract best fit parameter values and from the result returned by the optimizer

  params = bestfit$solution
  if (parscale == 2) #fitting parameters log scale
  {
    params = exp(bestfit$solution)
  }

  names(params) = fitparnames #for some reason nloptr strips names from parameters
  #run model to get trajectory for plotting
  modelpars = c(params,fixedpars)

  allpars = c(Y0,tfinal = max(fitdata$xvals), tstart = 0, dt = 0.1, modelpars)

  odeout <- do.call(simulate_Basic_Virus_Model_ode, as.list(allpars))

  simres = odeout$ts

  #compute confidence intervals using bootstrap sampling
  bssample <- boot::boot(data=fitdata,statistic=bootfct,R=nsample, par_ini = bestfit$solution, lb = lb, ub = ub, Y0 = Y0, xvals = xvals, fixedpars = fixedpars, fitparnames = fitparnames, maxsteps = maxsteps,parscale = parscale)


  #calculate the 95% confidence intervals for parameters
  ci.b=boot::boot.ci(bssample,index=1,type = "perc")
  ci.dV=boot::boot.ci(bssample,index=2, type = "perc")
  ciall = c(blow = ci.b$perc[4], bhigh = ci.b$perc[5], dVlow = ci.dV$perc[4], dVhigh = ci.dV$perc[5])
  if (parscale == 2) #fitting parameters log scale
  {
    ciall = exp(ciall)
  }

  #compute SSR for final fit. See comments inside of fitting function for explanations.
  modelpred = odeout$ts[match(fitdata$xvals,odeout$ts[,"time"]),"V"];
  logvirus=c(log10(pmax(1e-10,modelpred)));
  logvirus[(fitdata$outcome<=LOD & (fitdata$outcome-logvirus)>0)] = LOD
  ssrfinal=(sum((logvirus-fitdata$outcome)^2))

  #list structure that contains all output
  result = list()
  result$ts = simres
  result$bestpars = params
  result$SSR = ssrfinal
  result$confint = ciall

  #return the data not on a log scale for consistency
  fitdata$outcome = 10^fitdata$outcome
  fitdata$varnames = 'V_data'
  colnames(fitdata) = c("xvals",'yvals','varnames')
  result$data = fitdata

  #The output produced by the fitting routine
  return(result)
}
