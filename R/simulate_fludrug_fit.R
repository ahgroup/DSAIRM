#' Fitting a simple viral infection model with 2 types of drug mechanisms to influenza data
#'
#' @description This function fits the simulate_virusandtx_ode model,
#' which is a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system in the presence of drug treatment.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#'
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param plow : lower bound for virus production rate : numeric
#' @param phigh : upper bound for virus production rate : numeric
#' @param g : unit conversion factor : numeric
#' @param glow : lower bound for unit conversion factor : numeric
#' @param ghigh : upper bound for unit conversion factor : numeric
#' @param e : drug efficacy (between 0-1) : numeric
#' @param fitmodel : fitting model 1 or 2 : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @return The function returns a list containing the best fit timeseries,
#' the best fit parameters, the data and the AICc for the model.
#' @details A simple compartmental ODE models describing an acute viral infection with drug treatment
#' mechanism/model 1 assumes that drug treatment reduces rate of new virus production.
#' mechanism/model 2 assumes  that drug treatment reduces rate of new cell infection.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_fludrug_fit()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_fludrug_fit(iter = 5, fitmodel = 1)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select
#' @importFrom nloptr nloptr
#' @export

simulate_fludrug_fit <- function(U = 1e5, I = 0, V = 1, dI = 2, dV = 4, b = 1e-2, blow = 1e-5, bhigh = 1e1, p = 1e-2,  plow = 1e-5, phigh = 1e1, g = 1, glow = 0, ghigh = 1e3, e = 0.5, fitmodel = 1, iter = 500)
{

  ###################################################################
  #function that fits the ODE model to data
  ###################################################################
  fitfunction <- function(params, fitdata, Y0,  fixedpars, fitparnames, txtimes)
  {
    names(params) = fitparnames #for some reason nloptr strips names from parameters
    #call ode-solver lsoda to integrate ODEs

    SSR = 0
    #run ODE model for 3 different drug treatment scenarios (early/late/none)
    for (n in 1:3)
    {
      allpars = c(Y0,params, tfinal = max(fitdata[[n]]$xvals), dt = 0.1, tstart = 0, n = 0, dU = 0, txstart = txtimes[n]/24)
      odeout <- try(do.call(DSAIRM::simulate_virusandtx_ode, as.list(allpars)));
      #extract values for virus load at time points where data is available
      modelpred = odeout$ts[match(fitdata[[n]]$xvals,odeout$ts[,"time"]),"V"];

      #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
      #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming.
      #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming
      logvirus=c(log10(pmax(1e-10,modelpred)));

      #since the data is censored,
      #set model prediction to LOD if it is below LOD
      #this means we do not penalize model predictions below LOD
      logvirus[(fitdata[[n]]$outcome<=LOD & (fitdata[[n]]$outcome-logvirus)>0)] = LOD

      #return the objective function, the sum of squares,
      #which is being minimized by the optimizer
      SSR = SSR + (sum((logvirus-fitdata[[n]]$outcome)^2))

    }
    return(SSR)
  } #end function that fits the ODE model to the data


  ############################################################
  #the main function, which calls the fit function
  ############################################################

  #some settings for ode solver and optimizer
  #those are hardcoded here, could in principle be rewritten to allow user to pass it into function
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine
  maxsteps = iter #number of steps/iterations for algorithm

  #load data
  #This data is from Hayden et al 1996 JAMA
  #see help('hayden96flu') for more details
  #re-organize data and convert to days
  txtimes  = c(29,50,200) #conver to units of day, which is what the model and data are in
  txscenarios = c('early','late','none')
  fitdata = list()
  for (nn in 1:3)
  {
    nowdata = subset(hayden96flu, txtime == txtimes[nn], select=c("HoursPI", "LogVirusLoad"))
    nowdata$txscenario = txscenarios[nn]
    colnames(nowdata) = c("xvals",'outcome','txscenario')
    nowdata$xvals = nowdata$xvals / 24
    fitdata[[nn]] = nowdata
  }
  LOD = hayden96flu$LOD[1] #limit of detection, log scale

  # we fit either f or e in the model
  if (fitmodel == 1)
  {
    #parameters to be fit
    fitpars = c(b=b, g=g, p=p, e=e)
    fitparnames = names(fitpars)
    lb = as.numeric(c(blow, glow, plow, 0))
    ub = as.numeric(c(bhigh, ghigh, phigh, 1))
    #combining fixed parameters into a parameter vector
    fixedpars = c(dI=dI,dV=dV,f=0,fitmodel=fitmodel);
  }
  if (fitmodel == 2)
  {
    #parameters to be fit - input parameter e is model parameter f now
    fitpars = c(b=b, g=g, p=p, f=e)
    fitparnames = names(fitpars)
    lb = as.numeric(c(blow, glow, plow, 0))
    ub = as.numeric(c(bhigh, ghigh, phigh, 1))
    #combining fixed parameters into a parameter vector
    fixedpars = c(dI=dI,dV=dV,e=0,fitmodel=fitmodel);
  }

  Y0 = c(U = U, I = I, V = V)
  set.seed(123) #for reproducibility, since some solvers might use random numbers during optimization
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=fitpars, eval_f=fitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_NELDERMEAD",xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, fixedpars=fixedpars,fitparnames=fitparnames,txtimes=txtimes)

  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters
  modelpars = c(params,fixedpars)

  #compute sum of square residuals (SSR) for final solution
  SSR = 0
  #run best fitting ODE model for 3 different drug treatment scenarios (early/late/none)
  allode = NULL

  for (n in 1:3)
  {
    allpars = c(Y0,params, tfinal = max(fitdata[[n]]$xvals), dt = 0.1, tstart = 0, n = 0, dU = 0, txstart = txtimes[n]/24)
    odeout <- try(do.call(DSAIRM::simulate_virusandtx_ode, as.list(allpars)));
    #combine all time-series, add variable labeling treatment scenario
    #extract values for virus load at time points where data is available
    modelpred = odeout$ts[match(fitdata[[n]]$xvals,odeout$ts[,"time"]),"V"];
    logvirus=c(log10(pmax(1e-10,modelpred)));
    logvirus[(fitdata[[n]]$outcome<=LOD & (fitdata[[n]]$outcome-logvirus)>0)] = LOD
    SSR = SSR + (sum((logvirus-fitdata[[n]]$outcome)^2))
    # combine all into a data frame
    ts = odeout$ts
    colnames(ts) = c('time',paste('U',txscenarios[n],sep='_'),paste('I',txscenarios[n],sep='_'),paste('V',txscenarios[n],sep='_'))
    if (n==1) {allode  = ts}
    else {allode = cbind(allode,ts[,-1])}
  }
  ssrfinal=SSR

  #compute AICc
  N=nrow(hayden96flu) #number of datapoints
  K=length(fitpars); #fitted parameters for model
  AICc= N * log(ssrfinal/N) + 2*(K+1)+(2*(K+1)*(K+2))/(N-K)

  #list structure that contains all output
  result = list()
  result$ts = allode
  result$bestpars = params
  result$AICc = AICc
  result$SSR = ssrfinal

  #return the data not on a log scale for consistency
  #also as array, with scenario as column
  alldata = NULL
  for (nn in 1:3)
  {
    fitdata[[nn]]$outcome = 10^fitdata[[nn]]$outcome
    fitdata[[nn]]$varnames = paste('Vdata',txscenarios[nn],sep='_')
    fitdata[[nn]]$txscenario = NULL
    alldata = rbind(alldata,fitdata[[nn]])
  }
  colnames(alldata) = c("xvals",'yvals','varnames')
  result$data = alldata

  #The output produced by the fitting routine
  return(result)
}
