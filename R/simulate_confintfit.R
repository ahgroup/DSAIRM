##################################################################################
##fitting influenza virus load data to a simple ODE model
##model used is the one in "simulate_basicvirus.R"
##illustrates bootstrapping to compute CI
##written by Andreas Handel, ahandel@uga.edu, last change 5/25/18

##all sub-functions are specified first


###################################################################
#function that fits the ODE model to data
###################################################################
cifitfunction <- function(params, mydata, Y0, timevec, fixedpars, fitparnames)
{

   names(params) = fitparnames #for some reason nloptr strips names from parameters
   allpars = c(Y0,max(timevec),params,fixedpars)

    #this function catches errors
    odeout <- try(do.call(simulate_basicvirus, as.list(allpars)));

    #extract values for virus load at time points where data is available
    modelpred = odeout[match(mydata$time,odeout[,"time"]),"V"];

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming.
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,modelpred)));

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    return(sum((logvirus-mydata$outcome)^2))

} #end function that fits the ODE model to the data

###################################################################
#function to do the bootstraps
###################################################################
#this extra function is needed for the bootstrap routine.
#it basically calls the optimization routine and returns the best fit parameter values (stored in finalparams) to the bootstrap function
#the bootstrap routine is called in the main program below
bootfct <- function(mydata,indi, par_ini, lb, ub, Y0, timevec, fixedpars, fitparnames, maxsteps)
{
  mydata = mydata[indi,] #get samples
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=cifitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_NELDERMEAD",xtol_rel=1e-10,maxeval=maxsteps,print_level=0), mydata=mydata, Y0 = Y0, timevec = timevec, fixedpars=fixedpars,fitparnames=fitparnames)
  #extract best fit parameter values and from the result returned by the optimizer
  finalparams=bestfit$solution;
  return(finalparams)
}



############################################################
#the main part, which calls the fit function
############################################################

#' Fitting a simple viral infection models to influenza data
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system
#' @param U0 initial number of uninfected target cells
#' @param I0 initial number of infected target cells
#' @param V0 initial number of infectious virions
#' @param X0 initial level of immune response
#' @param n rate of uninfected cell production
#' @param dU rate at which uninfected cells die
#' @param p rate at which infected cells produce virus
#' @param dI rate at which infected cells die
#' @param g unit conversion factor
#' @param b rate at which virus infects cells
#' @param blow lower bound for infection rate
#' @param bhigh upper bound for infection rate
#' @param dV rate at which infectious virus is cleared
#' @param dVlow lower bound for virus clearance rate
#' @param dVhigh upper bound for virus clearance rate
#' @param iter max number of steps to be taken by optimizer
#' @param nsample number of samples for conf int determination
#' @return The function returns a list containing the best fit timeseries, the best fit parameters, and AICc for the model
#' @details a simple compartmental ODE model mimicking acute viral infection
#' is fitted to data
#' Data can either be real or created by running the model with known parameters and using the simulated data to
#' determine if the model parameters can be identified
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message
#' @examples
#' # To run the code with default parameters just call this function
#' \dontrun{result <- simulate_confintfit()}
#' @seealso See the shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @export

simulate_confintfit <- function(U0 = 1e5, I0 = 0, V0 = 1, X0 = 1, n = 0, dU = 0, dI = 1, p = 10, g = 1, b = 1e-5, blow = 1e-6, bhigh = 1e-3,  dV = 2, dVlow = 1e-3, dVhigh = 1e3, iter = 100, nsample = 10)
{

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
  mydata = dplyr::filter(alldata, Condition == 'notx')
  mydata = dplyr::rename(mydata, time = DaysPI, outcome = LogVirusLoad)
  mydata =  dplyr::select(mydata, time, outcome)

  Y0 = c(U0 = U0, I0 = I0, V0 = V0);  #combine initial conditions into a vector
  timevec = seq(0, max(mydata$time), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining fixed parameters and to be estimated parameters into a vector
  fixedpars = c(n=n,dU=dU,dI=dI,p=p,g=g);

  par_ini = as.numeric(c(dV, b))
  lb = as.numeric(c(dVlow, blow))
  ub = as.numeric(c(dVhigh, bhigh))
  fitparnames = c('dV','b')

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=cifitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_NELDERMEAD",xtol_rel=1e-10,maxeval=maxsteps,print_level=0), mydata=mydata, Y0 = Y0, timevec = timevec, fixedpars=fixedpars,fitparnames=fitparnames)

  #compute confidence intervals using bootstrap sampling
  bssample <- boot::boot(data=mydata,statistic=bootfct,R=nsample, par_ini = par_ini, lb = lb, ub = ub, Y0 = Y0, timevec = timevec, fixedpars = fixedpars, fitparnames = fitparnames, maxsteps = maxsteps)

  #calculate the 95% confidence intervals for parameters
  #ci.b=boot.ci(bssample,index=1,type = "perc")
  #print(sprintf('best-fit and 95CI for b: (%e,%e,%e)',ci.b$perc[4],ci.b$t0,ci.b$perc[5]));
  #ci.V0=boot.ci(bssample,index=3,type = "perc")
  #print(sprintf('best-fit and 95CI for V0: (%e,%e,%e)',ci.V0$perc[4],ci.V0$t0,ci.V0$perc[5]));

  browser()

  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters
  modelpars = c(params,fixedpars)

  allpars = c(Y0,tmax=max(mydata$time),modelpars)

  odeout <- do.call(simulate_basicvirus, as.list(allpars))



  #compute sum of square residuals (SSR) for initial guess and final solution
  modelpred = odeout[match(mydata$time,odeout[,"time"]),"V"];

  logvirus=c(log10(pmax(1e-10,modelpred)));
  ssrfinal=(sum((logvirus-mydata$outcome)^2))

  #compute AICc
  N=length(mydata$outcome) #number of datapoints
  K=length(par_ini); #fitted parameters for model 1
  AICc=N*log(ssrfinal/N)+2*(K+1)+(2*(K+1)*(K+2))/(N-K)

  #list structure that contains all output
  output$timeseries = odeout
  output$bestpars = params
  output$AICc = AICc
  output$data = mydata
  output$SSR = ssrfinal

  #The output produced by the fitting routine
  return(output)
}
