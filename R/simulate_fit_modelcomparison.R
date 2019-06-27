#' Fitting 2 simple viral infection models to influenza data
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system in the presence of drug treatment.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param X : initial level of immune response : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param g : unit conversion factor : numeric
#' @param k : rate of killing of infected cells by T-cells (model 1) or virus by Ab (model 2) : numeric
#' @param a : activation of T-cells (model 1) or growth of antibodies (model 2) : numeric
#' @param alow : lower bound for activation rate : numeric
#' @param ahigh : upper bound for activation rate : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param r : rate of T-cell expansion (model 1) : numeric
#' @param rlow : lower bound for expansion rate : numeric
#' @param rhigh : upper bound for expansion rate : numeric
#' @param dX : rate at which antibodies decay (model 2) : numeric
#' @param dXlow : lower bound for decay rate : numeric
#' @param dXhigh : upper bound for decay rate : numeric
#' @param fitmodel : fitting model 1 or 2 : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @return The function returns a list containing the best fit timeseries,
#' the best fit parameters, the data and the AICc for the model.
#' @details Two simple compartmental ODE models mimicking acute viral infection
#' with T-cells (model 1) or antibodies (model 2) are fitted to data.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_fit_modelcomparison()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_fit_modelcomparison(iter = 5, fitmodel = 1)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select
#' @importFrom nloptr nloptr

#' @export

simulate_fit_modelcomparison <- function(U = 1e5, I = 0, V = 1, X = 1, dI = 1, dV = 2, g = 0, p = 10, k = 1e-6, a = 1e-5, alow = 1e-6, ahigh = 1e-4, b = 1e-5, blow = 1e-6, bhigh = 1e-3, r = 1,  rlow = 0.1, rhigh = 2, dX = 1, dXlow = 0.1, dXhigh = 10, fitmodel = 1, iter = 100)
{


  ##all sub-functions are specified first

  #########################################
  #ode equations for model 1
  model1ode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {

        dUdt = -b*V*U
        dIdt = b*V*U - dI*I - k*X*I
        dVdt = p*I - dV*V - g*b*V*U
        dXdt = a*V + r*X

        list(c(dUdt, dIdt, dVdt,dXdt))
      }
    ) #close with statement
  } #end function specifying the ODEs

  #########################################
  #ode equations for model 2
  model2ode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {

        dUdt = -b*V*U
        dIdt = b*V*U - dI*I
        dVdt = p*I - dV*V - k*X*V - g*b*V*U
        dXdt = a*V*X - dX*X

        list(c(dUdt, dIdt, dVdt, dXdt))
      }
    ) #close with statement
  } #end function specifying the ODEs



  ###################################################################
  #function that fits the ODE model to data
  ###################################################################
  modelcompfitfunction <- function(params, fitdata, Y0, xvals, fitmodel, fixedpars, fitparnames)
  {

    names(params) = fitparnames #for some reason nloptr strips names from parameters
    modelpars = c(params,fixedpars)
    #call ode-solver lsoda to integrate ODEs

    if (fitmodel == 1)
    {
      odeout <- try(deSolve::ode(y = Y0, times = xvals, func = model1ode, parms=modelpars, atol=1e-8, rtol=1e-8));
    }
    if (fitmodel == 2)
    {
      odeout <- try(deSolve::ode(y = Y0, times = xvals, func = model2ode, parms=modelpars, atol=1e-8, rtol=1e-8));
    }
    colnames(odeout) = c('xvals','U','I','V','X')

    #extract values for virus load at time points where data is available
    modelpred = odeout[match(fitdata$xvals,odeout[,"xvals"]),"V"];

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming.
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,modelpred)));

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    return(sum((logvirus-fitdata$outcome)^2))

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
  #We only use some of the data here
  filename = system.file("extdata", "hayden96data.csv", package = "DSAIRM")
  alldata=read.csv(filename)
  fitdata =  subset(alldata, Condition == 'notx', select=c("DaysPI", "LogVirusLoad"))
  colnames(fitdata) = c("xvals",'outcome')

  Y0 = c(U = U, I = I, V = V, X = X);  #combine initial conditions into a vector
  xvals = seq(0, max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining fixed parameters into a parameter vector
  fixedpars = c(dI=dI,dV=dV,p=p,k=k, g=g);

  if (fitmodel == 1)
  {
    par_ini = as.numeric(c(a=a, r=r, b=b))
    lb = as.numeric(c(alow, rlow, blow))
    ub = as.numeric(c(ahigh, rhigh, bhigh))
    fitparnames = c('a','r','b')
  }

  if (fitmodel == 2)
  {
    par_ini = as.numeric(c(a=a, dX=dX, b=b))
    lb = as.numeric(c(alow, dXlow, blow))
    ub = as.numeric(c(ahigh, dXhigh, bhigh))
    fitparnames = c('a','dX','b')
  }

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=modelcompfitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_NELDERMEAD",xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fitmodel=fitmodel, fixedpars=fixedpars,fitparnames=fitparnames)


  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters
  modelpars = c(params,fixedpars)


  #time-series for best fit model
  if (fitmodel == 1)
  {
    odeout <- try(deSolve::ode(y = Y0, times = xvals, func = model1ode, parms=modelpars, atol=1e-8, rtol=1e-8));
  }
  if (fitmodel == 2)
  {
    odeout <- try(deSolve::ode(y = Y0, times = xvals, func = model2ode, parms=modelpars, atol=1e-8, rtol=1e-8));
  }
  colnames(odeout) = c('xvals','U','I','V','X')

  #compute sum of square residuals (SSR) for initial guess and final solution
  modelpred = odeout[match(fitdata$xvals,odeout[,"xvals"]),"V"];

  logvirus=c(log10(pmax(1e-10,modelpred)));
  ssrfinal=(sum((logvirus-fitdata$outcome)^2))

  #compute AICc
  N=length(fitdata$outcome) #number of datapoints
  K=length(par_ini); #fitted parameters for model
  AICc= N * log(ssrfinal/N) + 2*(K+1)+(2*(K+1)*(K+2))/(N-K)


  #list structure that contains all output
  result = list()
  result$ts = odeout
  result$bestpars = params
  result$AICc = AICc
  result$SSR = ssrfinal


  #return the data not on a log scale for consistency
  fitdata$outcome = 10^fitdata$outcome
  result$data = fitdata

  #The output produced by the fitting routine
  return(result)
}
