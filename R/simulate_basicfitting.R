##################################################################################
##fitting influenza virus load data to 2 simple ODE models
##illustrates model comparison and parameter estimation
##written by Andreas Handel, ahandel@uga.edu, last change 4/25/18



#########################################
#ode equations for model 1
model1ode <- function(t, y, parms)
{
  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {

      dUdt = -b*V*U
      dIdt = b*V*U - dI*I - k*X*I
      dVdt = p*I - dV*V
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
      dVdt = p*I - dV*V - k*X*V
      dXdt = a*V*X - dX*X

      list(c(dUdt, dIdt, dVdt, dXdt))
    }
  ) #close with statement
} #end function specifying the ODEs



###################################################################
#function that fits the ODE model to data
###################################################################
fitfunction <- function(params,data,modeltype)
{

   modeltype = params["model"]
   modelpars = params
   #call ode-solver lsoda to integrate ODEs
   if (modeltype == 1)
   {
     odeout <- try(deSolve::ode(y = Y0, times = timevec, func = model1ode, parms=modelpars, atol=1e-8, rtol=1e-8));
   }

    #extract values for virus load at time points where data is available
    modelpred = odeout[match(data$time,odeout[,"time"]),"V"];

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming.
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,virusfit)));

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    return(sum((logvirus-virusdata)^2))

} #end function that fits the ODE model to the data

############################################################
#the main part, which calls the fit function
############################################################

#' Fitting 2 simple viral infection models to influenza data
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system in the presence of drug treatment.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param U0 initial number of uninfected target cells
#' @param I0 initial number of infected target cells
#' @param V0 initial number of infectious virions
#' @param X0 initial level of immune response
#' @param n rate of new uninfected cell replenishment
#' @param dU rate at which uninfected cells die
#' @param dI rate at which infected cells die
#' @param dV rate at which infectious virus is cleared
#' @param b rate at which virus infects cells
#' @param p rate at which infected cells produce virus
#' @param f strength of cell infection reduction by drug (0-1)
#' @param e strength of virus production reduction by drug (0-1)
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A simple compartmental model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_virus_tx()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_virus_tx(V0 = 100, tmax = 100, n = 1e5, dU = 1e-2)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,1],result[,4],xlab='Time',ylab='Virus',type='l',log='y')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_basicfitting <- function(U0 = 1e5, I0 = 0, V0 = 1, tmax = 30, dI = 1, dV = 2, b = 1e-5, p = 10, f = 0, e = 0, steadystate = FALSE)
{
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine

  #load data
  data=read.csv('simplefitdata.csv')

  Y0 = c(U = U0, I = I0, V = V0, X = X0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining fixed parameters into a parameter vector
  fixedpars = c(dI=dI,dV=dV,b=b,p=p,k=k);

  if (modeltype == 1)
  {
    par_ini = c(a=a0, r=r0);
    lb = c(alow, rlow)
    ub = c(ahigh, rhigh)
  }

  if (modeltype == 2)
  {
    par_ini = c(a=a0, dX=dX0);
    lb = c(alow, dXlow)
    ub = c(ahigh, dXhigh)
  }


  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=fitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_COBYLA",xtol_rel=1e-10,maxeval=maxsteps,print_level=0),data=data,modeltype=modeltype);

  #extract best fit parameter values and from the result returned by the optimizer
  finalparams = bestfit$solution

  #time-series for best fit model
  finalmodel = deSolve::ode(y = Y0, times = timevec, func = model1ode, parms=modelpars, atol=1e-8, rtol=1e-8)

  #compute sum of square residuals (SSR) for initial guess and final solution
  logvirusfinal1=log10(odeoutputfinal1[seq(11,90,10),4]);
  ssrfinal1=sum((logvirusfinal1-virusdata)^2);

  #--> write code to compute AICc for both models, call those values AICc1 and AICc2
  N=length(virusdata) #number of datapoints
  K1=length(p.ini1); #fitted parameters for model 1
  AICc1=N*log(ssrfinal1/N)+2*(K1+1)+(2*(K1+1)*(K1+2))/(N-K1)

  #AIC of best fit model

  #list structure that contains all output
  output =

  #The output produced by the fitting routine
  return(output)
}
