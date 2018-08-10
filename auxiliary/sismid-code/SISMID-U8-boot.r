##################################################################################
#fitting influenza virus load data to a simple ODE model
#using bootstrapping to produce confidence intervals
##written by Andreas Handel, ahandel@uga.edu, last change 6/21/14
##################################################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package
require(boot)  #loads package for bootstrapping
require(nloptr) #optimization package
                
#################################
#experimental data values from Hayden et al. 1996 JAMA 
#one could load the data from a separate file
#here, for simplicity, we include it as part of the program
################################
timedata=c(1,     2,    3,    4,    5,    6,    7,    8  ); #days since infection
virusdata=c(0.97, 2.73, 2.72, 1.92, 0.85, 0.58, 0.42, 0.2);  #virus load, in units of log10 
                                                 
##################################################################################
#defining the functions, then comes the main program
##################################################################################

###################################################################
#function specifying the ode model. This is called by the ode solver inside the fit function
###################################################################
odeequations=function(t,y,pars)  
{ 
  	 
	  #these are the 3 differential equations
		dUtcdt=-pars["b"]*y[1]*y[3];
		dItcdt=pars["b"]*y[1]*y[3]-delta*y[2];
		dVirdt=p*y[2]-pars["clear"]*y[3];
    
    dAll=c(dUtcdt,dItcdt,dVirdt);
    
    return(list(dAll)); 

} #end function specifying the ODEs


###################################################################
#function that fits the ODE model to data 
###################################################################
fitfunction <- function(parameters,timedata,virusdata)
{

    Vir0=parameters[3]; #initial condition for free virus, which is being fit
    
    Utc0=4e8; #initial number of uninfected cells 
	  Itc0=0; #initial number of infected cells
	  Y0=c(Utc0, Itc0, Vir0);  #combine initial conditions into a vector 
  	timevec=seq(0, 8,0.1); #vector of times for which integration is evaluated
	  
    
    #parameters for ODE solver
    ode.parms=parameters[1:2]
    names(ode.parms)=c("b","clear")
    #call ode-solver lsoda to integrate ODEs 
    odeoutput=try(lsoda(Y0,timevec,odeequations,parms=ode.parms,atol=atolv,rtol=rtolv)); #
    if (length(odeoutput)==1) {cat('!!unresolvable integrator error - triggering early return from optimizer!!'); return(1e10) }
    #try command catches error from lsoda. If error occurs and lsoda "breaks", we exit the whole optimizer routine with a high objective function value
   
    
    #this extracts values for virus load at time points corresponding to values in timedata 
    #and also replicates data points that are sampled more than once
    #check out how the function match() works to understand the details of this
    virusfit=odeoutput[match(timedata,odeoutput[,1]),4];

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming. 
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming 
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,virusfit))); 
  
    #plot data and current fit to watch fitting in real time - not necessary but nice to see it
    if (showfigs==1)
    {
      plot(timedata,virusdata,type="p",xlim=c(0,8),ylim=c(-2,4));
      lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,4])),col="red")
      points(timedata,logvirus,col="red");
      legend("topleft",c("data","fit"),col = c("black","red"),lwd=2,cex=1)
    }
    #compute the objective function, namely SSR 
    SSR=sum((logvirus-virusdata)^2)
    return(SSR) #returns the objective function, the sum of squares, which is being minimized by optim 
    
} #end function that fits the ODE model to the data

###################################################################
#function to do the bootstraps
###################################################################
#this extra function is needed for the bootstrap routine.
#it basically calls the optimization routine and returns the best fit parameter values (stored in finalparams) to the bootstrap function
#the bootstrap routine is called in the main program below
bootfct1 <- function(alldata,indi)
{
    timedata=alldata[indi,1]; virusdata=alldata[indi,2];
    fitresult <- nloptr(x0=p.ini,eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_COBYLA",xtol_rel=1e-6,lb=lb,ub=ub,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata); 
    
    #extract best fit parameter values and from the result returned by the optimizer
    finalparams=fitresult$solution; 
    
    return(finalparams)
}


############################################################
#the main part, which calls the fit function 
############################################################
set.seed(100); #seed for random number generator

delta=1; #fixed parameter, clearance rate of infected cells
p=1e-5;  #we also fix p here
maxsteps=500; #maximum number of iterations for the optimization routine - should usually be more, but we restrict it to make the code run in a reasonable time

atolv=1e-4; rtolv=1e-4; #accuracy settings for the ODE solver routine

#starting guesses and bounds for the parameters that will be fit
b0=1e-2;  blow=1e-10;    bhigh=1e5;
clear0=10; clearlow=1e-5; clearhigh=1e5; 
Vir00=1;  Vlow=1e-3;    Vhigh=1e3;

lb=c(blow,clearlow,Vlow);
ub=c(bhigh,clearhigh,Vhigh);

p.ini=c(b0,clear0,Vir00); #initial conditions as vector
names(p.ini)=c('b','clear','V')

#fitting the model once
print(sprintf("starting to fit"));
showfigs=0;
fitresult <- nloptr(x0=p.ini,eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_COBYLA",xtol_rel=1e-6,lb=lb,ub=ub,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata); 

#extract best fit parameter values and from the result returned by the optimizer
finalparams=fitresult$solution; 
names(finalparams)=c("b","clear","V")


############################################################
#output result
############################################################

#compute model solution for initial and final parameter values
odeoutputinitial=lsoda(c(4e8,0,Vir00),seq(0, 8,0.1),odeequations,c(b=b0,clear=clear0));
odeoutputfinal=lsoda(c(4e8,0,finalparams[3]),seq(0, 8,0.1),odeequations,finalparams[1:2]);

#compute sum of square residuals (SSR) for initial guess and final solution
logvirusinitial=log10(odeoutputinitial[seq(11,90,10),4]);
ssrinitial=sum((logvirusinitial-virusdata)^2);
logvirusfinal=log10(odeoutputfinal[seq(11,90,10),4]);
ssrfinal=sum((logvirusfinal-virusdata)^2);
                                            
#code that prints both the initial and final values of the parameters that are being fit and the initial and final value of the SSR onto the screen 
print(sprintf('initial values: b=%e; c=%e; V0=%e; SSR=%f;',b0,clear0,Vir00,ssrinitial));
print(sprintf('final values: b=%e; c=%e; V0=%e; SSR=%f;',finalparams[1],finalparams[2],finalparams[3],ssrfinal));

#plot data and model solution for initial and final parameter values
graphics.off(); #close graphics window     
plot(timedata,virusdata,type="p",xlim=c(0,8),ylim=c(-2,4),xlab="time (days)",ylab="Log virus",col="black",lwd=2);
lines(odeoutputinitial[,1],log10(pmax(1e-10,odeoutputinitial[,4])),col="red",type="l",lwd=2)
lines(odeoutputfinal[,1],log10(pmax(1e-10,odeoutputfinal[,4])),col="blue",type="l",lwd=2)
legend(2,1, c("initial","final"),col = c("red","blue"),lwd=2,cex=1)

print(sprintf('finished single fit - press Enter to start bootstrapping')); scan(file="",what="character",nmax=1);

#bootstrapping ODE model
#now we are doing the fit R times with newly sampled data 
#this will produce R different best-fit parameter estimates
#we can then use this distribution of best-fit values to determine 95% confidence intervals
#to get reliable results, one should do hundreds to thousands of bootstraps  
#but this takes long, especially for ODE models, therefore we do fewer replicates here
print(sprintf('starting to bootstrap - be patient'))
alldata=cbind(timedata,virusdata)
#this is the call to the bootstrap function
#the bootstrap function resamples the data contained in "alldata" and then calls bootfct1, which itself calls the optimizer
#this is done Rmax times to obtain Rmax different best-fit values
#the bootstrap results are contained in the datastructure bssample1
Rmax=50; #one should usually do 100s or 1000s of bootstrap replicates, but that would take too long, so here we only do a few bootstraps
showfigs=0; #to speed up things, turn off the figure plotting
bssample <- boot(data=alldata,statistic=bootfct1,R=Rmax) #do Rmax bootstraps (fitting on resampled data)

#calculate the 95% confidence intervals for parameters b and V0
ci.b=boot.ci(bssample,index=1,type = "perc") 
print(sprintf('best-fit and 95CI for b: (%e,%e,%e)',ci.b$perc[4],ci.b$t0,ci.b$perc[5]));
ci.V0=boot.ci(bssample,index=3,type = "perc") 
print(sprintf('best-fit and 95CI for V0: (%e,%e,%e)',ci.V0$perc[4],ci.V0$t0,ci.V0$perc[5]));

#print the best-fit and 95 CI values for the remaining parameter, i.e. c 
#print the average virus lifespan in units of hours
#see the documentation for boot.ci (?boot.ci) for more information
ci.c=boot.ci(bssample,index=2,type = "perc") 
print(sprintf('best-fit and 95CI for virus clearance rate c, units of 1/day: (%f,%f,%f)',ci.c$perc[4],ci.c$t0,ci.c$perc[5]));
print(sprintf('best-fit and 95CI for virus lifespan, units of hour: (%f,%f,%f)',24/ci.c$perc[5],24/ci.c$t0,24/ci.c$perc[4]));



###################################################################
#end main program
###################################################################