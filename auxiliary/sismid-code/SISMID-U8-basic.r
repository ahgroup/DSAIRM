##################################################################################
#fitting influenza virus load data to a simple ODE model
#this shows an example of bounding parameters in different ways
##written by Andreas Handel, ahandel@uga.edu, last change 6/21/14
##################################################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package
require(nloptr) #we are using the optimizers in the nloptr package

                
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
odeequations=function(t,y,parameters) 
{ 
  	Utc=y[1]; Itc=y[2]; Vir=y[3];  #uninfected cells, infected cells, virus
	  b.in=parameters[1]; p.in=parameters[2]; clear.in=parameters[3]; #four model parameters, passed into function by main program


  	if (boundtype==0 | boundtype==3) #no transformation or manual bound enforcement for parameters
  	{
     b=b.in; 
     p=p.in; 
     clear=clear.in; 
  	}
  	  
    
   #brute force way of enforcing bounds for parameters
   #if the solver tries a parameter value outside the bound, we reset the value artifically back to its limits.
   #note that with this approach, the solver might return best fit values that seem outside the bounds (e.g. negative values), 
   #but they really aren't since we enforce them to be within the bounds 	 
	 #basically, whenever the solver claims the best fit value is outside the bound, it really is the value at the bound
   if (boundtype==1)
   {
      b=min(bhigh,max(blow,b.in));
      p=min(phigh,max(plow,p.in));
      clear=min(clearhigh,max(clearlow,clear.in));
   }
  
    #since method 2 fits the log of the parameters (to make sure the parameters themselves don't become <0)
    #but the model uses the actual parameters, one needs to transform back.
    #For instance, we fit log(b). In the differential equations we need to transform back to b 
    if (boundtype==2 || boundtype==4)
    {
     b=exp(b.in)
     p=exp(p.in)
     clear=exp(clear.in)   
    }
  
   
	  #these are the 3 differential equations
		dUtcdt=-b*Vir*Utc;
		dItcdt=b*Vir*Utc-delta*Itc;
		dVirdt=p*Itc-clear*Vir;
 
		return(list(c(dUtcdt,dItcdt,dVirdt))); 

} #end function specifying the ODEs


###################################################################
#function that fits the ODE model to data 
###################################################################
fitfunction <- function(parameters,timedata,virusdata)
{
    #call ode-solver lsoda to integrate ODEs 
    odeoutput=lsoda(Y0,timevec,odeequations,parameters,atol=1e-6,rtol=1e-6);

    virusfit=odeoutput[match(timedata,odeoutput[,1]),4]; #extract model values at time points corresponding to values in timedata
 
    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming. 
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming 
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,virusfit))); 
  
    #plot data and current fit to watch fitting in real time - not necessary but nice to see it
    if (ploton==1)
    {
      plot(timedata,virusdata,type="p",xlim=c(0,8),ylim=c(-2,4));
      lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,4])),col="red")
      lines(timedata,logvirus,type="p",col="red");
    }
       
    #compute the objective function, namely SSR 
    SSR=sum((logvirus-virusdata)^2)
   
    return(SSR) #returns the objective function, the sum of squares, which is being minimized by optim 

    
} #end function that fits the ODE model to the data

############################################################
#the main part, which calls the fit function 
############################################################

ploton=1; #set this to 1 if you want to see plots during fit. for speed-up, turn it off
#if you are on a MAC and the windows() command below doesn't work, replace it with quartz()
if (ploton==1) {windows(height=8,width=8)} #plot in separate window since RStudio is a bit buggy when plotting into its figure window

delta=1; #fixed parameter, clearance rate of infected cells
maxsteps=1000; #maximum number of iterations for the optimization routine - should usually be very large (10K or more) to ensure convergence
xtol=1e-10; #tolerance for solver

Vir0=1e-1; #initial condition for free virus
Utc0=4e8; #initial number of uninfected cells 
Itc0=0; #initial number of infected cells
Y0=c(Utc0, Itc0, Vir0);  #combine initial conditions into a vector 
timevec=seq(0, 8,0.1); #vector of times for which integration is evaluated

#starting guesses and bounds for the parameters that will be fit
b0=5e-2;  blow=1e-10;    bhigh=1;
p0=1e-4;  plow=1e-10;    phigh=1e1;
clear0=10; clearlow=1e-5; clearhigh=1e5; 

p.ini=c(b0,p0,clear0) 
lb=c(blow,plow,clearlow);
ub=c(bhigh,phigh,clearhigh);



#no bound enforcement
boundtype=0;
print(sprintf("starting to fit without bounds"));
fitresult0 <- nloptr(x0=p.ini,eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=xtol,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata);
finalparams0=(fitresult0$solution); 
   
#uses brute-force bound enforcement
boundtype=1;
print(sprintf("starting to fit with brute force bounding"));
fitresult1 <- nloptr(x0=p.ini,eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=xtol,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata);
finalparams1=(fitresult1$solution); 
    
#uses log-transform of parameters to enforce positive values
boundtype=2;
print(sprintf("starting to fit using log-transformed parameters"));
fitresult2 <- nloptr(x0=log(p.ini),eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=xtol,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata);
finalparams2=(fitresult2$solution); 

#uses built-in bound enforcement capabilities of the solver
boundtype=3;
print(sprintf("starting to fit with built-in bounding"));
fitresult3 <- nloptr(x0=p.ini,eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=xtol,lb=lb,ub=ub,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata);
finalparams3=(fitresult3$solution); 

#uses built-in bound enforcement capabilities of the solver AND log-transforms parameters
boundtype=4;
print(sprintf("starting to fit with built-in bounding & log-transform"));
fitresult4 <- nloptr(x0=log(p.ini),eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=1e-6,lb=log(lb),ub=log(ub),maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata);
finalparams4=(fitresult4$solution); 
   

############################################################
#output result
############################################################

#compute model solution for initial and final parameter values
boundtype=0; odeoutputinitial=lsoda(Y0,timevec,odeequations,p.ini);
boundtype=0; odeoutputfinal0=lsoda(Y0,timevec,odeequations,finalparams0);
boundtype=1; odeoutputfinal1=lsoda(Y0,timevec,odeequations,finalparams1);
boundtype=2; odeoutputfinal2=lsoda(Y0,timevec,odeequations,finalparams2);
boundtype=3; odeoutputfinal3=lsoda(Y0,timevec,odeequations,finalparams3);
boundtype=4; odeoutputfinal4=lsoda(Y0,timevec,odeequations,finalparams4);

#compute sum of square residuals (SSR) for initial guess and final solution
modelresultinitial=log10(odeoutputinitial[seq(11,81,10),4]);
ssrinitial=sum(( modelresultinitial-virusdata)^2);
 modelresultfinal0=log10(odeoutputfinal0[seq(11,81,10),4]);
ssrfinal0=sum(( modelresultfinal0-virusdata)^2);
 modelresultfinal1=log10(odeoutputfinal1[seq(11,81,10),4]);
ssrfinal1=sum(( modelresultfinal1-virusdata)^2);
 modelresultfinal2=log10(odeoutputfinal2[seq(11,81,10),4]);
ssrfinal2=sum(( modelresultfinal2-virusdata)^2);
 modelresultfinal3=log10(odeoutputfinal3[seq(11,81,10),4]);
ssrfinal3=sum(( modelresultfinal3-virusdata)^2);
 modelresultfinal4=log10(odeoutputfinal4[seq(11,81,10),4]);
ssrfinal4=sum(( modelresultfinal4-virusdata)^2);
                                            
#print initial and final parameter values
#code that prints both the initial and final values of the parameters that are being fit and the initial and final value of the SSR onto the screen 
print(sprintf('initial values: b=%e; p=%e; c=%e; SSR=%f;',b0,p0,clear0,ssrinitial));
print(sprintf('final values - no bounds: b=%e; p=%e; c=%e;  SSR=%f;',finalparams0[1],finalparams0[2],finalparams0[3],ssrfinal0));
print(sprintf('final values - brute force: b=%e; p=%e; c=%e;  SSR=%f;',finalparams1[1],finalparams1[2],finalparams1[3],ssrfinal1));
print(sprintf('final values - log transform: b=%e; p=%e; c=%e;  SSR=%f;',exp(finalparams2[1]),exp(finalparams2[2]),exp(finalparams2[3]),ssrfinal2));
print(sprintf('final values - built-in bounds: b=%e; p=%e; c=%e;  SSR=%f;',finalparams3[1],finalparams3[2],finalparams3[3],ssrfinal3));
print(sprintf('final values - built-in bounds & log transform: b=%e; p=%e; c=%e;  SSR=%f;',exp(finalparams4[1]),exp(finalparams4[2]),exp(finalparams4[3]),ssrfinal4));


#plot data and model solution for initial and final parameter values
graphics.off(); #close graphics window     
plot(timedata,virusdata,type="p",xlim=c(0,8),ylim=c(-1,7),col="black",xlab="time (weeks)",ylab="people");
lines(odeoutputinitial[,1],log10(pmax(1e-10,odeoutputinitial[,4])),col="red",type="l",lwd=2,lty=1)
lines(odeoutputfinal0[,1],log10(pmax(1e-10,odeoutputfinal0[,4])),col="magenta",type="l",lwd=2,lty=2)
lines(odeoutputfinal1[,1],log10(pmax(1e-10,odeoutputfinal1[,4])),col="blue",type="l",lwd=2,lty=3)
lines(odeoutputfinal2[,1],log10(pmax(1e-10,odeoutputfinal2[,4])),col="black",type="l",lwd=2,lty=4)
lines(odeoutputfinal3[,1],log10(pmax(1e-10,odeoutputfinal3[,4])),col="green",type="l",lwd=2,lty=5)
lines(odeoutputfinal4[,1],log10(pmax(1e-10,odeoutputfinal4[,4])),col="orange",type="l",lwd=2,lty=6)
legend("topleft", c("initial","final - no bounds","final - forced","final - transformed","final - built-in","final - built-in & transformed"),col = c("red","magenta","blue","black","green","orange"),lwd=2,cex=1,lty=seq(1:6))


###################################################################
#end main program
################################################################### 