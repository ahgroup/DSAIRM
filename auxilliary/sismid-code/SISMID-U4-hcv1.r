############################################################
##a model for HCV infection with interferon treatment
##written by Andreas Handel, ahandel@uga.edu, last change 5/23/10
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package

#functions come first, main program below

###################################################################
#function that specificies the ode model called by lsoda (the ode solver) 
###################################################################
odeequations=function(t,y,parms) 
{ 
  	Utc=y[1]; Itc=y[2]; Vir=y[3];  #uninfected cells, infected cells, virus
    lambda=parms[1]; d=parms[2]; b=parms[3]; delta=parms[4]; p=parms[5]; clear=parms[6]; f=parms[7]; e=parms[8]; #model parameters
	 
	  #these are the differential equations
		dUtcdt=lambda-d*Utc-(1-f)*b*Vir*Utc;
		dItcdt=(1-f)*b*Vir*Utc-delta*Itc;
		dVirdt=(1-e)*p*Itc-clear*Vir;
 
		return(list(c(dUtcdt,dItcdt,dVirdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
  tmax=14;              
  timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated 
	
  #values for model parameters, units are assumed to be 1/days
	lambda=1e4;
	d=0.001;
	b=3e-7;
  delta=0.2;
	p=1e2;
	clear=6;
	parms=c(lambda,d,b,delta,p,clear); #vector of parameters which is sent to the ODE function
	
	#set initial condition to steady state values 
	Utc0=clear*delta/(b*p); #initial number of uninfected cells at steady state 
	Vir0=p*lambda/(clear*delta)-d/b; #initial number for free virus V at steady state
  Itc0=lambda/delta-d*clear/(b*p); #initial number of infected cells at steady state
	Y0=c(Utc0, Itc0, Vir0);  #combine initial conditions into a vector 
				
	#call ode-solver lsoda to integrate ODEs 
  f=0.0; e=0.8; #change f and e from zero to some value between 0 and 1 to simulate different types and strengths of hypotehtical IFN action
  #f=0.99; e=0.0; #change f and e from zero to some value between 0 and 1 to simulate different types and strengths of hypotehtical IFN action
  odeoutput1=lsoda(Y0,timevec,odeequations,c(parms,f,e));
  
  #plot results
  plot(odeoutput1[,1],odeoutput1[,4],type="l",xlab="time (days)",ylab="Virus load",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1e-7,1e7))
    
  
###################################################################
#end main program
###################################################################                       
