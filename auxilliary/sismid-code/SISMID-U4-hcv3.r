############################################################
##a model for HCV infection with interferon and ribavirin treatment
##written by Andreas Handel (ahandel@uga.edu), last change 6/25/2013
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
  	Itc=y[1]; Vi=y[2]; Vni=y[3];  #infected cells, infectious virus, non-infectious virus
    b=parms[1]; delta=parms[2]; p=parms[3]; clear=parms[4]; e=parms[5]; r=parms[6]; #model parameters
	 
	  #these are the differential equations
		dItcdt=b*Vi*Us-delta*Itc;
		dVidt=(1-r)*(1-e)*p*Itc-clear*Vi;
    dVnidt=r*(1-e)*p*Itc-clear*Vni;
 
		return(list(c(dItcdt,dVidt,dVnidt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
  tmax=50;              
  timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated 
	
  #values for model parameters, units are assumed to be 1/days
	b=3e-7;
  delta=0.2;
	p=1e2;
	clear=6;
	parms=c(b,delta,p,clear); #vector of parameters which is sent to the ODE function
	Us=clear*delta/(b*p); #number of uninfected cells at steady state, kept fixed
	
	#initial condition 
	Vi0=1e7; #initial number for infectious virus V at steady state
  Vni0=0; #initial number for non-infectious virus V at steady state
  Itc0=clear/p*Vi0; #initial number of infected cells at steady state
	Y0=c(Itc0, Vi0, Vni0);  #combine initial conditions into a vector 
    			
	#call ode-solver lsoda to integrate ODEs with no treatment
  e=0; r=0;
  odeoutput1=lsoda(Y0,timevec,odeequations,c(parms,e,r));
  #call ode-solver lsoda to integrate ODEs with IFN and no ribavirin treatment
  e=0.5; r=0;
  odeoutput2=lsoda(Y0,timevec,odeequations,c(parms,e,r));
  #call ode-solver lsoda to integrate ODEs with IFN and ribavirin treatment
  e=0.5; r=0.9;
  odeoutput3=lsoda(Y0,timevec,odeequations,c(parms,e,r));
  
  #plot results
  plot(odeoutput1[,1],odeoutput1[,3]+odeoutput1[,4],type="l",xlab="time (days)",ylab="Virus load",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1e1,1e7))
  lines(odeoutput2[,1],odeoutput2[,3]+odeoutput2[,4],type="l",col="blue",lwd=2)
  lines(odeoutput3[,1],odeoutput3[,3]+odeoutput3[,4],type="l",col="red",lwd=2)
  legend(1,1e3, c("no drug","IFN","IFN + rib"),col = c("green","blue","red"),lwd=2)
  
###################################################################
#end main program
###################################################################                       
