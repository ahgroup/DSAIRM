############################################################
##a model for HCV infection with interferon and ribavirin treatment 
#that can account for triphasic decline in virus load
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
  	Utc=y[1]; Itc=y[2]; Vi=y[3]; Vni=y[4];  #infected cells, infectious virus, non-infectious virus
    lambda=parms[1]; d=parms[2]; b=parms[3]; delta=parms[4]; p=parms[5]; 
    clear=parms[6]; gU=parms[7]; gI=parms[8]; Umax=parms[9]; e=parms[10]; r=parms[11]; #model parameters
  	 
	  #these are the differential equations
		dUtcdt=lambda-d*Utc-b*Vi*Utc+gU*Utc*(1-(Utc+Itc)/Umax);
    dItcdt=b*Vi*Utc-delta*Itc+gI*Itc*(1-(Utc+Itc)/Umax);
		dVidt=(1-r)*(1-e)*p*Itc-clear*Vi;
    dVnidt=r*(1-e)*p*Itc-clear*Vni;
 
		return(list(c(dUtcdt,dItcdt,dVidt,dVnidt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
  tmax=50;              
  timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated 
	
  #values for model parameters, units are assumed to be 1/days
	lambda=5;
	d=0.01;
	b=2e-7;
  delta=0.2;
	p=5;
	clear=5;
  gU=0.5;
  gI=0.25;
  Umax=1e7;

  parms=c(lambda,d,b,delta,p,clear,gU,gI,Umax); #parameter vector

  #set initial conditions 
	Utc0=5e5; #initial number of uninfected cells  
	Vi0=2.5e6; #initial number for virus  
  Vni0=0; #initial number for non-infectious virus 
  Itc0=clear/p*Vi0; #initial number of infected cells 
	Drug0=0; #initial level of drug concentration
  Y0=c(Utc0, Itc0, Vi0, Vni0);  #combine initial conditions into a vector 
      			
	#call ode-solver lsoda to integrate ODEs with no treatment
  e=0; r=0;
  odeoutput1=lsoda(Y0,timevec,odeequations,c(parms,e,r));
  #call ode-solver lsoda to integrate ODEs with IFN and no ribavirin treatment
  e=0.95; r=0;
  odeoutput2=lsoda(Y0,timevec,odeequations,c(parms,e,r));
  #call ode-solver lsoda to integrate ODEs with IFN and ribavirin treatment
  e=0.95; r=0.5;
  odeoutput3=lsoda(Y0,timevec,odeequations,c(parms,e,r));
  
  #plot results
  plot(odeoutput1[,1],odeoutput1[,4]+odeoutput1[,5],type="l",xlab="time (days)",ylab="Virus load",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1e1,1e7))
  #lines(odeoutput1[,1],odeoutput1[,2],type="l",col="violet",lwd=2)
  #lines(odeoutput1[,1],odeoutput1[,3],type="l",col="blue",lwd=2)
  lines(odeoutput2[,1],odeoutput2[,4]+odeoutput2[,5],type="l",col="blue",lwd=2)
  lines(odeoutput3[,1],odeoutput3[,4]+odeoutput3[,5],type="l",col="red",lwd=2)
  legend(1,1e3, c("no drug","IFN","IFN + rib"),col = c("green","blue","red"),lwd=2)
   
  
  
###################################################################
#end main program
###################################################################                       
