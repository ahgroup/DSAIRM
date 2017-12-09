############################################################
##a model for HIV infection with a CTL immune responses
##written by Andreas Handel, ahandel@uga.edu, last change 6/25/2013
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package

#functions come first, main program below

###################################################################
#function that specificies the ode model called by lsoda (the ode solver) 
###################################################################
#Note: In contrast to previous programs, here we don't send the paramater values into the ODE function, instead they are treated as global variables. 
#This is bad programming style! But it makes coding easier :) Just make sure you keep track of where you define variables 
odeequations=function(t,y,parms) #lsoda requires parms to be there, even though it's empty
{ 
  	Utc=y[1]; Itc=y[2]; Vir=y[3]; CTL=y[4];  #uninfected cells, infected cells, virus, CTL
   
	  #these are the differential equations for uninfected cells, infected cells and virus
		dUtcdt=lambda-d*Utc-b*Vir*Utc;
		dItcdt=b*Vir*Utc-delta*Itc-k*Itc*CTL;
		dVirdt=p*Itc-clear*Vir;
		dCTLdt=a*CTL*Vir-w*CTL; 
		
		return(list(c(dUtcdt,dItcdt,dVirdt,dCTLdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
  tmax=120;              
  timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated (from 0 to 50 days in steps of 0.1)
	
  #values for model parameters, units are assumed to be 1/days
	lambda=1e4;
	d=0.01;
	b=5e-7;
  delta=1;
	p=50;
	clear=5;
  w=0.1;
  k=5e-5;

	#set initial condition 
	Utc0=1e6; #initial number of uninfected cells  
	Vir0=1; #initial number for free virus V 
  Itc0=0; #initial number of infected cells 
	CTL0=1; #initial number of activated effector CTL (arbitrary scale)
  Y0=c(Utc0, Itc0, Vir0, CTL0);  #combine initial conditions into a vector 
    				
	#integrate model without CTL dynamics 
  a=0;  
  #Note: In contrast to previous programs, here we don't send the paramater values into the ODE function, instead they are treated as global variables. 
  #This is bad programming style! But it makes coding easier :) Just make sure you keep track of where you define variables 
  #Therefore "parms" is empty and is not used in the ode function
  odeoutput1=lsoda(Y0,timevec,odeequations,parms="",atol=1e-5); 
  #integrate model with the predator-prey CTL dynamics, unvaccinated 
  a=1e-6; 
  k=5e-5;
  CTL0=1; #initial number of activated effector CTL (arbitrary scale)
  Y0=c(Utc0, Itc0, Vir0, CTL0);  #combine initial conditions into a vector 
  odeoutput2=lsoda(Y0,timevec,odeequations,parms="",atol=1e-5);

  #integrate model with the predator-prey CTL dynamics, vaccinated (implemented as increased a)
  #change any of a/k/CTL0 to simulate greater effectiveness/numbers of memory CTL
  a=1.5e-6; 
  k=6e-5;
  CTL0=10; #initial number of activated effector CTL (arbitrary scale)
  Y0=c(Utc0, Itc0, Vir0, CTL0);  #combine initial conditions into a vector 
  odeoutput3=lsoda(Y0,timevec,odeequations,parms="",atol=1e-5);
  
  #if the plot looks too crowded in the plot window inside RStudio,
  #uncomment the following line to open a new plotting window
  #windows(width=8,height=8)

  #another way to split the graphics window into several parts
  layout(matrix(c(1,2,3,4),ncol=2, byrow=T))
    
  #plot results
  ymin=1; ymax=1e8;
  #model without IR 
  plot(odeoutput1[,1],odeoutput1[,2],type="l",xlab="time (days)",ylab="",main="no CTL",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax))
  lines(odeoutput1[,1],odeoutput1[,3],type="l",col="red",lwd=2)
  lines(odeoutput1[,1],odeoutput1[,4],type="l",col="blue",lwd=2)
  lines(odeoutput1[,1],odeoutput1[,5],type="l",col="black",lwd=2)
  
  #model with predator-prey (PP) IR, no vaccination 
  plot(odeoutput2[,1],odeoutput2[,2],type="l",xlab="time (days)",ylab="",main="with CTL - no vac",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax))
  lines(odeoutput2[,1],odeoutput2[,3],type="l",col="red",lwd=2)
  lines(odeoutput2[,1],odeoutput2[,4],type="l",col="blue",lwd=2)
  lines(odeoutput2[,1],odeoutput2[,5],type="l",col="black",lwd=2)
  legend(50,1e7, c("U","I","V","CTL"),col = c("green","red","blue","black"),lwd=2)
  
  #model with predator-prey (PP) IR, with vaccination 
  plot(odeoutput3[,1],odeoutput3[,2],type="l",xlab="time (days)",ylab="",main="with CTL - with vac",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax))
  lines(odeoutput3[,1],odeoutput3[,3],type="l",col="red",lwd=2)
  lines(odeoutput3[,1],odeoutput3[,4],type="l",col="blue",lwd=2)
  lines(odeoutput3[,1],odeoutput3[,5],type="l",col="black",lwd=2)
  
  #this plots virus load for vaccinated and non-vaccinated animals
  plot(odeoutput3[,1],odeoutput3[,4],type="l",xlab="time (days)",ylab="virus",main="comparison",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,ymax))
  lines(odeoutput2[,1],odeoutput2[,4],type="l",col="red",lwd=2)
  legend(20,1e8, c("CTL-vac","CTL-novac"),col = c("green","red"),lwd=2,lty=c(1,1))
   
  
###################################################################
#end main program
###################################################################                       
