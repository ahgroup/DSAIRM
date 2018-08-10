############################################################
##a simple model for an acute virus infection
#model is implemented as ODE and discrete version to illustrate both approaches
##written by Andreas Handel (ahandel@uga.edu), last change 6/20/13
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
library(deSolve)  #loads ODE solver package

#functions come first, main program below

###################################################################
#function that specificies the ode model called by lsoda (the ode solver) 
###################################################################
odeequations=function(t,y,parameters) 
{ 
  	Utc=y[1]; Itc=y[2]; Vir=y[3];  #uninfected target cells, infected target cells, virus
		b=parameters[1]; delta=parameters[2]; p=parameters[3]; clear=parameters[4]; #four model parameters, passed into function by main program
	 
	  #these are the 3 differential equations
		dUtcdt=-b*Vir*Utc;
		dItcdt=b*Vir*Utc-delta*Itc;
		dVirdt=p*Itc-clear*Vir;
 
		return(list(c(dUtcdt,dItcdt,dVirdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
                
  Utc0=1e8; #initial number of uninfected cells 
	Itc0=0; #initial number of infected cells
	Vir0=10; #initial condition for free virus V
  Y0=c(Utc0, Itc0, Vir0);  #combine initial conditions into a vector 
  
  #values for model parameters, units are assumed to be 1/days
	b=1e-8;
	delta=2;
	p=1e2;
	clear=10; #Note: we don't call the parameter "c" since c is a reserved function in R that creates vectors (see next line for a use of it)
	#always make sure that your variable names do not clash with built-in definitions, or you might get unpredictable results
  parameters=c(b,delta,p,clear); #vector of parameters which is sent to the ODE function
	
	timevec=seq(0,20,0.1); #vector of times for which integration is evaluated (from 0 to 10 days in steps of 0.1)
	
	#call ode-solver to integrate ODEs 
  odeoutput=lsoda(Y0,timevec,odeequations,parameters);

  #now do a manual loop of a discrete-time model
  dt=0.1; #discrete time step - change it to see how agreement between continuous and discrete time models changes
  t.discrete=seq(0,20,by=dt);
  Uvec=rep(0,length(t.discrete)); Ivec=Uvec; Vvec=Uvec; #create empty vectors to save results
  Uvec[1]=Utc0; Ivec[1]=Itc0; Vvec[1]=Vir0;
  for (n in 1:(length(t.discrete)-1)) #this is the time loop for the discrete-time model
  {
       Uvec[n+1]=Uvec[n]- dt*b*Uvec[n]*Vvec[n];
       Ivec[n+1]=Ivec[n] + dt*(b*Uvec[n]*Vvec[n]-delta*Ivec[n]);
       Vvec[n+1]=Vvec[n] + dt*(p*Ivec[n]-clear*Vvec[n]);
  }


  #plot results
  plot(odeoutput[,1],odeoutput[,2],type="l",xlab="time (days)",ylab="",col="green",lwd=2,log="y",xlim=c(0,20),ylim=c(1,1e9))
  lines(odeoutput[,1],odeoutput[,3],type="l",col="red",lwd=2)
  lines(odeoutput[,1],odeoutput[,4],type="l",col="blue",lwd=2)

  lines(t.discrete,Uvec,col="green",lwd=2,lty=2)
  lines(t.discrete,Ivec,type="l",col="red",lwd=2,lty=2)
  lines(t.discrete,Vvec,type="l",col="blue",lwd=2,lty=2)


  legend("topright", c("Uninfected","Infected","Virus","Uninfected - discrete","Infected - discrete","Virus - discrete"),col = c("green","red","blue"),lwd=2,lty=c(1,1,1,2,2,2))
  
###################################################################
#end main program
###################################################################                           
