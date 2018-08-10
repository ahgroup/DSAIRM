############################################################
##a simple model for a simple bacteria infection model
##written by Andreas Handel (ahandel@uga.edu), last change 6/20/12
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
library(deSolve)  #loads ODE solver package

#functions come first, main program below

###################################################################
#function that specificies the ode model called by lsoda (the ode solver) 
###################################################################
odeequations=function(t,y,pars) 
{ 
    #Note: y is a vector containing the variables of our system, pars is a vector containing the parameters
    #It's not necessary to give them names like B, I, g, etc. We could just work with y[1], par[1] etc.
    #But assigning them easy to understand names often helps, though it slows down the code a bit 
  	B=y[1]; I=y[2];  #bacteria and immune response
    g=pars[1]; Bmax=pars[2]; d=pars[3]; k=pars[4]; #model parameters, passed as vector "par" into function by main program
	  r=pars[5]; delta=pars[6];  
    
	  #these are the differential equations
		dBdt=g*B*(1-B/Bmax)-d*B-k*B*I;
		dIdt=r*B*I-delta*I;
		
		#these is how the differential equations would need to look
    #if we were to skip the step of assigning easy to understand names to the variables and paramters  
		#dBdt=par[1]*y[1]*(1-y[1]/par[2])-par[3]*y[1]-par[4]*y[1]*y[2];
		#dIdt=par[5]*y[1]*y[2]-par[6]*y[2];
		
		return(list(c(dBdt,dIdt))); #this is returned to the calling function, i.e. lsoda

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
                
  B0=1e2; #initial number of bacteria 
	I0=10; #initial number of immune response
  Y0=c(B0, I0);  #combine initial conditions into a vector 
  
  #values for model parameters, units are assumed to be 1/days
  g=1; 
  Bmax=1e6;
  d=1e-1;
  k=1e-7;
  r=1e-3;
  delta=1;
	pars=c(g,Bmax,d,k,r,delta); #vector of parameters which is sent to the ODE function
	
	tmax=10; #number of days for which to run the simulation
	timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated (from 0 to 10 days in steps of 0.1)
	
	#call ode-solver to integrate ODEs
  #integrate for time "timevec", starting with initial condition 'Y0'. 
  odeoutput=lsoda(Y0,timevec,odeequations,pars);

  #plot results
  #first column contains time vector, the following columns contain variables 1 (bacteria) and 2 (immune response)
  plot(odeoutput[,1],odeoutput[,2],type="l",xlab="time (days)",ylab="",col="blue",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,1e9))
  lines(odeoutput[,1],odeoutput[,3],type="l",col="red",lwd=2)
  legend("topleft", c("Bacteria","Immune Response"),col = c("blue","red"),lwd=2)
  
###################################################################
#end main program
###################################################################                           
