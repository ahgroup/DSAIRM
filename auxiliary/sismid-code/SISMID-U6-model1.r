############################################################
#a simple model for a bacteria infection, used to illustrate model analysis
##written by Andreas Handel, ahandel@uga.edu, last change 6/20/14
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
  	B=y[1]; X=y[2]; #bacteria and immune response
    g=parameters[1]; Bmax=parameters[2]; k=parameters[3]; r=parameters[4]; d=parameters[5]; 
    
	  #these are the differential equations
		dBdt=g*B*(1-B/Bmax)-k*B*X;
		dXdt=r*B*X-d*X;
 
		return(list(c(dBdt,dXdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
                
B0=10; #initial number of bacteria 
X0=1; #initial level of immune response
Y0=c(B0, X0);  #combine initial conditions into a vector 
tmax=365;  #number of days
timevec=seq(0,tmax,0.05); #vector of times for which integration is evaluated 
	
#values for fixed model parameters, units are 1/days
k=1e-7; #kill rate
Bmax=1e7;  #bacteria carrying capacity
d=0.1;  #immune response death rate
g=0.5; #bacteria growth rate

samplemax=20; #number of samples/values for parameter r
rvec=10^seq(-7,-4,length=samplemax) #create values for r spaced logarithmically between 10E-7 and 10E-4

ploton=1; #set this to 1 if you want to see the time-series for every parameter sample. Leave it at zero to make the code run faster.
 
#####################

Bpeak=rep(0,samplemax); #initialize vector that will contain the solution
	
#this is the loop over samplemax samples for the parameter values
for (nsample in 1:samplemax)
{
	    print(sprintf('Starting Simulation %d of %d',nsample,samplemax));
	   
      #values for sampled parameters 
      r=rvec[nsample];
	  
      parameters=c(g,Bmax,k,r,d); #vector of parameters which is sent to the ODE function
			
	    #call ode-solver to integrate ODEs
      #see the documentation of the deSolve package to learn the difference between this solver and lsoda
	    #I'm using vode instead of lsoda since I found that lsoda occasionally failed for this problem
      odeoutput=vode(Y0,timevec,odeequations,parameters,mf=22,atol=1e-12,rtol=1e-12,hini=1e-10);
      vl=length(odeoutput[,1]);
      
      #we can plot time series every time if we want, just to watch what happens - is not necessary to do so
      if (ploton==1)
      {
        plot(odeoutput[,1],odeoutput[,2],col="red",type="l",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,1e8),xlab="time (days)",ylab="")
        lines(odeoutput[,1],odeoutput[,3],col="blue",lwd=2)
        legend("topright",c("Bacteria","IR"),col=c("red","blue"),lwd=2)
      }
            
      #record the peak value for B   
      Bpeak[nsample]=max(odeoutput[,2]);
        
 }
#produce result plot
plot(rvec,Bpeak,log="xy",pch=20,col="blue",xlab="IR Activation Rate",ylab="Bacteria Peak")



###################################################################
#end main program
###################################################################