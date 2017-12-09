############################################################
#a simple model for a TB infection, used to illustrate uncertainty analysis - complete code
##written by Andreas Handel, ahandel@uga.edu, last change 6/24/13
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows 
library(deSolve)  #loads ODE solver package
library(lhs) #LHS package

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
r=5e-7;  #immune response growth parameter

ploton=0; #set this to 1 if you want to see the time-series for every parameter sample. Set to zero to make the code run faster.

samplemax=100; #number of samples

lhssample=randomLHS(samplemax,2); #this creates a LHS with samplemax samples for 2 parameters, drawn from a uniform distribution between zero and one

#we assume that d is uniformly distributed 
dmin=0.1; dmax=0.4;
#dmin=1; dmax=4;
dvec=(dmax-dmin)*lhssample[,1]+dmin; #this transforms the uniform values between 0 and 1 that lhssample produces to uniform values between dmin and dmax
          
          
#transforming g from a uniform to a normal distribution. This is how one can do it in R:
#we assume that g is normally distributed with mean gmean and SD gsd
gmean=0.5; gsd=0.1;
gvec=qnorm(lhssample[,2], mean = gmean, sd = gsd); #this transforms the uniform distribution produced by lhssample to a normal distribution

gvec=pmax(1e-10,gvec) 
#to prevent negative values of g, this sets every value in g that is lower than 1E-10 to 1E-10. This is not a clean way of doing things, one should instead re-sample. 
#But it's quick/easy and justified as long as almost no values in gvec are negative.

#####################

Bpeak=rep(0,samplemax); #initialize vector that will contain the solution
	
#this is the loop over samplemax samples for the parameter values
for (nsample in 1:samplemax)
{
	    print(sprintf('Starting Simulation %d of %d',nsample,samplemax));
	   
     #values for sampled parameters 
	   d=dvec[nsample];
	   g=gvec[nsample]; 
	
      parameters=c(g,Bmax,k,r,d); #vector of parameters which is sent to the ODE function
			
	    #call ode-solver to integrate ODEs
      #see the documentation of the deSolve package to learn the difference between this solver and lsoda
	    #feel free to try lsoda for this problem, you will likely find that it fails for some parameter samples
      odeoutput=vode(Y0,timevec,odeequations,parameters,mf=22,atol=1e-12,rtol=1e-12,hini=1e-10);
      vl=length(odeoutput[,1]);
      
      #we can plot time series every time if we want, just to watch what happens - is not necessary to do so
      if (ploton==1)
      {
        plot(odeoutput[,1],odeoutput[,2],col="red",type="l",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,1e8),xlab="time (days)",ylab="")
        lines(odeoutput[,1],odeoutput[,3],col="blue",lwd=2)
        legend("topright",c("TB","IR"),col=c("red","blue"),lwd=2)
      }
            
      #record the peak value for B   
      Bpeak[nsample]=max(odeoutput[,2]);
        
 }
#produce boxplot
boxplot(Bpeak)
par(mfrow=c(2,1))
boxplot(Bpeak,main="Boxplot of Bpeak")
plot(ecdf(Bpeak),main="Cumulative Distribution of Bpeak")


###################################################################
#end main program
###################################################################