############################################################
#a simple model for a TB infection, used to illustrate sensitivty analysis using Regression Analysis 
##written by Andreas Handel, ahandel@uga.edu, last change 6/4/11
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows 
require(deSolve)  #loads ODE solver package
require(lhs) #LHS package
require(sensitivity) #package that does the Partial Rank Correlation Coefficient

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
tmax=5*365;  #number of days
timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated 
	
#values for fixed model parameters, units are 1/days
k=1e-7; #kill rate
Bmax=1e7;  #bacteria carrying capacity
r=5e-7;  #immune response growth parameter

samplemax=100; #number of samples

ploton=0; #set this to 1 if you want to see the time-series for every parameter sample. Leave it at zero to make the code run faster.

lhssample=randomLHS(samplemax,2); #this creates a LHS with samplemax samples for 2 parameters, drawn from a uniform distribution between zero and one

#we assume that d is uniformly distributed 
dmin=0.1; dmax=0.4;
#dmin=1; dmax=4;
dvec=(dmax-dmin)*lhssample[,1]+dmin; #this transforms the uniform values between 0 and 1 that lhssample produces to uniform values between dmin and dmax
          
          
#I'll do the harder bit for you, transforming g from a uniform to a normal distribution. This is how one can do it in R:
#we assume that g is normally distributed with mean gmean and SD gsd
gmean=0.5; gsd=0.1;
gvec=qnorm(lhssample[,2], mean = gmean, sd = gsd); #this transforms the uniform distribution produced by lhssample to a normal distribution

gvec=pmax(1e-10,gvec) 
#to prevent negative values of g, this sets every value in g that is lower than 1E-10 to 1E-10. This is not a clean way of doing things, one should instead re-sample. 
#But it's quick/easy and justified as long as almost no values in gvec are negative.

#####################

Bs=rep(0,samplemax); #initialize vectors that will contain the solution
Xs=rep(0,samplemax);
Bpeak=rep(0,samplemax); 
	
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
      
      #a quick check to make sure the system is at steady state, i.e. the value for B at the final time is not more than 1% different than B several time steps earlier
      if ((abs(odeoutput[vl,2]-odeoutput[vl-10,2])/odeoutput[vl,2])>1e-2)
      {
         print(sprintf('Steady state not reached!'));
         browser();
      }
        
      #record the final values for B and X and the peak of B  
      Bs[nsample]=odeoutput[vl,2];
      Xs[nsample]=odeoutput[vl,3];   
      Bpeak[nsample]=max(odeoutput[,2]);   
        
 }

#produce 6 plots, namely Bs, Xs and Bpeak as a function of both parameter samples d and g. 

par(mfrow=c(2,3))
plot(dvec,Bs,xlab="d",ylab="Bs",col="blue")
plot(dvec,Xs,xlab="d",ylab="Xs",col="blue")
plot(dvec,Bpeak,xlab="d",ylab="Bpeak",col="blue")
plot(gvec,Bs,xlab="g",ylab="Bs",col="blue")
plot(gvec,Xs,xlab="g",ylab="Xs",col="blue")
plot(gvec,Bpeak,xlab="g",ylab="Bpeak",col="blue")

#dev.copy(device=png,width=600,height=600,file="scatterplot.png"); dev.off(); #this saves the scatterplot to a png file

#compute adjusted R-squared for the regression model to make sure it's an ok proxy
#this uses the built-in R function lm
#Note that computing R^2 this way and later doing SRRC with the "sensitvity" package means the models are not quite the same since here we don't do rank transform
#we therefore underestimate the quality of the regression model as proxy
#so the reported R^2 and the SRRC don't come from the exactly same model, but it's good enough for our purpose
Bs.reg=lm(Bs ~ dvec+gvec+dvec*gvec) #the notation dvec+gvec+dvec*gvec corresponds to b1*X1+b2*X2+b3*X1*X2
Xs.reg=lm(Xs ~ dvec+gvec+dvec*gvec)
Bpeak.reg=lm(Bpeak ~ dvec+gvec+dvec*gvec)
R2.Bs=summary.lm(Bs.reg)$adj.r.squared
R2.Xs=summary.lm(Xs.reg)$adj.r.squared
R2.Bpeak=summary.lm(Bpeak.reg)$adj.r.squared

#simpler regression model, without cross-correlation term
Bs.reg=lm(Bs ~ dvec+gvec)
Xs.reg=lm(Xs ~ dvec+gvec)
Bpeak.reg=lm(Bpeak ~ dvec+gvec)
R2.Bs2=summary.lm(Bs.reg)$adj.r.squared
R2.Xs2=summary.lm(Xs.reg)$adj.r.squared
R2.Bpeak2=summary.lm(Bpeak.reg)$adj.r.squared


cat("\n")
print(sprintf('Adjusted R-squared for Bs, Xs, Bpeak - first regression model: %f, %f, %f',R2.Bs,R2.Xs,R2.Bpeak))
print(sprintf('Adjusted R-squared for Bs, Xs, Bpeak - simple regression model: %f, %f, %f',R2.Bs2,R2.Xs2,R2.Bpeak2))
cat("\n")

#compute Partial rank correlation coefficients using package "sensitivity"
Bs.prcc=pcc(as.data.frame(cbind(dvec,gvec)),Bs,rank=TRUE) 
Xs.prcc=pcc(as.data.frame(cbind(dvec,gvec)),Xs,rank=TRUE) 
Bpeak.prcc=pcc(as.data.frame(cbind(dvec,gvec)),Bpeak,rank=TRUE) 

#compute Standardized Rank Regression Coefficient using package "sensitivity"
#this is a model of the form Y=b1*X1+b2*X2+b3*X1*X2
Bs.srrc=src(as.data.frame(cbind(dvec,gvec,dvec*gvec)),Bs,rank=TRUE) 
Xs.srrc=src(as.data.frame(cbind(dvec,gvec,dvec*gvec)),Xs,rank=TRUE) 
Bpeak.srrc=src(as.data.frame(cbind(dvec,gvec,dvec*gvec)),Bpeak,rank=TRUE) 

#compute Standardized Rank Regression Coefficient using package "sensitivity"
#this is a model of the form Y=b1*X1+b2*X2
Bs.srrc2=src(as.data.frame(cbind(dvec,gvec)),Bs,rank=TRUE) 
Xs.srrc2=src(as.data.frame(cbind(dvec,gvec)),Xs,rank=TRUE) 
Bpeak.srrc2=src(as.data.frame(cbind(dvec,gvec)),Bpeak,rank=TRUE) 


print(sprintf('PRCC for Bs and d: %f',Bs.prcc$PRCC[1,1]));
print(sprintf('SRRC for Bs and d: %f',Bs.srrc$SRRC[1,1]));
print(sprintf('SRRC2 for Bs and d: %f',Bs.srrc2$SRRC[1,1]));
cat("\n")
print(sprintf('PRCC for Bs and g: %f',Bs.prcc$PRCC[2,1]));
print(sprintf('SRRC for Bs and g: %f',Bs.srrc$SRRC[2,1]));
print(sprintf('SRRC2 for Bs and g: %f',Bs.srrc2$SRRC[2,1]));
cat("\n")
print(sprintf('PRCC for Xs and d: %f',Xs.prcc$PRCC[1,1]));
print(sprintf('SRRC for Xs and d: %f',Xs.srrc$SRRC[1,1]));
print(sprintf('SRRC2 for Xs and d: %f',Xs.srrc2$SRRC[1,1]));
cat("\n")
print(sprintf('PRCC for Xs and g: %f',Xs.prcc$PRCC[2,1]));
print(sprintf('SRRC for Xs and g: %f',Xs.srrc$SRRC[2,1]));
print(sprintf('SRRC2 for Xs and g: %f',Xs.srrc2$SRRC[2,1]));
cat("\n")
print(sprintf('PRCC for Bpeak and d: %f',Bpeak.prcc$PRCC[1,1]));
print(sprintf('SRRC for Bpeak and d: %f',Bpeak.srrc$SRRC[1,1]));
print(sprintf('SRRC2 for Bpeak and d: %f',Bpeak.srrc2$SRRC[1,1]));
cat("\n")
print(sprintf('PRCC for Bpeak and g: %f',Bpeak.prcc$PRCC[2,1]));
print(sprintf('SRRC for Bpeak and g: %f',Bpeak.srrc$SRRC[2,1]));
print(sprintf('SRRC2 for Bpeak and g: %f',Bpeak.srrc2$SRRC[2,1]));
 

 
###################################################################
#end main program
###################################################################