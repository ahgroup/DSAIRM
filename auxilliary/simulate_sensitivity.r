############################################################
#a simple model for a TB infection, used to illustrate sensitivty analysis using Correlation Coefficients - complete code
##written by Andreas Handel, ahandel@uga.edu, last change 6/24/13
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows 
require(deSolve)  #loads ODE solver package
require(lhs) #LHS package
require(sensitivity) #package to do Partial Rank Correlation Coefficients

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
timevec=seq(0,tmax,0.5); #vector of times for which integration is evaluated 
	
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
          
          
#transforming g from a uniform to a normal distribution. This is how one can do it in R:
#we assume that g is normally distributed with mean gmean and SD gsd
gmean=0.5; gsd=0.1;
gvec=qnorm(lhssample[,2], mean = gmean, sd = gsd); #this transforms the uniform distribution produced by lhssample to a normal distribution

gvec=pmax(1e-10,gvec) 
#to prevent negative values of g, this sets every value in g that is lower than 1E-10 to 1E-10. This is not a clean way of doing things, one should instead re-sample. 
#But it's quick/easy and justified as long as almost no values in gvec are negative.

#####################

Bs=rep(0,samplemax); #initialize vectors that will contain the solution
Xs=rep(0,samplemax);
	
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
         #browser(); #if you want the code to pause if steady state is not reached 
         #(so you can figure out what's going wrong), uncomment the browser() command.
      }
        
      #writing final values for B and X at the end of the simulation (i.e. the steady state) into the vectors Bs and Xs 
      Bs[nsample]=odeoutput[vl,2];
      Xs[nsample]=odeoutput[vl,3];      
          
        
 }

#if the plot looks too crowded in the plot window inside RStudio,
#uncomment the following line to open a new plotting window
#windows(width=8,height=8) #adjust width and height to get the desired plot size


par(mfrow=c(2,2), las=0, cex=1.2)
plot(dvec,Bs,xlab="d",ylab="Bs",col="blue")
plot(gvec,Bs,xlab="g",ylab="Bs",col="blue")
plot(dvec,Xs,xlab="d",ylab="Xs",col="blue")
plot(gvec,Xs,xlab="g",ylab="Xs",col="blue")
#dev.copy(device=png,width=600,height=600,file="scatterplot.png"); dev.off(); #this saves the scatterplot to a png file

#compute Spearman rank correlation coefficients using core R functionality
d.Bs=cor.test(dvec, Bs, alternative = c("two.sided"), method = c("spearman"))
g.Bs=cor.test(gvec, Bs, alternative = c("two.sided"), method = c("spearman"))
d.Xs=cor.test(dvec, Xs, alternative = c("two.sided"), method = c("spearman"))
g.Xs=cor.test(gvec, Xs, alternative = c("two.sided"), method = c("spearman"))

#compute Partial rank correlation coefficients using package "sensitivity"
Bs.prcc=pcc(as.data.frame(cbind(dvec,gvec)),Bs,rank=TRUE) 
Xs.prcc=pcc(as.data.frame(cbind(dvec,gvec)),Xs,rank=TRUE) 

print(sprintf('SRCC for Bs and d: %f, p-value: %e',d.Bs$estimate,d.Bs$p.value));
print(sprintf('SRCC for Bs and g: %f, p-value: %e',g.Bs$estimate,g.Bs$p.value));
print(sprintf('SRCC for Xs and d: %f, p-value: %e',d.Xs$estimate,d.Xs$p.value));
print(sprintf('SRCC for Xs and g: %f, p-value: %e',g.Xs$estimate,g.Xs$p.value));
 
print(sprintf('PRCC for Bs and d: %f',Bs.prcc$PRCC[1,1]));
print(sprintf('PRCC for Bs and g: %f',Bs.prcc$PRCC[2,1]));
print(sprintf('PRCC for Xs and d: %f',Xs.prcc$PRCC[1,1]));
print(sprintf('PRCC for Xs and g: %f',Xs.prcc$PRCC[2,1]));

 
###################################################################
#end main program
###################################################################