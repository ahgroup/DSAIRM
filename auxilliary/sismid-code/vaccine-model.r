############################################################
#a simple model for a bacteria infection, used to illustrate model analysis
##written by Andreas Handel, ahandel@uga.edu, last change 6/20/14
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows 
library(deSolve)  #loads ODE solver package
graphics.off()

#functions come first, main program below

###################################################################
#function that specificies the ode model called by lsoda (the ode solver) 
###################################################################
odeequations=function(t,y,parameters) 
{ 
  	Ag=y[1]; I=y[2]; B=y[3]; A=y[4]; #bacteria and immune response

  	c=parameters[1]; 
  	k=parameters[2]; 
  	d=parameters[3]; 
  	r=parameters[4]; 
  	p=parameters[5]; 
  	dA=parameters[6]; 
  	
	  #these are the differential equations
		dAgdt=-c*Ag
		dIdt=k*Ag-d*I
		dBdt=r*I*B
    dAdt=p*B-dA*A
		  
		return(list(c(dAgdt,dIdt,dBdt,dAdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################

c=1; d=2; k=10; r=1e-3; p=10; dA=0.1;
parameters=c(c,k,d,r,p,dA); 

I0=0; B0=1; A0=0;
samplemax=20;
Agvec=seq(1,1000,length=samplemax)
                
tmax=30;  #number of days
timevec=seq(0,tmax,0.05); #vector of times for which integration is evaluated 

ploton=1; #set this to 1 if you want to see the time-series for every parameter sample. Leave it at zero to make the code run faster.
 
#####################

Afinal=rep(0,samplemax); #initialize vector that will contain the solution
	
#this is the loop over samplemax samples for the parameter values
for (nsample in 1:samplemax)
{
	    print(sprintf('Starting Simulation %d of %d',nsample,samplemax));
	   
      #values for sampled parameters 
      Ag0=Agvec[nsample];
	  
      Y0=c(Ag0,I0,B0,A0)			
	    #call ode-solver to integrate ODEs
      #see the documentation of the deSolve package to learn the difference between this solver and lsoda
	    #I'm using vode instead of lsoda since I found that lsoda occasionally failed for this problem
      odeoutput=vode(Y0,timevec,odeequations,parameters,mf=22,atol=1e-12,rtol=1e-12,hini=1e-10);
      vl=length(odeoutput[,1]);
      
      #we can plot time series every time if we want, just to watch what happens - is not necessary to do so
      if (ploton==1)
      {
        plot(odeoutput[,1],odeoutput[,2],col="red",type="l",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,max(odeoutput)),xlab="time (days)",ylab="")
        lines(odeoutput[,1],odeoutput[,3],col="blue",lwd=2)
        lines(odeoutput[,1],odeoutput[,4],col="green",lwd=2)
        lines(odeoutput[,1],odeoutput[,5],col="black",lwd=2)
        legend("topright",c("Ag","I","B","A"),col=c("red","blue","green",'black'),lwd=2)
        #browser()
      }
            
      #record the peak value for B   
      Afinal[nsample]=odeoutput[nrow(odeoutput),5]
 }
#produce result plot
plot(Agvec,Afinal,log="",pch=20,col="blue",xlab="Antigen level",ylab="Antibodies")



###################################################################
#end main program
###################################################################