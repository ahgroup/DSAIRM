############################################################
##a model for HIV infection dynamics, based on De Boer 2007 J Vir
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
  	Utc=y[1]; Etc=y[2]; Itc=y[3]; nCTL=y[4]; aCTL=y[5];  #uninfected cells, latent infected cells, productively infected cells, non-activated CTL, activated (effector) CTL
    lambda=parms[1]; d=parms[2]; b=parms[3]; delta=parms[4]; p=parms[5]; clear=parms[6];  #model parameters
	  g=parms[7]; dE=parms[8]; dY=parms[9]; k=parms[10]; a=parms[11]; m=parms[12]; hb=parms[13]; hk=parms[14]; ha=parms[15]; hm=parms[16]; 
	  	  
    #virus equation, not a differential equation. 
    #We could replace V in the equations below by this expression directly, but it's a bit easier to follow if we keep it separate 
    V=p/clear*Itc; 
    
	  #these are the differential equations
		dUtcdt=lambda-d*Utc-b*V*Utc/(hb+Utc);
		dEtcdt=b*V*Utc/(hb+Utc)-g*Etc-dE*Etc;
    dItcdt=g*Etc-delta*Itc-k*Itc*aCTL/(hk+Itc+aCTL);
		dnCTLdt=-a*nCTL*Itc/(ha+nCTL+Itc);
		daCTLdt=a*nCTL*Itc/(ha+nCTL+Itc)+m*Itc*aCTL/(hm+aCTL+Itc)-dY*aCTL;
		
		return(list(c(dUtcdt,dEtcdt,dItcdt,dnCTLdt,daCTLdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
  tmax=100;              
  timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated (from 0 to 50 days in steps of 0.1)
	
  #values for model parameters, units are assumed to be 1/days
  #Rob uses different letters for his parameters, I listed the equivalent ones in his model as comments
	lambda=1e8; #de Boer's sigma
	d=0.01;     #de Boer's dT
	b=0.63;     #de Boer's beta
  delta=1;    #de Boer's delta
	p=10;       #de Boer's scaled p
	clear=1;    #de Boer only uses a p which is our p/clear 
  g=1;        #de Boer's gamma
  dE=0.02;    #de Boer's d
  dY=0.5;     #de Boer's dE
  k=10;
  m=1.5;
  hb=1e8;     
  hk=1e5;     
  ha=1e3;
  hm=1e3;
    
	#set initial condition 
	Utc0=1e10; #initial number of uninfected cells  
	Etc0=0; #initial number of infected cells 
	Itc0=1; #initial number of infected cells 
  aCTL0=0; #initial number of activated effector CTL
 				
	#call ode-solver to integrate ODEs without CTL dynamics 
  a=0; #rate of CTL activation
  nCTL0=1e3; #initial number of non-activated CTL
  parms=c(lambda,d,b,delta,p,clear,g,dE,dY,k,a,m,hb,hk,ha,hm); #vector of parameters which is sent to the ODE function
  Y0=c(Utc0, Etc0, Itc0, nCTL0, aCTL0);  #combine initial conditions into a vector 
	odeoutput1=lsoda(Y0,timevec,odeequations,parms,atol=1e-5);
  
  #call ode-solver to integrate ODEs with the CTL dynamics - parameters are set to correspond to Fig. 1e in De Boer 2007 J Vir 
  a=0.1; #rate of CTL activation
  nCTL0=1e3; #initial number of non-activated CTL
  parms=c(lambda,d,b,delta,p,clear,g,dE,dY,k,a,m,hb,hk,ha,hm); 
  Y0=c(Utc0, Etc0, Itc0, nCTL0, aCTL0);  #combine initial conditions into a vector 
  odeoutput2=lsoda(Y0,timevec,odeequations,parms,atol=1e-5);
  
  #call ode-solver to integrate ODEs with the CTL dynamics & vaccination - parameters are set to correspond to Fig. 2c in De Boer 2007 J Vir
  #vaccination increases rate of CTL activation and initial number of CTL
  a=1; #rate of CTL activation
  nCTL0=1e4; #initial number of non-activated CTL
  Y0=c(Utc0, Etc0, Itc0, nCTL0, aCTL0);  #combine initial conditions into a vector 
  parms=c(lambda,d,b,delta,p,clear,g,dE,dY,k,a,m,hb,hk,ha,hm); 
  odeoutput3=lsoda(Y0,timevec,odeequations,parms,atol=1e-5);
  

  #if the plot looks too crowded in the plot window inside RStudio,
  #uncomment the following line to open a new plotting window
  #windows(width=8,height=8)

  layout(matrix(c(1,2,3,4),ncol=2, byrow=T))
    
  #plot results
  #note that we plot virus load instead of infected cells by multiplying I by p/c 
  
  ymin=1e0; ymax=1e12;
  #model without CTL immune response
  plot(odeoutput1[,1],odeoutput1[,2],type="l",xlab="time (days)",ylab="",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax),main="no CTL")
  lines(odeoutput1[,1],p/clear*odeoutput1[,4],type="l",col="red",lwd=2) #multiply by p/clear to plot virus instead of infected cells
  lines(odeoutput1[,1],odeoutput1[,5],type="l",col="blue",lwd=2)
  lines(odeoutput1[,1],odeoutput1[,6],type="l",col="black",lwd=2)
  
  #model with CTL immune response, no vaccination
  plot(odeoutput2[,1],odeoutput2[,2],type="l",xlab="time (days)",ylab="",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax),main="no vaccination")
  lines(odeoutput2[,1],p/clear*odeoutput2[,4],type="l",col="red",lwd=2)
  lines(odeoutput2[,1],odeoutput2[,5],type="l",col="blue",lwd=2)
  lines(odeoutput2[,1],odeoutput2[,6],type="l",col="black",lwd=2)

  #model with CTL immune response, with vaccination
  plot(odeoutput3[,1],odeoutput3[,2],type="l",xlab="time (days)",ylab="",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax),main="with vaccination")
  lines(odeoutput3[,1],p/clear*odeoutput3[,4],type="l",col="red",lwd=2)
  lines(odeoutput3[,1],odeoutput3[,5],type="l",col="blue",lwd=2)
  lines(odeoutput3[,1],odeoutput3[,6],type="l",col="black",lwd=2)
  legend(20,1e10, c("U","V","nCTL","aCTL"),col = c("green","red","blue","black"),lwd=2)
  
  #virus and CTL for model with CTL immune response, vaccination and no vaccination
  plot(odeoutput3[,1],p/clear*odeoutput3[,4],type="l",lty=2,xlab="time (days)",ylab="",col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(ymin,ymax),main="comparison")
  lines(odeoutput3[,1],odeoutput3[,6],type="l",col="green",lwd=2)
  lines(odeoutput2[,1],p/clear*odeoutput2[,4],type="l",lty=2,col="red",lwd=2)
  lines(odeoutput2[,1],p/clear*odeoutput2[,6],type="l",col="red",lwd=2)
  legend(20,1e10, c("vaccinated V","CTL","non-vaccinated V","CTL"),col = c("green","green","red","red"),lwd=2,lty=c(2,1,2,1))
 
###################################################################
#end main program
###################################################################                   
