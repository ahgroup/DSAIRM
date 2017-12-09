############################################################
##a model for HCV infection with interferon treatment, taking into account PK/PD  
##written by Andreas Handel (ahandel@uga.edu), last change 6/20/13
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
  	Utc=y[1]; Itc=y[2]; Vir=y[3]; Drug=y[4];  #uninfected cells, infected cells, virus, drug
    lambda=parms[1]; d=parms[2]; b=parms[3]; delta=parms[4]; p=parms[5]; clear=parms[6]; Ddecay=parms[7]; D50=parms[8]; n=parms[9]; #model parameters
	 
	  #these are the differential equations
		e=Drug^n/(Drug^n+D50);
    dUtcdt=lambda-d*Utc-b*Vir*Utc;
		dItcdt=b*Vir*Utc-delta*Itc;
		dVirdt=(1-e)*p*Itc-clear*Vir;
		dDrugdt=-Ddecay*Drug;
 
		return(list(c(dUtcdt,dItcdt,dVirdt,dDrugdt))); 

} #end function specifying the ODEs

###################################################################
#main program
###################################################################
  
  #values for model parameters, units are assumed to be 1/days
	lambda=1e4;
	d=0.001;
	b=3e-7;
  delta=0.2;
	p=1e2;
	clear=6;

  #set initial condition to NON-steady state values 
	Utc0=1e7; #initial number of uninfected cells  
	Vir0=10; #initial number for free virus V 
  Itc0=0; #initial number of infected cells 
	Drug0=0; #initial level of drug concentration
  Y0=c(Utc0, Itc0, Vir0, Drug0);  #combine initial conditions into a vector 
  
	#PK/PD parameters
  maxintervals=4; #number of treatment episodes to consider
	tdosing=7; #integration for time between consecutive drug administrations              
  Ddecay=0.2; #rate at which drug concentration decays
	D50=2e3; #drug dose concentration at which it is 50% effective 
	Drugdose=1e4; #drug dose given each tmax days, arbitrary scale
  n=1; #parameter that determines steepness of efficacy increase	
	
	parms=c(lambda,d,b,delta,p,clear,Ddecay,D50,n); #vector of parameters which is sent to the ODE function
	  
  #call ode-solver lsoda to integrate for some time until steady state is reached, without drug present
  timevec=seq(0,100,0.1); #vector of times for which integration is evaluated
	odeoutput=lsoda(Y0,timevec,odeequations,parms);
  Nodrugdata=odeoutput; #create an extra array that contains all time course data
  
	#call ode-solver lsoda to integrate between drug administrations. 
  #the increase in drug concentration is best done outside the ODE integration and then used as new initial condition for the next integration interval
  Drugdata=matrix(nrow=0,ncol=5); #set up an empty matrix that will contain all the time series data for the drug-treatment portion
  for (intervals in 1:maxintervals)
  {
    Y0=odeoutput[length(odeoutput[,1]),2:5]; #use final result from initial integration as new starting condition
    Y0[4]=Y0[4]+Drugdose; #administer drug
    tf=odeoutput[length(odeoutput[,1]),1]; #final time of previous integration
    timevec=seq(tf,tf+tdosing,0.1); #new time vector for integration
    odeoutput=lsoda(Y0,timevec,odeequations,parms); #integrate next interval up to point of new drug dose administration
    Drugdata=rbind(Drugdata,odeoutput);
  }
  
  #plot results
  par(mfrow=c(2,1), las=1, cex=1)     #split plotting area into several subplots
  
  #results without drug until steady state is reached
  plot(Nodrugdata[,1],Nodrugdata[,4],type="l",xlab="time (days)",ylab="virus",main="initial virus dynamics before treatment start",col="blue",lwd=2,log="y",xlim=c(0,Nodrugdata[length(Nodrugdata[,1]),1]),ylim=c(1e3,1e8))
  
  #results for drug treatment bit
  #A number of options are set in the plot command that you might not have encountered before
  #see ?plot and ?par in the R help file to learn what they do
  par(mar=c(5, 5, 4, 2)) #this changes margins for plot
  plot(Drugdata[,1],Drugdata[,4],type="l",xlab="time (days)",ylab="",main="virus dynamics after treatment start",col="blue",lwd=2,log="y",xlim=c(Drugdata[1,1],Drugdata[length(Drugdata[,1]),1]),ylim=c(1e3,1e7),cex.axis=1.3,cex.lab=1.3,cex.main=1.5)
  lines(Drugdata[,1],Drugdata[,5],type="l",col="red",lwd=2)
  legend(Drugdata[1,1]+10,5e6, c("virus","drug"),col = c("blue","red"),lwd=2)

  #this command prints the result figure into a png file - uncomment it if you want to save the plot as png file
  #dev.copy(device=png,width=600,height=600,file="HCV-PKPD.png"); dev.off();
  
###################################################################
#end main program
###################################################################                       
