############################################################
##a model for a persistent virus infection
##the model includes a delay between infection of cells and virus production
#delay is implemented two ways. Once with the DDE solver (dede) in the deSolve package, once with dummy compartments
##written by Andreas Handel, ahandel@uga.edu, last change 6/27/12
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package

#functions come first, main program below

###################################################################
#function that specificies the DDE model  
###################################################################
ddeequations=function(t,y,parms) 
{ 
  	#uninfected cells, productively infected cells, dead cells, virus
		#note the n1 compartments for Itc
    Utc=y[1]; #uninfected cells
    Itc=y[2]; #infected cells
    Dtc=y[3]; #dead cells
    Vir=y[4]; #virus  
    lambda=parms[1]; d=parms[2]; b=parms[3]; delta=parms[4]; p=parms[5]; clear=parameters[6];  #model parameters
    tau=parms[7]; #lag

    if (t < tau)
	 	   Ilag = 0 #if simulation has run less than tau time, no infected cells produce virus
    else
       Ilag = lagvalue(t - tau, 2) #getting y[2], i.e. infected cells, tau timesteps in the past
	 	 
	  #these are the differential equations
		dUtcdt=lambda-d*Utc-b*Vir*Utc;
		dItcdt=b*Vir*Utc-delta*Itc; #1st infected
		dDtcdt=delta*Itc; #dead cells
		dVirdt=p*Ilag - clear*Vir; #cells infected tau timesteps back are producing virus 
 
		return(list(c(dUtcdt,dItcdt,dDtcdt,dVirdt))); 

} #end function specifying the ODEs


###################################################################
#function that specificies the ode model with dummy compartments 
###################################################################
odeequations=function(t,y,parms) 
{ 
  	#uninfected cells, productively infected cells, dead cells, virus
		#note the n1 compartments for Itc
    Utc=y[1]; #uninfected cells
    Itc=y[2:(n1+1)]; #n1 dummy compartments for infected cells
    Ip=y[n1+2]; #last infected cell compartment, the one that produces virus
    Dtc=y[n1+3]; #dead cells
    Vir=y[n1+4]; #virus  
    lambda=parms[1]; d=parms[2]; b=parms[3]; delta=parms[4]; p=parms[5]; clear=parameters[6]; g=parameters[7];  #model parameters
	 	 
	  dItcdt=rep(0,n1); #initialize infected compartments
	  
    #these are the differential equations
		dUtcdt=lambda-d*Utc-b*Vir*Utc;
		dItcdt[1]=b*Vir*Utc-n1*g*Itc[1]; #1st infected cell dummy compartment
		dItcdt[2:n1]=n1*g*Itc[1:(n1-1)]-n1*g*Itc[2:n1]; #infected cell dummy compartments
    dIpdt=n1*g*Itc[n1]-delta*Ip; #infected cell compartment that produces virus
  	dDtcdt=delta*Ip; #dead cells
		dVirdt=p*Ip-clear*Vir; #only the last compartment of I produces virus for a duration of 1/delta 
 
		return(list(c(dUtcdt,dItcdt,dIpdt,dDtcdt,dVirdt))); 

} #end function specifying the ODEs



###################################################################
#main program
###################################################################
 
          
  Utc0=1e8; #initial number of uninfected cells 
	Itc0=0; #initial number of productively infected cells, n1 compartments
	Dtc0=0; #initial number of dead cells
	Vir0=10; #initial condition for free virus V
  Y0=c(Utc0, Itc0, Dtc0, Vir0);  #combine initial conditions into a vector 
  
  #values for model parameters, units are assumed to be 1/days
  d=0.01; #natural death rate of unifected cells
  lambda=d*Utc0;
  b=1e-5;
	delta=1; #1/lifespan of infected, virus-produceing cell
	p=5;
	clear=10;

  #parameter specific for DDE model
	tau=2; #lag in units of days
	parameters=c(lambda,d,b,delta,p,clear,tau); #vector of parameters which is sent to the ODE function
	
	tmax=10;
	timevec=seq(0,tmax,0.1); #vector of times for which integration is evaluated (from 0 to 50 days in steps of 0.1)
			
	#call DDE-solver to integrate equations 
  output=dede(y=Y0,times=timevec,func=ddeequations,method="bdf",parameters);
  
  tvec=output[,1];
  Utc=output[,2];
  Itc=output[,3];  #sum all dummy compartments to get the total number of infected cells
  Dtc=output[,4]; 
  Vir=output[,5];  

  #plot results from DDE run
  windows(width=6,height=6)
  par(mfrow=c(2,1), las=1, cex=1)     #split plotting area into several subplots
  titletext=paste("tau (days)=",as.character(tau),sep=""); #this produces a text string which automatically shows the delay
  plot(tvec,Utc,type="l",xlab="time (days)",ylab="",main=titletext,col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,1e9),asp=1)
  lines(tvec,Itc,type="l",col="red",lwd=2)
  lines(tvec,Dtc,type="l",col="black",lwd=2)
  lines(tvec,Vir,type="l",col="blue",lwd=2)
  legend("bottomright",c("uninfected","producing","dead","virus"),col = c("green","red","black","blue"),lwd=2,cex=0.7,y.intersp=0.8)


  #call ode-solver to integrate ODEs 
  #parameters specific to dummy compartment model
 	g=1/tau; #1/duration of latent period before virus production starts
  n1=5; #number of dummy compartments for productively infected cells - minimum is 2   
	Itc0=rep(0,n1); #initial number of productively infected cells, n1 compartments
  Ip0=0; #last compartment for infected cells
  Y0=c(Utc0, Itc0, Ip0, Dtc0, Vir0);  #combine initial conditions into a vector 

 	parameters=c(lambda,d,b,delta,p,clear,g); #vector of parameters which is sent to the ODE function
  odeoutput=lsoda(Y0,timevec,odeequations,parameters);
  
  tvec=odeoutput[,1];
  Utc=odeoutput[,2];
  Itc=rowSums(odeoutput[,3:(n1+2)]);  #sum all dummy compartments to get the total number of infected, non-virus producing cells
  Ip=odeoutput[,n1+3]; 
  Dtc=odeoutput[,n1+4]; 
  Vir=odeoutput[,n1+5];  

  #plot results from ODE run
  titletext=paste("n1=",as.character(n1),sep=""); #this produces a text string which automatically shows the number of compartments used
  plot(tvec,Utc,type="l",xlab="time (days)",ylab="",main=titletext,col="green",lwd=2,log="y",xlim=c(0,tmax),ylim=c(1,1e9),asp=1)
  lines(tvec,Itc,type="l",col="orange",lwd=2)
  lines(tvec,Ip,type="l",col="red",lwd=2)
  lines(tvec,Dtc,type="l",col="black",lwd=2)
  lines(tvec,Vir,type="l",col="blue",lwd=2)
  legend("bottomright",c("uninfected","latent","producing","dead","virus"),col = c("green","orange","red","black","blue"),lwd=2,cex=0.7,y.intersp=0.8)
 

###################################################################
#end main program
###################################################################  
                                         
