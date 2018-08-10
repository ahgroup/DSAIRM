##################################################################################
#fitting a simple ODE model to artificial data
#this example illustrates identifiability/degeneracy
##written by Andreas Handel, ahandel@uga.edu, last change 6/22/13
##################################################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package
require(nloptr) #for fitting  
 
#################################
#experimental data values from Hayden et al. 1996 JAMA 
#one could load the data from a separate file
#here, for simplicity, we include it as part of the program
#note that for this script, we won't actually use/fit this data
################################
timedata=c(1,     2,    3,    4,    5,    6,    7,    8  ); #days since infection
virusdata=c(0.97, 2.73, 2.72, 1.92, 0.85, 0.58, 0.42, 0.2);  #virus load, in units of log10 

###################################################################
#function specifying the ode model. This is called by the ode solver inside the fit function
###################################################################
odeequations=function(t,y,parameters) 
{ 
  	Utc=y[1]; Itc=y[2]; Vir=y[3];  #uninfected cells, infected cells, virus
	  b=parameters[1]; p=parameters[2]; clear=parameters[3]; #model parameters, passed into function by main program

	  #these are the 3 differential equations
		dUtcdt=-b*Vir*Utc;
		dItcdt=b*Vir*Utc-delta*Itc;
		dVirdt=p*Itc-clear*Vir;
 
		return(list(c(dUtcdt,dItcdt,dVirdt))); 

} #end function specifying the ODEs


###################################################################
#function that fits the ODE model to data 
###################################################################
fitfunction <- function(parameters,timedata,ydata)
{

    #call ode-solver lsoda to integrate ODEs 
    odeoutput=lsoda(Y0,timevec,odeequations,parameters);

   	virusfit=odeoutput[match(timedata,odeoutput[,1]),4];
   
    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming. 
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming 
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,virusfit))); 
  
    #plot data and current fit to watch fitting in real time - not necessary but nice to see it
    if (ploton==1)
    {
      plot(timedata,ydata,type="p",xlim=c(0,8),ylim=c(-2,4));
      lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,4])),col="red")
      lines(timedata,logvirus,type="p",col="red");
    }
     
    #compute the objective function, namely SSR 
    SSR=sum((logvirus-ydata)^2)
     
    return(SSR) #returns the objective function, the sum of squares, which is being minimized by optim 

    
} #end function that fits the ODE model to the data

############################################################
#the main part, which calls the fit function 
############################################################

tstart=proc.time(); #capture current time to measure execution speed

#some general settings
ploton=1; #set this to 1 if you want to see plots during fit. for speed-up, turn it off
if (ploton==1) {windows(height=8,width=8)} #plot in separate window since RStudio is a bit buggy when plotting into its figure window

maxsteps=2000; #max steps the optimizer can take 

tmax=timedata[length(timedata)];
timevec=seq(timedata[1], tmax, 0.1); #vector of times (in units of days) for which integration is evaluated

delta=1; #fixed parameter, clearance rate of infected cells

Vir0=1e-1; #initial condition for free virus
Utc0=4e8; #initial number of uninfected cells 
Itc0=0; #initial number of infected cells
Y0=c(Utc0, Itc0, Vir0);  #combine initial conditions into a vector 

#starting guesses and bounds for the parameters that will be fit
b0=1e-2;  blow=1e-10;    bhigh=1e5;
p0=1e-4;  plow=1e-10;    phigh=1e5;
clear0=10; clearlow=1e-5; clearhigh=1e5; 

lb=c(blow,plow,clearlow);
ub=c(bhigh,phigh,clearhigh);


#run the model once to produce artificial data
par1=c(b=2e-2,p=2e-5,clear=5e0); #set parameters to something. Ideally, the values returned from the fit should be close to these values
odeoutput.artificial=lsoda(Y0,timevec,odeequations,par1); #running model to create some artificial data
modelfit.all=odeoutput.artificial[match(timedata,odeoutput.artificial[,1]),4]; #virus load "data" for the above parameter values
sigma=0.1; #amount of noise added to artificial data
data.artificial=(log10(modelfit.all)+rnorm(length(timedata),sd=sigma)) #normally distributed error added to the log of the data
plot(timedata,log10(modelfit.all),type="p",xlim=c(0,tmax),ylim=c(-1,max(data.artificial,virusdata)),col="red",xlab="time (days)",ylab="virus (log)",lwd=2,cex.axis=1.5,cex.lab=1.5,pch=21,main='artificial data');
points(timedata,data.artificial,type="p",col="blue",lwd=2,cex.axis=1.5,cex.lab=1.5,pch=19);
legend('bottom',c('before noise','after noise'),col=c('red','blue'),pch=c(21,19))
print(sprintf('finished producing artificial data - press Enter to continue')); scan(file="",what="character",nmax=1);

#comment out one of these lines to either fit noiseless artificial data or with noise added
#ydata=log10(modelfit.all); print(sprintf("starting fit using artificial data without noise"));
ydata=data.artificial; print(sprintf("starting fit using artificial data with noise"));

p.ini=c(b0,p0,clear0); #initial conditions as vector
#p.ini=par1; #initial conditions as vector - this uses as initial conditions the values that produced the artificial data. should always lead to a good fit to the artificial data
#fit with bounds using nloptr package. Fitting parameters in log space. Data is fit in linear space.
fitresult <- nloptr(x0=p.ini,eval_f=fitfunction,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=1e-12,lb=lb,ub=ub,maxeval=maxsteps,print_level=0),timedata=timedata,ydata=ydata); 

#extract best fit parameter values and from the result returned by the optimizer
fpar=fitresult$solution; 

#code that checks and warns the user if the fit has not converged and instead stopped for another reason (e.g. because it reached maxsteps iterations)
if (fitresult$status > 4) {cat('Optimization not successful \n');} 

tend=proc.time(); #capture current time to compute total execution time
tdiff=tend-tstart;

print(sprintf('Finished fitting using solver %s in %f seconds',fitresult$call[[4]][[2]],tdiff[3]));


############################################################
#output result
############################################################


#compute model solution for initial and final parameter values
odeoutput.initial=lsoda(Y0,timevec,odeequations,p.ini); 
modelfit.initial=odeoutput.initial[match(timedata,odeoutput.initial[,1]),4]; 
odeoutput.final=lsoda(Y0,timevec,odeequations,fpar); 
modelfit.final=odeoutput.final[match(timedata,odeoutput.final[,1]),4]; 

#plot data and model solution for initial and final parameter values
graphics.off(); #close graphics window     
cxl=1.75; cxa=1.75;
plot(timedata,log10(modelfit.all),type="p",xlim=c(0,tmax+2),ylim=c(0,max(ydata)+1),col="black",xlab="time (days)",ylab="virus (log)",cex.axis=cxa,cex.lab=cxl,pch=18,cex=1.5);
points(timedata,data.artificial,col="gray",type="p",pch=19,cex=1.5)
points(timevec,log10(odeoutput.initial[,4]),col="blue",type="l",lwd=1,lty=1,cex=1)
lines(timevec,log10(odeoutput.final[,4]),col="red",type="l",lwd=1,lty=2,cex=1)
legend('topright', c("noiseless data","noisy data","initial guess","final fit"),col = c("black","gray","blue","red"),lty=c(-1,-1,1,1),pch=c(18,19,-1,-1),lwd=2,cex=1.25)


#play alarm to signal end of simulation
alarm(); Sys.sleep(0.5); alarm(); Sys.sleep(0.5); alarm();

#print initial and final parameter values
#write some code that prints artifical, initial and final values of the parameters that are being fit 
#Note: If we fit the artifical data without noise, our final values returned from the solver 
#should equal the values used to produce the artificial data
print(sprintf('artificial values: b=%e; p=%e; c=%e;',par1[1],par1[2],par1[3]));
print(sprintf('initial values: b=%e; p=%e; c=%e;',p.ini[1],p.ini[2],p.ini[3]));
print(sprintf('final values: b=%e; p=%e; c=%e;',fpar[1],fpar[2],fpar[3]));


###################################################################
#end main program
###################################################################  