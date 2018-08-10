##################################################################################
##fitting influenza virus load data to 2 simple ODE models to perform model comparison
##this script contains AIC calculation/model comparison 
##written by Andreas Handel, ahandel@uga.edu, last change 6/21/14
##################################################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package
require(nloptr) #optimization package
                 
#################################
#experimental data values from Hayden et al. 1996 JAMA 
#one could load the data from a separate file
#here, for simplicity, we include it as part of the program
################################
timedata=c(1,     2,    3,    4,    5,    6,    7,    8  ); #days since infection
virusdata=c(0.97, 2.73, 2.72, 1.92, 0.85, 0.58, 0.42, 0.2);  #virus load, in units of log10 
                                                 
##################################################################################
#defining the functions, then comes the main program
##################################################################################
odeequations=function(t,y,parameters) 
{ 
  	Utc=y[1]; Itc=y[2]; Vir=y[3]; X=y[4];
    
    
    if (model==1) #runs the 1st model
    {
     	 	a=parameters[1];
        r=parameters[2];
        dUtcdt=-b*Vir*Utc;
  		  dItcdt=b*Vir*Utc-delta*Itc-k*X*Itc;
  		  dVirdt=p*Itc-clear*Vir;
        dXdt=a*Vir+r*X
    }
    if (model==2) #runs the 2nd model
    {
     	 a=parameters[1];
  	   w=parameters[2];
  	   k=parameters[3];
       dUtcdt=-b*Vir*Utc;
    	 dItcdt=b*Vir*Utc-delta*Itc;
    	 dVirdt=p*Itc-clear*Vir-k*X*Vir;
       dXdt=a*Vir*X-w*X
    }
  	return(list(c(dUtcdt,dItcdt,dVirdt,dXdt))); 
} #end function specifying the ODEs


###################################################################
#function that fits the ODE model to data 
###################################################################
fitfunction <- function(parameters,timedata,virusdata)
{
    	
    #call ode-solver lsoda to integrate ODEs 
    
  odeoutput=lsoda(Y0,timevec,odeequations,parms=parameters,atol=atolv,rtol=rtolv); #
  #odeoutput=try(lsoda(Y0,timevec,odeequations,parms=parameters,atol=atolv,rtol=rtolv)); #
  #if (length(odeoutput)==1) {cat('!!unresolvable integrator error - triggering early return from optimizer!!'); return(1e10) }
  #try command catches error from lsoda. If error occurs and lsoda "breaks", we exit the whole optimizer routine with a high objective function value
 
  virusfit=odeoutput[match(timedata,odeoutput[,1]),4]; #extract values for virus load at time points corresponding to values in timedata 

    #since the ODE returns values on the original scale, we need to transform it into log10 units for the fitting procedure
    #due to numerical issues in the ODE model, virus might become negative, leading to problems when log-transforming. 
    #Therefore, we enforce a minimum value of 1e-10 for virus load before log-transforming 
    #fitfunction returns the log-transformed virus load obtained from the ODE model to the nls function
    logvirus=c(log10(pmax(1e-10,virusfit))); 
  
    #plot data and current fit to watch fitting in real time - not necessary but nice to see it
    if (ploton==1)
    {
      plot(timedata,10^virusdata,type="p",xlim=c(0,8),ylim=c(1,1e9),log="y");
      lines(odeoutput[,1],odeoutput[,2],col="blue")
      lines(odeoutput[,1],odeoutput[,3],col="green")
      lines(odeoutput[,1],odeoutput[,4],col="red")
      lines(odeoutput[,1],odeoutput[,5],col="orange")
      points(timedata,10^logvirus,col="red");
      legend('topright',c("data","U","I","V","X"),col=c("black","blue","green","red","orange"),lwd=2)
    }
    #return the objective function, the sum of squares, which is being minimized by optim 
    return(sum((logvirus-virusdata)^2))
    
} #end function that fits the ODE model to the data

############################################################
#the main part, which calls the fit function 
############################################################

maxsteps=3000; #maximum number of iterations for the optimization routine 

atolv=1e-4; rtolv=1e-4; #accuracy settings for the ODE solver routine

V0=1e-2; #fix initial virus inoculum
Utc0=4e8; #initial number of uninfected cells 
Itc0=0; #initial number of infected cells
X0=1; #starting value for immune response, arbitrary value
Y0=c(Utc0, Itc0, V0, X0);  #combine initial conditions into a vector 
timevec=seq(0, 8,0.1); #vector of times for which integration is evaluated

#starting guesses and bounds for the parameters that will be fit
a0=1;  alow=1e-3;   ahigh=1e3;
r0=1;   rlow=1e-3;  rhigh=10; 
w0=1e-3;   wlow=1e-5;   whigh=1e2;
k0=0.1; klow=0.01;   khigh=1e2;
   
ploton=0; #turns plotting on or off

print(sprintf("starting to fit 1st model"));
model=1;
b=5e-3; delta=1; p=1e-4; clear=1; k=0.1; #fixed parameters

p.ini1=c(a=a0,r=r0);
lb1=c(alow,rlow)
ub1=c(ahigh,rhigh)
fitresult1 <- nloptr(x0=p.ini1,eval_f=fitfunction,lb=lb1,ub=ub1,opts=list("algorithm"="NLOPT_LN_COBYLA",xtol_rel=1e-10,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata); 
#extract best fit parameter values and from the result returned by the optimizer
finalparams1=fitresult1$solution; 



odeoutputfinal1=lsoda(c(4e8,0,V0,X0),seq(0, 8,0.1),odeequations,finalparams1); #compute model solution for final parameter values
print(sprintf('finished fitting 1st model')); 


print(sprintf("starting to fit 2nd model"));
model=2;
b=5e-5; delta=1; p=1e-2; clear=1; #fixed parameters
p.ini2=c(a=a0,w=w0,k=k0);
lb2=c(alow,wlow,klow)
ub2=c(ahigh,whigh,khigh)

fitresult2 <- nloptr(x0=p.ini2,eval_f=fitfunction,lb=lb2,ub=ub2,opts=list("algorithm"="NLOPT_LN_COBYLA",xtol_rel=1e-10,maxeval=maxsteps,print_level=0),timedata=timedata,virusdata=virusdata); 
finalparams2=fitresult2$solution; 

odeoutputfinal2=lsoda(c(4e8,0,V0,X0),seq(0, 8,0.1),odeequations,finalparams2); #compute model solution for final parameter values 
print(sprintf('finished fitting 2nd model')); 

############################################################
#output result
############################################################

#compute sum of square residuals (SSR) for initial guess and final solution
logvirusfinal1=log10(odeoutputfinal1[seq(11,90,10),4]);
ssrfinal1=sum((logvirusfinal1-virusdata)^2);
logvirusfinal2=log10(odeoutputfinal2[seq(11,90,10),4]);
ssrfinal2=sum((logvirusfinal2-virusdata)^2);

#--> write code to compute AICc for both models, call those values AICc1 and AICc2
N=length(virusdata) #number of datapoints         
K1=length(p.ini1); #fitted parameters for model 1
K2=length(p.ini2); #fitted parameters for model 2
AICc1=N*log(ssrfinal1/N)+2*(K1+1)+(2*(K1+1)*(K1+2))/(N-K1)
AICc2=N*log(ssrfinal2/N)+2*(K2+1)+(2*(K2+1)*(K2+2))/(N-K2)

print(sprintf('final values for model 1: a=%e; r=%e; SSR=%f; AIC=%f',finalparams1[1],finalparams1[2],ssrfinal1,AICc1));
print(sprintf('final values for model 2: a=%e; w=%e; k=%e; SSR=%f; AIC=%f',finalparams2[1],finalparams2[2],finalparams2[3],ssrfinal2,AICc2));

if ((AICc1+10)<AICc2) {print(sprintf('Model 1 is statistically better')) }
if ((AICc2+10)<AICc1) {print(sprintf('Model 2 is statistically better')) }
if (abs(AICc2-AICc1)<2) {print(sprintf('Models are essentially statistically equally good')) }
if (abs(AICc2-AICc1)>2 & abs(AICc2-AICc1)<10) {print(sprintf('Things are murky, hard to say which model is statistically better')) }

#plot final results
plot(timedata,10^virusdata,type="p",xlim=c(0,8),ylim=c(1,1e9),log="y");
lines(odeoutputfinal1[,1],odeoutputfinal1[,4],col="blue")
lines(odeoutputfinal2[,1],odeoutputfinal2[,4],col="green")
legend('topright',c("data","model 1","model 2"),col=c("black","blue","green"),lwd=2)


###################################################################
#end main program
###################################################################