############################################################
##a stochastic model for influenza infection with drug treatment and resistance emergence
##this script runs a loop over different treatment efficacy values and records the number of emergence events
##written by Andreas Handel (ahandel@uga.edu), last change 6/1/11
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
library(adaptivetau) #uses the package adaptivetau to perform the stochastic simulation

#specify transition rates as function. Corresponds to the following propensity vector (in GillesiepSSA notation)
#pvec = c("b*U*Vs", "b*U*Vr", "delta*Is", "delta*Ir", "(1-e)*(1-mu)*p*Is", "clear*Vs", "(1-e)*mu*p*Is+(1-f)*p*Ir", "clear*Vr")  
transrates = function(x, par, t)
{
         rates=c(par[1]*x[1]*x[4], 
                 par[1]*x[1]*x[5], 
                 par[2]*x[2], 
                 par[2]*x[3], 
                 (1-par[3])*(1-par[4])*par[5]*x[2], 
                 par[6]*x[4], 
                 (1-par[3])*par[4]*par[5]*x[2]+(1-par[7])*par[5]*x[3], 
                 par[6]*x[5])
          return(rates)
}

#the seed initializes the random number generator to a specific value. For the same seed, the same sequence of (pseudo)-random numbers is returned
#this is very important to allow for reproducibility/model comparison/debugging/etc.
#if you change this number, a different sequence of random numbers will be produced, leading to different simulation results
set.seed(111) 

Y0 = c(U=1e6, Is=0, Ir=0, Vs=100, Vr=0)      # Initial state vector
M = matrix(c(-1,-1,0,0,0,0,0,0,  1,0,-1,0,0,0,0,0,  0,1,0,-1,0,0,0,0, 0,0,0,0,1,-1,0,0,  0,0,0,0,0,0,1,-1),nrow=5, ncol=8, byrow=TRUE) # State-change matrix
tmax = 500           # Final time (in days)

ploton=0; #you could plot the results for each run, but you really only want it to see if something goes wrong, otherwise it slows down things too much
samplemax=10; #number of runs for a given value of e
effvec=seq(0,1,by=0.1) #values of drug efficacy for which we run the simulation
emergefrac1=rep(0,length(effvec)); emergefrac2=emergefrac1; #create vectors that will contain the emergence probability values for the two approaches
ct=1; #a counter to allow indexing of the emergefrac vector
vlevel=1e5; #value for which we consider the resistant virus to have emerged
for (eff in effvec)
{
    print(sprintf('starting run for efficacy %f',eff))
    # Define parameters (model is in units of day)
    parms = c(b=1/Y0[1], delta=1, e=eff, mu=1e-4, p=30, clear=10, f=0.1)
    
    #calculate and display R0 for drug sensitive and resistant virus 
    Rs=parms[1]*(1-parms[3])*parms[5]*Y0[1]/(parms[2]*parms[6]);
    Rr=parms[1]*(1-parms[7])*parms[5]*Y0[1]/(parms[2]*parms[6]);
    print(sprintf('Rs=%e; Rr=%e;',Rs,Rr));

    emergect1=0; emergect2=0; #these count the number of emergence events that occur during the samplemax runs
    for (n in 1:samplemax)
    {
      #print(sprintf('run %d/%d for efficacy level %f',n,samplemax,eff)) #if "nothing happens" on the screen for too long and you become impatient, you can uncomment this to see if/what is going on
      out=ssa.adaptivetau(Y0,M,transrates,parms,tmax)

      if (ploton==1)
      {
        plot(out[,1],out[,2],col="black",xlim=c(0,tmax),ylim=c(0,1e3))
        points(out[,1],out[,3],col="green")
        points(out[,1],out[,4],col="blue")
        points(out[,1],out[,5],col="red")
        points(out[,1],out[,6],col="orange")
        legend('topright',legend=c('U','Is','Ir','Vs','Vr'),col=c('black','green','blue','red','orange'),lwd=2)
      }

      fv=length(out[,1]) #get length of solution matrix
      #this checks if at the end of the simulation, ie. at tmax, all infected cells and virions are actually gone. If not, the infection is not yet fully over.
      if (sum(out[fv,-(1:2)])>0) { print(sprintf('at sample %d, there are still infected cells or virions left, increase simulation time.',n)); }

      #this is one definition of emergence: if the resistant virus reaches levels higher than the sensitive virus at any point during the infection, we consider it emerged
      if (max(out[,6]-out[,5])>0) {emergect1=emergect1+1; } 
      #record a second definition of emergence: if the resistant strain reaches levels higher than vlevel, we consider it emergence. record this in emergect2
      if (max(out[,6])>vlevel) {emergect2=emergect2+1; } 
      
    }
    #for each value of drug effectiveness, report fraction of emergence events
    emergefrac1[ct]=emergect1/samplemax; 
    #record fraction of emergence event according to our 2nd definition of emergence in the vector emergefrac2
    emergefrac2[ct]=emergect2/samplemax; 
    ct=ct+1;
}

#produce a figure that plots the result, i.e. the two types of emergence as function of drug efficacy
plot(effvec,emergefrac1,xlab="drug efficacy",ylab="probability of resistance emergence",type="b",pch=19,lwd=1,lty=2,xlim=c(0,1),ylim=c(0,1))
points(effvec,emergefrac2,type="b",pch=18,lwd=1,lty=2,col="blue")
legend('topright',legend=c('Vr>Vs','Vr>Vlevel'),col=c("black","blue"),pch=c(19,18),cex=1.3)
                              
###################################################################
#end  program
###################################################################                     
