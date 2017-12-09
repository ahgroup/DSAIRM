############################################################
##a discrete-time model for a simple within-host virus infection model
##noise/stochasticity is added to the equations
##written by Andreas Handel (ahandel@uga.edu), last change 6/4/11 
############################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows

############################################################
#note that this simulation could be programmed much more compactly, by for instance doing both runs in a single loop
#or not even using a loop and instead using the vector capabilites of R
#I choose to write it this way to make it a bit easier to understand
#but if speed (CPU time) were important, you should rewrite it in vectorized form
############################################################

#the seed initializes the random number generator to a specific value. For the same seed, the same sequence of (pseudo)-random numbers is returned
#this is very important to allow for reproducibility/model comparison/debugging/etc.
#if you change this number, a different sequence of random numbers will be produced, leading to different simulation results
set.seed(111) 

tmax=20   # Final time (in days)
tau=0.1   #time step
tvec=seq(0,tmax,by=tau) #vector of times
b=1e-4; d=2; clear=10; p=100; #model parameters

U=rep(0,length(tvec)); Ic=U; V=U; #initialize vector for system variables (U, I, V)
U[1]=1e4; Ic[1]=1; V[1]=0;
n=1;
#do first run, without noise
for (t in tvec[-1]) #since we already saved the initial condition at t=0, we want to run from the next time point until the end, therefore we kick out the 1st entry (t=0) in tvec 
{
    U[n+1]=U[n]-(b*U[n]*V[n])*tau
    Ic[n+1]=Ic[n]+(b*U[n]*V[n]-d*Ic[n])*tau
    V[n+1]=V[n]+(p*Ic[n]-clear*V[n])*tau  
    n=n+1; #use as counter to be able to address the entries in the variable vectors
}

#plot results from 1st run, without noise
plot(tvec,U,log="y",xlab="days p.i.",col="blue",lty=2,lwd=2,type="l",xlim=c(0,tmax),ylim=c(1e-2,1e5))
lines(tvec,Ic,col="red",lty=2,lwd=2)
lines(tvec,V,col="green",lty=2,lwd=2)

#do second run, with noise
U=rep(0,length(tvec)); Ic=U; V=U; #initialize vector for system variables (U, I, V)
U[1]=1e4; Ic[1]=1; V[1]=0;
n=1;
set.seed(100) #this sets the seed for the Random Number generator. Run the model twice with the same seed, look at the results. Then change the seed. Then comment this line, which makes R produce a "random" seed and run the model again a few times.
for (t in tvec[-1])
{
    #--> add noise term(s) somewhere to these equations and observe how it affects the results
    #--> to produce noise/random numbers, use functions such as rnorm, runif, rpois
    U[n+1]=U[n]-(b*U[n]*V[n])*tau
    Ic[n+1]=Ic[n]+(b*U[n]*V[n]-d*Ic[n])*tau
    #V[n+1]=V[n]+(p*Ic[n]-clear*V[n])*tau  
    
    #some noise examples for the V equation
    #V[n+1]=V[n]+(p*Ic[n]-clear*V[n]+10*runif(1,0,V[n]))*tau  
    V[n+1]=V[n]+(rnorm(1,mean=p,sd=p/5)*Ic[n]-clear*V[n])*tau  
    #V[n+1]=V[n]+(p*Ic[n]-clear*V[n]+runif(1))*tau  
    
    n=n+1; 
}

#plot results from 2nd run, with noise, on top of the 1st run.
lines(tvec,U,col="blue",lty=1,lwd=2)
lines(tvec,Ic,col="red",lty=1,lwd=2)
lines(tvec,V,col="green",lty=1,lwd=2)
                              
###################################################################
#end  program
###################################################################                         
