############################################################
##this code runs the uncertainty and sensitivity analysis
############################################################


B0min = isolate(input$B0min)
B0max = isolate(input$B0max)
I0min = isolate(input$I0min)
I0max = isolate(input$I0max)
bmin = isolate(input$bmin)
bmax = isolate(input$bmax)
Bmaxmin = 10^isolate(input$Bmaxmin);
Bmaxmax = 10^isolate(input$Bmaxmax);
dBmin = isolate(input$dBmin)
dBmax = isolate(input$dBmax)
kmin = 10^isolate(input$kmin)
kmax = 10^isolate(input$kmax)
rmin = 10^isolate(input$rmin)
rmax = 10^isolate(input$rmax)
dImin = isolate(input$dImin)
dImax = isolate(input$dImax)
gmean = isolate(input$gmean)
gvar = isolate(input$gvar)
tmax = isolate(input$tmax);

rngseed = isolate(input$rngseed)
samples = isolate(input$samples)
plottype = isolate(input$plottype)
plotscale = ""




simulate_usanalysis


simulate_usanalysis <- function(B0min = 1, B0max = 10, I0min = 1, I0max = 10, Bmaxmin=1e5, Bmaxmax=1e6, dBmin=1e-1, dBmax = 1e-1, kmin=1e-7, kmax=1e-7, rmin=1e-3, rmax=1e-3, dImin=1, dImax=2, gmean=0.5, gvar=0.1, tmax = 30, samples = 10, rngseed = 100)
  {

    #this creates a LHS with the specified number of samples for all 8 parameters
    #drawn from a uniform distribution between zero and one
    #if a parameter should be kept fixed, simply set min and max to the same value
    set.seed(rngseed)
    lhssample=lhs::randomLHS(samples,8);

    #transforming parameters to be  uniform between their low and high values
    B0vec = stats::qunif(lhssample[,1],min = B0min, max = B0max)
    I0vec = stats::qunif(lhssample[,2],min = I0min, max= I0max)
    Bmaxvec = stats::qunif(lhssample[,3],min = Bmaxmin, max = Bmaxmax)
    dBvec   = stats::qunif(lhssample[,4],min = dBmin, max = dBmax)
    kvec = stats::qunif(lhssample[,5],min = kmin, max = kmax)
    rvec = stats::qunif(lhssample[,6],min = rmin, max = rmax)
    dIvec = stats::qunif(lhssample[,7],min = dImin, max = dImax)

    #transforming parameter g to a gamma distribution with mean gmean and variance gvar
    #this is just to illustrate how different assumptions of parameter distributions can be implemented
    gvec = stats::qgamma(lhssample[,8], shape = gmean^2/gvar, scale = gvar/gmean);

    Bpeak=rep(0,samples) #initialize vectors that will contain the solution
    Bsteady=rep(0,samples)
    Isteady=rep(0,samples)

    nosteady = rep(FALSE,samples) #indicates if steady state has not been reached
    for (n in 1:samples)
    {
        #values for sampled parameters
        B0=B0vec[n]
        I0=I0vec[n]
        Bmax=Bmaxvec[n]
        dB=dBvec[n];
        k=kvec[n];
        r=rvec[n];
        dI=dIvec[n];
        g=gvec[n];

        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeoutput <- simulate_basicbacteria(B0 = B0, I0 = I0, tmax = tmax, g=g, Bmax=Bmax, dB=dB, k=k, r=r, dI=dI)

        timeseries = odeoutput$ts

        Bpeak[n]=max(timeseries[,"Bc"]); #get the peak for B
        Bsteady[n] = utils::tail(timeseries[,"Bc"],1)
        Isteady[n] = utils::tail(timeseries[,"Ic"],1)


        #a quick check to make sure the system is at steady state,
        #i.e. the value for B at the final time is not more than
        #1% different than B several time steps earlier
        vl=nrow(timeseries);
        if ((abs(timeseries[vl,"Bc"]-timeseries[vl-10,"Bc"])/timeseries[vl,"Bc"])>1e-2)
        {
          nosteady[n] = TRUE
        }
    }

    simresults = data.frame(Bpeak = Bpeak, Bsteady = Bsteady, Isteady = Isteady, B0 = B0vec, I0 = I0vec, Bmax = Bmaxvec, dB = dBvec, k = kvec, r = rvec, dI = dIvec, g = gvec, nosteady = nosteady)

    result = list()
    result$dat = simresults
    return(result)
}
