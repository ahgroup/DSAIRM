## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  library('DSAIRM')

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dsairmmenu()

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dsairmapps()

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dsairmapps('BasicBacteria')

## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
library('DSAIRM')

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  help('simulate_basicbacteria')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
result <- simulate_basicbacteria(B0 = 500, I0 = 5, tmax = 100, g = 0.5,  r = 0.002)

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
plot(result$ts[,"Time"],result$ts[,"Bc"],xlab='Time',ylab='Bacteria Numbers',type='l')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
rvec = 10^seq(-5,-2,length=20) #values of log immune activation rate, r, for which to run the simulation 
peak = rep(0,length(rvec)) #this will record the peak values for each r
for (n in 1:length(rvec))
{
  #call the simulator function with different values of g each time
  result <- simulate_basicbacteria(B0 = 10, I0 = 1, tmax = 200, r = rvec[n])
  peak[n] <- max(result$ts[,"Bc"]) #record max number of bacteria for each value of r
}
#plot final result
plot(rvec,peak,type='p',xlab='Immune activation rate',ylab='Max number of bacteria',log='xy')

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  system.file("simulatorfunctions", package = "DSAIRM")

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  simulate_basicbacteria <- function(B0 = 10, I0 = 1, tmax = 30, g=1, Bmax=1e6, dB=1e-1, k=1e-7, r=1e-3, dI=1)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  mysimulator <- function(B0 = 10, I0 = 1, tmax = 30, g=1, Bmax=1e6, dB=1e-1, k=1e-7, r=1e3, dI=1, s=1E3)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  pars = c(g=g,Bmax=Bmax,dB=dB,k=k,r=r,dI=dI)

## ----eval=FALSE, echo=TRUE, color='red'----------------------------------
#  pars = c(g=g,Bmax=Bmax,dB=dB,k=k,r=r,dI=dI,s=s)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  dBdt = g*B*(1-B/Bmax) - dB*B - k*B*I
#  dIdt = r*B*I - dI*I

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  dBdt = g*B*(1-B/Bmax) - dB*B - k*B*I
#  dIdt = r*B*I/(s+B) - dI*I

## ----eval=TRUE, echo=TRUE------------------------------------------------
source('mysimulator.R') #to initialize the new function - it needs to be in same directory as these lines of code
svec = 10^seq(-3,3,length=20) #values of saturation parameter 
Imax = rep(0,length(svec)) #this will record the final immune response level
for (n in 1:length(svec))
{
  result <- mysimulator(s = svec[n])
  Imax[n] <- max(result$ts[,"Ic"])
}
plot(svec,Imax,type='p',xlab='Saturation parameter',ylab='Max immune response level',log='xy')

