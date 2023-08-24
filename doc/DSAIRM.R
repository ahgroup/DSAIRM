## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  library('DSAIRM')

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  dsairmmenu()

## ----dsaidemenu,  fig.cap='Main menu of the DSAIRM package.', echo=FALSE------
knitr::include_graphics("mainmenu.png")

## ----appexample,  fig.cap='Screenshot of the input-output elements of one of the apps.', echo=FALSE----
knitr::include_graphics("appexample.png")

## ----modelexample,  fig.cap='Screenshot of the _Model_ section of one of the apps.', echo=FALSE----
knitr::include_graphics("modelexample.png")

## ----whattodoexample,  fig.cap='Screenshot of the _What to do_ section of one of the apps.', echo=FALSE----
knitr::include_graphics("whattodoexample.png")

## ---- eval=TRUE, message=FALSE------------------------------------------------
library('DSAIRM')

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  help('simulate_basicbacteria_ode')

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
result <- simulate_basicbacteria_ode()

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
plot(result$ts[ , "time"],result$ts[ , "B"],xlab='Time',ylab='Bacteria',type='l')

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
result <- simulate_basicbacteria_ode(g = 0.5, r = 0.002)
plot(result$ts[ , "time"],result$ts[ , "B"],xlab='Time',ylab='Bacteria',type='l')

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
#values for immune activation rate, r, for which to run the simulation
rvec = 10^seq(-5,-2,length=20)  
#this variable will hold the peak values for each r
Bpeak = rep(0,length(rvec)) 
for (n in 1:length(rvec))   
{
  #run the simulation model for different values of r 
  #for simplicity, all other inputs are kept at their defaults
  result <- simulate_basicbacteria_ode(r = rvec[n])
  #record max number of bacteria for each value of r
  Bpeak[n] <- max(result$ts[,"B"]) 
}
#plot final result
plot(rvec,Bpeak,type='p',xlab='Immune activation rate',ylab='Peak bacteria',log='xy')

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  simulate_basicbacteria_ode <- function(B = 10, I = 1, g = 1, Bmax = 1e+05, dB = 0.1, k = 1e-06, r = 0.001, dI = 1, tstart = 0, tfinal = 30, dt = 0.05)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  mysimulator <- function(B = 10, I = 1, g = 1, Bmax = 1e+05, dB = 0.1, k = 1e-06, r=1e3, dI=1, tstart = 0, tfinal = 30, dt = 0.05, s=1E3)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  pars = c(g=g,Bmax=Bmax,dB=dB,k=k,r=r,dI=dI)

## ----eval=FALSE, echo=TRUE, color='red'---------------------------------------
#  pars = c(g=g,Bmax=Bmax,dB=dB,k=k,r=r,dI=dI,s=s)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  dBdt = g*B*(1-B/Bmax) - dB*B - k*B*I
#  dIdt = r*B*I - dI*I

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  dBdt = g*B*(1-B/Bmax) - dB*B - k*B*I
#  dIdt = r*B*I/(s+B) - dI*I

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
#initialize the new function
#it needs to be in the same directory as this code
source('mysimulator.R') 
#values of saturation parameter to explore
svec = 10^seq(-3,3,length=20)  
#this will record the maximum immune response level
Ipeak = rep(0,length(svec)) 
for (n in 1:length(svec))
{
  #run the simulation model for different values of s 
  #for simplicity, all other inputs are kept at their defaults
  result <- mysimulator(s = svec[n]) 
  #record max immune response for each value of s
  Ipeak[n] <- max(result$ts[,"I"])
}

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
plot(svec,Ipeak,type='p',xlab='Saturation parameter, s',ylab='Peak immune response',log='xy')

