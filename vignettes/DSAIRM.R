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

