## Dynamical Systems Approach to Immmune Response Modeling (DSAIRM) 

#Next steps
* Write up all solutions into the DSAIRMsolutions repository
* Do tasks for Pk/Pd app
* Improve error messages when simulation failed (see basic bacteria app) - use color if possible
* Get everything ready for CRAN
* Clean up diagrams as needed

* Change structure of simulate_ functions such that each one returns a list. At least one list element, called ts or dat, should contain data. Other things can be returned.
Time-series dataframe should be returned with columns "Time" followed by variable names. Other simulations should return a data frame with columns xvals/yvals/varnames.   

* Change documentation creation: Have a single Rmd file and its html equivalent (could we skip that) in the app folder. 
Merge functionality of generate_HTMLfiles and generate_instrction_tabs such that on loading/opening the app, the Rmd file is processed displayed. Ideally without saving the html files that are currently in the www/ subfolder. Instead, just create properly formated tabs on the fly (check if that's fast enough). Update vignettes and docfordevelopers accordingly. Done in DSAIDE for 1st app, almost working.

* Add learning objectives to each overview tab

* Check all refs: Add Satelli04 to bib file

* Continue streamlining code base to make it more general/modular/flexible

* Write up all solutions 

* Make sure to follow ROpenSci guidelines, submit for review: https://github.com/ropensci/onboarding


* Consider making a website for the package with  http://pkgdown.r-lib.org

* Run package through 'goodpractice': https://github.com/MangoTheCat/goodpractice/ - tried, not sure this package works

#Further apps that could be implemented
* More complicated HCV PK/PD (SISMID-U4-3/4)
* U/S analysis using regression instead of correlation (U6-us3)
* models with memory/dummy compartments (U9-memory)
* discrete-time model with noise (U9-noise)
* fitting of multiple datasets and types of data, e.g. Pawelek 2016 model
* fitting of multiple-condition datasets, e.g. with and without drug present (find old R code for this) 
* fitting using different likelihood approaches (instead of just SSR)
* co-infection model
