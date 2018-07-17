# Dynamical Systems Approach to Immmune Response Modeling (DSAIRM) 

## Next steps
* Change documentation creation: Have a single Rmd file and its html version in each app folder. Merge functionality of generate_HTMLfiles and generate_instrction_tabs such that on loading/opening the app, the HTML file is processed and displayed. Almost done using the new generate_documentation() function and implemented for Basic Bacteria app. Some text doesn't display correctly yet, this needs to be fixed. Also nice to have (but could wait with that): XML package is orphaned, could we use an actively maintained one instead)? And is there a way to only store the Rmd file and create the HTML file 'on the fly' without saving it to disk?

* Change structure of all simulate_ functions such that each one returns a list. At least one list element, called ts (for a simulation that produces a time-series) or dat (otherwise), should contain data. Other things can be returned.
Time-series dataframe should be returned with columns "Time" followed by variable names. Other simulations should return a data frame with columns xvals/yvals/varnames.   

* Check documentation (and text comments) for each app (further reading section), each function, and update vignettes and docfordevelopers accordingly. 

* Get everything ready for CRAN

* Do tasks for Pk/Pd app



## For later
* Add learning objectives to each overview tab
* Update all solutions 
* Continue streamlining code base to make it more general/modular/flexible
* Improve error messages when simulation failed (see basic bacteria app) - use color if possible
* Clean up diagrams as needed
* Mayb submit for Ropensci review: https://github.com/ropensci/onboarding


## Further apps that could be implemented
* More complicated HCV PK/PD (SISMID-U4-3/4)
* U/S analysis using regression instead of correlation (U6-us3)
* models with memory/dummy compartments (U9-memory)
* discrete-time model with noise (U9-noise)
* fitting of multiple datasets and types of data, e.g. Pawelek 2016 model
* fitting of multiple-condition datasets, e.g. with and without drug present (find old R code for this) 
* fitting using different likelihood approaches (instead of just SSR)
* co-infection model
