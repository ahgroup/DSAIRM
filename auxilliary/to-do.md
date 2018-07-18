# Dynamical Systems Approach to Immmune Response Modeling (DSAIRM) 

## Next steps
S1: * Change documentation creation: Have a single Rmd file and its html version in each app folder. Merge functionality of generate_HTMLfiles and generate_instrction_tabs such that on loading/opening the app, the HTML file is processed and displayed. Almost done using the new generate_documentation() function. Some text doesn't display correctly yet, this needs to be fixed. Also nice to have (but could wait with that): XML package is orphaned, could we use an actively maintained one instead (e.g. xml2)? And is there a way to only store the Rmd file and create the HTML file 'on the fly' without saving it to disk?

S2: * Done, needs further checking: Change structure of all simulate_ functions such that each one returns a list. At least one list element, called ts (for a simulation that produces a time-series) or dat (otherwise), should contain data. Other things can be returned.
Time-series dataframe should be returned with columns "Time" followed by variable names. Other simulations should return a list with elements as appropriate. Note: internal only functions (e.g. the internal ODE functions in fitmodelcomparison) don't need to return lists, only user-facing functions/returns should all be lists.   

S3: * Update doc and comments for each function, and update vignettes and docfordevelopers. 

S4: * Re-check everything.


A:
* Get everything ready for CRAN
* Do tasks for Pk/Pd app


## For later
* Continue streamlining code base to make it more general/modular/flexible
* Start implementing unit tests using the testthat package
* Improve error messages when simulation failed: Add a failure flag to each underlying simulator, have calling function check the failure status and process accordingly

* Update all solutions 
* Add learning objectives to each overview tab
* Clean up diagrams as needed
* Maybe submit for Ropensci review: https://github.com/ropensci/onboarding


## Further apps that could be implemented
* More complicated HCV PK/PD (SISMID-U4-3/4)
* U/S analysis using regression instead of correlation (U6-us3)
* models with memory/dummy compartments (U9-memory)
* discrete-time model with noise (U9-noise)
* fitting of multiple datasets and types of data, e.g. Pawelek 2016 model
* fitting of multiple-condition datasets, e.g. with and without drug present (find old R code for this) 
* fitting using different likelihood approaches (instead of just SSR)
* co-infection model
