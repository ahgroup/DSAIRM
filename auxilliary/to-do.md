## Dynamical Systems Approach to Immmune Response Modeling (DSAIRM) 

#Next steps
* Write up all solutions into the DSAIRMsolutions repository
* Do tasks for Pk/Pd app
* Improve error messages when simulation failed (see basic bacteria app) - use color if possible
* Get everything ready for CRAN
* Clean up diagrams as needed
* Change structure of simulate_ functions such that each one returns a list. (Time-series) dataframe should be returned as xvals (time) / yvals (value of variable) / varnames (variable names)   
* Change documentation creation: Have a single Rmd file in the app folder. Merge functionality of generate_HTMLfiles and generate_instrction_tabs such that on loading/opening the app, the Rmd file is processed displayed. Ideally without saving the html files that are currently in the www/ subfolder. Instead, just create properly formated tabs on the fly (check if that's fast enough). 

#Further apps that could be implemented
* More complicated HCV PK/PD (SISMID-U4-3/4)
* U/S analysis using regression instead of correlation (U6-us3)
* models with memory/dummy compartments (U9-memory)
* discrete-time model with noise (U9-noise)
* fitting of multiple datasets and types of data, e.g. Pawelek 2016 model
* fitting of multiple-condition datasets, e.g. with and without drug present (find old R code for this) 
* fitting using different likelihood approaches (instead of just SSR)
* co-infection model
