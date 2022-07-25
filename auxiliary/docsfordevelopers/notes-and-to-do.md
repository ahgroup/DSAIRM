# To-do list for DSAIRM package


## Specific Code Improvements
* Adjust color palette for plotly to be same as ggplot (and make color blind friendly). Seems to work in generate_ggplot() but not yet in generate_plotly()
* Make sure floating taskbar works ok with inputs and is always above all other content (currently some of the inputs and the top bar are above float)
* Provide a 'download scenario' button, which downloads code that reproduces a given scenario. To that end, finish implementing download_code in develop branch, once it works move over to main. Download_code related:

* rewrite run_model.R such that it calls construct_simulation_code instead of repeating code. 
* run_model.R should also be calling a separate function ( generate_output() ) that processes results returned from the app calls so it can be used with generate_() functions. 
* move all code that is related to writing the R script from app.R into a generate_downloadcode function. 
* improve formatting of output code by adding line breaks, e.g. every modelsetting entry in its own line. 
* Don't use colons in variable names (no my.result). Instead just call it 'result' or such.
* The code that is produced by generate_downloadcode() should ONLY include the call to simulate_ and the generate_plot/text DSAIRM functions. No other DSAIRM function (including these new ones) should be used inside the code script given to users.





## General Code Improvements
* Continue implementing unit tests using the testthat package
* Add checks to parameter settings, don't allow unreasonable (e.g. negative) ones
* Continue streamlining code base to make it more general/modular/flexible
* Fix/finish plot function edits (see code in generate_ggplot)


## Content Improvement
* Re-create all diagrams with flowdiagramr
* Write tasks for extended bacteria and bacteria model fitting apps
* Check learning objectives to each overview tab
* Implement further apps, see below
* Once flowdiagramr is ready, use it to re-do model diagrams

## Apps to be implemented
* More complicated HCV PK/PD (SISMID-U4-3/4)
* U/S analysis using regression instead of correlation (U6-us3)
* models with memory/dummy compartments (U9-memory)
* discrete-time model with noise (U9-noise)
* fitting of multiple datasets and types of data, e.g. Pawelek 2016 model
* fitting of multiple-condition datasets, e.g. with and without drug present (find old R code for this) 
* fitting using different likelihood approaches (instead of just SSR)
* co-infection model

## Documentation / Outreach / Advertisement
* Make list of related courses, email instructors
* Create more docs
* Find classes/instructors who could use package.
* Make videos for levels 1/2/3 for packages 

## General thoughts and comments
* Maybe change name of package
* look into R consortium package certification
* Get best practices badge: https://bestpractices.coreinfrastructure.org/en
* Maybe submit for Ropensci review: https://github.com/ropensci/onboarding
* Hashtags on twitter when promoting DSAIRM app: #rstats, #dynamicalsystems #immunology #infectiousdisease






