# To-do Andreas

* Implement basic bacteria fitting app - use as level 3, show fitting to data. data?
* Review/critique download scenario button




# To-do Cody

Most important:
* Do a quick review of the basic virus app, just to make sure things work.

Other:
* Work through all comments at bottom



## Pending
* Videos or other teaching materials?
    + Ideas
        - pipeline to contribute, modelbuilder to DSAIRM/DSAIDE
* Review new apps once ready
* Check that tasks/solutions are still correct, especially fitting/nloptr related


## Completed
* Created style guide section in docsfordevelopers/documentation.md
    + put consistency notes from below, likely could use rephrasing/examples, but low priority
* Review the new acute virus and chronic virus apps. Review/check both DSAIRM and DSAIRMsolutions for those 2 apps. Flag/fix as needed.
* Implement "download scenario" button
    + three new functions to help
        - construct_modelsettings.R takes shiny input and other relevant objects to create modelsettings list used in simulation runs
        - construct_simulation_code.R takes a modelsettings list and returns a list of simulation function calls encoded within the modelsettings; this is the first half of run_model()
        - generate_output.R takes a modelsettings list and a list of results that should be generated from individual function calls encoded within the provided modelsettings, these are used to process the set of results in a way that allows them to be passed on to the generate_ plotting functions
    + clunky coding for now, still various options in implementation
    + currently, construct_modelsettings() is only one integrated directly into app, though possible for all
        - construct_simulation_code() only executed when download button hit
        - generate_output() designed for use in the downloaded script, not as part of app
    + could be used in solutions
    + not thoroughly tested
    



# To-do list for DSAIRM package

## Code Improvements
* Continue implementing unit tests using the testthat package
* Provide a 'download scenario' button, which downloads code that reproduces a given scenario.
* Add checks to parameter settings, don't allow unreasonable (e.g. negative) ones
* Continue streamlining code base to make it more general/modular/flexible
* Fix/finish plot function edits (see code in generate_ggplot)
* Make sure float works ok with inputs and is above

## Content Improvement
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



# Old comments, probably from Cody

## App specific

* Virus and Treatment
  + Does there need to be an explanation of txstart? 
* Fit basic model
  + describe censoring referenced in other apps (e.g., fit model comparison)
* Confidence intervals
  + more content on bootstrapping, such as using percentiles for confidence intervals? a "see here for more" citation?
* Fit flu drug
  + error message in console: Warning in RColorBrewer::brewer.pal(N, "Set2") : n too large, allowed maximum for palette Set2 is 8 Returning the palette you asked for with that many colors
    - Could use virdis::viridis() instead (supposed to be color blind friendly?)
  + generated text under plot has parameters as b/g/p/ f or e (depending on model type), but could be a k?
