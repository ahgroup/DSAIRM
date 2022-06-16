# To-do Andreas

* Create diagrams for new apps with flowdiagamr: acute virus and IR, chronic virus and IR, extended bacteria, bacteria fitting
* make tasks for extended bacteria and bacteria model fitting


# To-do Cody

First:
* Do a quick review of the basic virus app and the model variant exploration app, just to make sure things work.
  + basic virus and model variant exploration seem okay

* Do another round of review of the new ones: acute virus and IR, chronic virus and IR, extended bacteria, bacteria fitting
  + acute virus and IR, chronic virus and IR seem okay
  + extended bacteria 
    - model diagram missing A compartment entirely, maybe overwritten when made the fitting diagram?
    - also initial description says still 2 compartment model, fixed
    - app seems to take just a tad bit longer to open from main menu, happened a few times consistently, now can't reproduce, maybe just my computer
  + bacteria fitting
    - warning of missing data printed in console
      - Warning: Removed 2 row(s) containing missing values (geom_path).
      - Warning: Removed 10 rows containing missing values (geom_point).


Later:
* If time and interested: add tasks to extended bacteria and bacteria model fitting
  + extended bacteria topic / flow ideas
    - steady state (basic bacteria introduces briefly)
    - IR turn on each separately (mirror other IR apps)
    
  + bacteria fitting
    - quick concept checkmark of what model components are and are not being estimated (initial values for variables/compartments)
    - how to "turn off" estimation for parameter (e.g., set initial&range to zero, set range to match initial)
    



download_code related (probably not for now):
* rewrite run_model.R such that it calls construct_simulation_code instead of repeating code. as suitable, save a version of run_model in the auxiliary/oldfiles folder in case we need to go back.




## Completed
* Created style guide section in docsfordevelopers/documentation.md
    + put consistency notes from below, likely could use rephrasing/examples, but low priority
* Review the new acute virus and chronic virus apps. Review/check both DSAIRM and DSAIRMsolutions for those 2 apps. Flag/fix as needed.


    



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
