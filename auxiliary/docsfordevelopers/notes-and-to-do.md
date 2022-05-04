# To-do Andreas

* Implement new acute virus with simple IR app - level 1, show GUI approach. similar to virus and IR model, but simpler (no chronic, only 1 or 2 IR equations).
* Implement chronic virus with simple IR app - level 2, show systematic model exploration. similar to virus model exploration but with IR.
* Implement basic bacteria fitting app - use as level 3, show fitting to data. data?



# To-do Cody

* work through all comments at bottom
* implement "download scenario" button?
* videos or other teaching materials?
* review new apps once ready



# To-do list for DSAIRM package

## Code Improvements
* Continue implementing unit tests using the testthat package
* Provide a 'download scenario' button, which downloads code that reproduces a given scenario.
* Add checks to parameter settings, don't allow unreasonable (e.g. negative) ones
* Continue streamlining code base to make it more general/modular/flexible
* Fix/finish plot function edits (see code in generate_ggplot)
* Make sure Latex equations work well when floating task list

## Content Improvement
* Check tasks/solutions, especially fitting/nloptr related
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

## Consistency
* Sectioning
  + Capitalization rules to follow title rules ("To"" when an infinitive also)
  + Documentation (bullets for lists only and notes section)
    - Overview
      > Learning Objectives (stated imperatively)
    - The Model 
      > Model Overview (number processes)
      > Model Diagram
      > Model Equations ({+} varying subsections, use aligned equations)
      > Model Concepts ({-})
      > Notes
    - What To Do
      > Unit description ({-} not section; bolded)
      > Tasks
    - Further Information
      > References
      
* In-text references
  + Apps referenced in bold italics
  + App tabs referenced in italics
  + When outside equations, parameters in italics, using appropriate subscript / superscript, or plain text when written out
  + When outside equations, variables in bold when single letter, or plain text when written out (e.g., **I** versus immune response)
    - including 
      > initial values (subscript naught), 
      > max values (subscript peak?, superscript \*) just write out, 
      > min values (subscript \*?) just write out, 
      > final values (subscript final or time number?) just write out, 
      > steady state (subscript s)
  + Subscripts / superscripts should be lowercase except when referencing a variable (e.g., _d~I~_)
  
* Concepts or keywords or vocabulary in bold italics
  + first time introduced? maybe a couple after if within same doc
  

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
