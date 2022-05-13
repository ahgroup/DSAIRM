# To-do Andreas

* Implement new acute virus with simple IR app - level 1, show GUI approach. similar to virus and IR model, but simpler (no chronic, only 1 or 2 IR equations).
* Implement chronic virus with simple IR app - level 2, show systematic model exploration. similar to virus model exploration but with IR.
* Implement basic bacteria fitting app - use as level 3, show fitting to data. data?




# To-do Cody

* Review/critique download scenario button
- do we need shinyjs? I prefer to keep package dependencies at a minimum, so unless we really need it, I'd rather not use it. i took it out for now. willing to include if you can make a convincing case that we need it :)
- i tried to use checkboxInput() for the download with download triggered at end of "run simulation" routine if box is checked. But I'm not sure how to trigger the download/downloadhandler routine without a downloadButton(). so sticking with that for now. can potentially revisit. 
- things don't quite work for the more complex models. the code that's produced is not quite right. for those models, the output returned from the simulate_ function generally needs processing before sent to the generate_ functions. Take a look at the various DSAIRM solution files, those show how the code should approximately look like for the various functions. it's not straightfoward for the more complex models. Since the whole 'download code' routine will likely be somewhat long, I suggest placing it into a separate function, not inside app.R. The stuff in the exsiting dowload_code.R function (currently in auxiliary/oldfiles) might or might not be useful.
- also, not sure I stated, but I don't want the run_model() function in the downloaded code. that's one level too abstract. i want it to look like the solutions, meaning only calls to the simulate_ and generate_ functions.



## Pending
* Videos or other teaching materials?
    + Ideas
        - pipeline to contribute, modelbuilder to DSAIRM/DSAIDE
* Review new apps once ready
* Check that tasks/solutions are still correct, especially fitting/nloptr related

## Currently
* Reacquaint with app framework
* Work through all comments at bottom

## Completed
* Created style guide section in docsfordevelopers/documentation.md
    + put consistency notes from below, likely could use rephrasing/examples, but low priority
* Implement "download scenario" button
    + generate_shinyinput.R edits
        - to show download button in app window
        - to place at bottom right of panel
    + app.R edits
        - to store modelsettings in global environment
        - to write content for downloadHandler()
        - to include shinyjs for disabling button before first run (wouldn't be needed for directly grabbing shiny gui inputs)
    + current version tries to replicate each fctcall in run_model(), but integrating multiple results without actually calling run_model() doesn't seem worth it
        - can easily enough pass to rbind(), but subsequent passes to generate_PLOT() fail 
        - list() of each result does not integrate/overlay, no better than plotting each separate
        - post processing of results in run_model() missing, is it needed?
        - would extracting shiny gui inputs and parsing face same issues?
        - generating the reproducible code during run_model() would be just as tedious?; collections of each fctcall would simplify backend; perhaps making run_model() more generic less modular would help; maybe compartmentalize model setup, simulation runs, results post-processing



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
