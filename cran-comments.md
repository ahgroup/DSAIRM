## Test environments
* local Windows 10.1 x86_64, R 3.4.2
* ubuntu 12.04 (on travis-ci), R 3.4.2

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    shinyapps   6.1Mb

Each shiny app contains information about the model that is studied. This documentation includes a figure for each app, thus the 6Mb size.
I have excluded the folders containing the files with the figures and all the raw documentation (folders \inst\shinyapps\allappdocumentation and \inst\shinyapps\media) from the package with .Rbuildignore but this does not seem to be considered by R CMD check.



## Submission History

version 0.3.0: This is the first submission of this package.
