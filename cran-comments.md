## Test environments
* local Windows 10.1 x86_64, R 3.5.1
* Ubuntu 14.04 (on travis-ci), R 3.4, release
* MacOS 10.12 (on travis-ci), R 3.4, release
* Windows (on appveyor), R 3.5.1


## R CMD check results

There were no errors and warnings.

There were 2 notes:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Andreas Handel <ahandel@uga.edu>'

* checking R code for possible problems ... NOTE
generate_plots: no visible binding for global variable 'xvals'

And several others of that sort. Seems to come from various dplyr commands. I see no way around this, and it doesn't seem to affect anything.


## Submission History

version 0.4.0: This is the first submission of this package.
