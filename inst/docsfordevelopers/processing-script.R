#run this script after major changes to do some cleaning and processing automatically
library('here')
library('devtools')
library('pkgdown')
library('zip')

###################################################
# build all html documentation files from Rmd files
##################################################

#source several helper functions
#one loads the settings for this app from the apptable file
#the other is used to fill in the information below into the task table
#Note: path inst/NNN will become NNN in installed package
#but these functions are run during package development, thus are located in inst/NNN
files_to_source = list.files(here::here("inst/helperfunctions"),full.names=TRUE)
sapply(files_to_source, source)

#get path to Rmd files containing documentation
basepath = here::here()
files = list.files(path = paste0(basepath, "/inst/appinformation/"), recursive=FALSE, pattern = "\\.Rmd$", full.names = TRUE)

#remove all html documentation files before recreating
html_files = list.files(path = paste0(basepath, "/inst/appinformation/"), recursive=FALSE, pattern = "\\.html$", full.names = TRUE)
file.remove(html_files)


#re-build all html documentation files from the rmd files at once
for (n in 1: length(files)) {rmarkdown::render(files[n]); Sys.sleep(2)}



###################################################
# Copy simulator functions into the /inst/simulator folder and zip
##################################################

#delete files in /simulatorfunctions/ and the zip file
zipfilename = paste0(basepath,"/inst/simulatorfunctions/simulatorfunctions.zip")
simulation_originals = list.files(path = paste0(basepath,"/R/"), recursive=TRUE, pattern = "^simulate", full.names = TRUE)
simulation_copies = list.files(path = paste0(basepath,"/inst/simulatorfunctions/"), recursive=TRUE, pattern = "^simulate", full.names = TRUE)

#remove zip file and copy of simulators
file.remove(zipfilename)
file.remove(simulation_copies)

#copy files
file.copy(simulation_originals, paste0(basepath,"/inst/simulatorfunctions/"), overwrite = TRUE)

# create zip file
zip::zipr(zipfile = zipfilename, files = simulation_originals, recurse = FALSE, include_directories = FALSE)


###################################################
# Other building tasks to do before release
##################################################

#run spell check - only turn on if wanted
#spelling::spell_check_files(files)

###################################################
# recreate documentation for exported functions
##################################################
devtools::document(roclets = c('rd', 'collate', 'namespace'))

# re-build vignette
devtools::build_vignettes()

#update the pkgdown website
pkgdown::build_site()
