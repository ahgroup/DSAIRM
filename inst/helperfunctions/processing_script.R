#run this script after major changes to do some cleaning and processing automatically

  library('here')
  library('devtools')
  library('pkgdown')
  library('zip')
  library('dplyr')


  basepath = here::here()

  ###################################################
  # build all html documentation files from Rmd files
  ###################################################


  #source several helper functions
  startupfile = paste0(basepath, "/inst/appinformation/startup_script.R")
  source(startupfile)

  #get path to Rmd files containing documentation
  #Note: path inst/NNN will become NNN in installed package
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

