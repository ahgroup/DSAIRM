context("test-generate_documentation.R")


test_that("generate_documentation correctly produces a results needed for the doc tabs",
{

  #find path to apps
  packagename = 'DSAIRM'
  appdir = system.file("appinformation", package = packagename) #find path to apps
  appName = "virusandtx"
  at = read.table(file = paste0(appdir,"/apptable.tsv"), sep = '\t', header = TRUE)
  appsettings <<- as.list(at[which(at$appid == appName),])
  #file name for documentation
  currentdocfilename <<- paste0(appdir,"/",appsettings$docname)
  docs = generate_documentation(currentdocfilename)
  #these elements of the tag list needs to contain the indicated words
  expect_true(grepl("Overview",docs[[1]][[2]]$title))
  expect_true(grepl("drug",docs[[1]][[3]]))
})
