context("test-construct_modelsettings.R")

test_that("construct_modelsettings works for all apps",
          {
            packagename = 'DSAIRM'

            #find path to apps
            appdir = system.file("appinformation", package = packagename) #find path to apps
            #load app table that has all the app information
            at = read.table(
              file = paste0(appdir, "/apptable.tsv"),
              sep = '\t',
              header = TRUE
            )


            #iterate over at
            ##need to construct appsettings
            ##then pass to generate_shinyinput to pull input values back out









          })







