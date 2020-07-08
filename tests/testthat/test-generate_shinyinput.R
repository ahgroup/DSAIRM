context("test-generate_shinyinput.R")


test_that("generate_shinyinput correctly produces a shiny input structure",
{

            packagename = 'DSAIRM'

            appdir = system.file("appinformation", package = packagename) #find path to apps
            modeldir = system.file("mbmodels", package = packagename) #find path to apps
            simdir = system.file("simulatorfunctions", package = packagename) #find path to apps

            #load app table that has all the app information
            at = read.table(file = paste0(appdir,"/apptable.tsv"), sep = '\t', header = TRUE)

            appName = "basicbacteria"

            appsettings <- as.list(at[which(at$appid == appName),])

            #a few apps have 2 simulator functions, combine here into vector
            if (nchar(appsettings$simfunction2) > 1)
            {
              appsettings$simfunction <- c(appsettings$simfunction,appsettings$simfunction2)
            }

            #all columns are read in as characters, convert some
            appsettings$use_mbmodel = as.logical(appsettings$use_mbmodel)
            appsettings$use_doc = as.logical(appsettings$use_doc)
            appsettings$nplots = as.numeric(appsettings$nplots)

            #if an mbmodel should be used, check that it exists and load
            appsettings$mbmodel <- NULL
            #appsettings$mbmodel = readRDS(paste0(modeldir,"/",appsettings$mbmodelname) )

            #if the doc of a file should be parsed for UI generation, get it here
            filepath = paste0(simdir,'/',appsettings$simfunction[1],'.R')
            appsettings$filepath = filepath


            modelinputs2 <- generate_shinyinput(use_mbmodel = FALSE, mbmodel = appsettings$mbmodel,
                                                use_doc = TRUE, model_file = appsettings$filepath,
                                                model_function = appsettings$simfunction[1],
                                                otherinputs = appsettings$otherinputs, packagename = packagename)


            #this element of the tag list needs to contain the word susceptible
            expect_true(grepl('bacteria',modelinputs2[[2]][[1]][[3]]))


            modelinputs3 <- generate_shinyinput(use_mbmodel = FALSE, mbmodel = appsettings$mbmodel,
                                                use_doc = FALSE, model_file = appsettings$filepath,
                                                model_function = appsettings$simfunction[1],
                                                otherinputs = appsettings$otherinputs, packagename = packagename)

            #this element of the tag list only contains the label S, not the word susceptible
            expect_false(grepl('Bacteria',modelinputs3[[2]][[1]][[3]]))
            expect_true(grepl('B',modelinputs3[[2]][[1]][[3]]))

            #can be done if we turn on mbmodel
            #expect_equal(length(modelinputs1),length(modelinputs2))
            #expect_equal(length(modelinputs1),length(modelinputs3))
})



