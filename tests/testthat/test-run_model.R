context("test-run_model.R")

test_that("run_model correctly runs different models",
          {
            ########################################
            # see if apptable can be loaded and used
            packagename = 'DSAIRM'

            #find path to apps
            appdir = system.file("appinformation", package = packagename) #find path to apps
            #load app table that has all the app information
            at = read.table(
              file = paste0(appdir, "/apptable.tsv"),
              sep = '\t',
              header = TRUE
            )
            #test basic bacteria model
            #supply all input settings as list
            appName = "basicbacteriaexploration"
            appsettings <- as.list(at[which(at$appid == appName), ])
            modelsettings =  list(
              B = 100,
              I = 10,
              g = 2,
              Bmax = 1e5,
              dB = 1,
              k = 1e-4,
              r = 1e-4,
              dI = 2,
              tstart = 0,
              tfinal = 300,
              dt = 0.1,
              samples = 10,
              parmin = 2,
              parmax = 10,
              samplepar = 'g',
              pardist = 'lin'
            )
            modelsettings = c(appsettings, modelsettings)
            result = run_model(modelsettings)
            Bpeak = round(result[[1]]$dat$Bpeak[1])

            testthat::expect_equal(Bpeak, 49505)



            ########################################
            #run basic virus model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicvirus_ode'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tfinal  = 120
            modelsettings$modeltype = "_ode_"

            result = run_model(modelsettings)
            #check that simulation ran until max time
            testthat::expect_equal(max(result[[1]]$dat$xvals), modelsettings$tfinal)

            ########################################
            #run basic bacteria model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicbacteria_discrete'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tfinal = 50
            modelsettings$modeltype = "_discrete_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ifinal = round(tail(result[[1]]$dat$yvals, 1))
            testthat::expect_equal(Ifinal, 13594)

            #no model type provided, should fail
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicbacteria_discrete'
            result = run_model(modelsettings)
            testthat::expect_equal(result, "List element modeltype must be provided.")


            ########################################
            #run basic virus stochastic model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicvirus_stochastic'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$V = 10
            modelsettings$rngseed = 115
            modelsettings$tfinal = 100
            modelsettings$modeltype = "_stochastic_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1
            modelsettings$nreps = 1
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ufinal = tail(dplyr::filter(result[[1]]$dat, varnames == "U")$yvals,1)
            testthat::expect_equal(Ufinal, 1858)

            #5 reps
            modelsettings$nreps = 5
            modelsettings$simfunction = 'simulate_basicvirus_stochastic'
            result = run_model(modelsettings)
            testthat::expect_equal(nrow(result[[1]]$dat), 19200)


            ########################################
            #run virus and IR model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_virusandir_ode'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tstart = 10
            modelsettings$tfinal = 100
            modelsettings$B = 1
            modelsettings$dt = 0.1
            modelsettings$dU = 0
            modelsettings$b = 1e-04
            modelsettings$p = 10
            modelsettings$gB = 0.1
            modelsettings$modeltype = "_ode_"
            modelsettings$plotscale = 'y'
            result = run_model(modelsettings)
            Bmax = result[[1]]$dat %>% dplyr::filter(varnames == "B")
            Bmax = round(max(Bmax$yvals))
            testthat::expect_equal(Bmax, 23)





          })
