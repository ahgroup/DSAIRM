context("test-run_model.R")


test_that("run_model correctly runs different models",
{
            tfinal = 120
            modelsettings =  list(U = 1e5,I=0,V=10, n = 0, dU = 0,dI = 1, dV = 2, b = 2e-05, p = 5, g = 1, tstart = 0, tfinal = tfinal, dt = 0.1, modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_basicvirus_ode'
            result = run_model(modelsettings)
            #check that simulation ran until max time
            testthat::expect_equal(max(result[[1]]$dat$xvals), tfinal)

            modelsettings =  list(B = 10, I = 1, g = 1, Bmax = 1e6, dB = 0.1, k = 1e-07, r = 1e-3, dI = 1, tstart = 0, tfinal = 50, dt = 0.01, modeltype = "_discrete_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_basicbacteria_discrete'
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ifinal = round(tail(result[[1]]$dat$yvals,1))
            testthat::expect_equal(Ifinal, 13594)


            modelsettings =  list(U = 10000, I = 0, V = 10, n = 0, dU = 0, b = 1e-04, dI = 1, p = 10, dV = 2, rngseed = 115, tfinal = 100, modeltype = "_stochastic_", plotscale = 'y', nplots = 1,  nreps = 1)
            modelsettings$simfunction = 'simulate_basicvirus_stochastic'
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ufinal = min(dplyr::filter(result[[1]]$dat, varnames == "U")$yvals)
            testthat::expect_equal(Ufinal, 58)

            #make model fail by setting start after end
            modelsettings =  list(tstart = 10, tfinal = 0, dt = 0.1, modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_basicvirus_ode'
            result = run_model(modelsettings)
            testthat::expect_equal(result, "Model run failed. Maybe unreasonable parameter values?")

            #make model fail by setting something to negative
            modelsettings =  list(U = 10000, I = 0, V = -10, n = 0, modeltype = "_stochastic_",  nplots = 1,  nreps = 1)
            modelsettings$simfunction = 'simulate_basicvirus_stochastic'
            result = run_model(modelsettings)
            testthat::expect_equal(result, "Model run failed. Maybe unreasonable parameter values?")

            #no model type provided, should fail
            modelsettings =  list(B = 10, I = 1, g = 1, Bmax = 1e6, dB = 0.1, k = 1e-07, r = 1e-3, dI = 1, tstart = 0, tfinal = 50, dt = 0.01, plotscale = 'x', nplots = 1)
            modelsettings$simfunction = 'simulate_basicbacteria_discrete'
            result = run_model(modelsettings)
            testthat::expect_equal(result, "List element modeltype must be provided.")

            #another model try
            packagename = 'DSAIRM'

            #find path to apps
            appdir = system.file("appinformation", package = packagename) #find path to apps
            #load app table that has all the app information
            at = read.table(file = paste0(appdir,"/apptable.tsv"), sep = '\t', header = TRUE)
            appName = "bacteriaexploration"
            appsettings <- as.list(at[which(at$appid == appName),])
            modelsettings =  list(B = 100, I = 10, g=2, Bmax=1e5, dB=1, k=1e-4, r=1e-4, dI=2, tstart = 0, tfinal = 300, dt = 0.1, samples = 10, parmin=2, parmax=10, samplepar='g',  pardist = 'lin')
            modelsettings = c(appsettings,modelsettings)
            result = run_model(modelsettings)
            Bpeak = round(result[[1]]$dat$Bpeak[1])

            testthat::expect_equal(Bpeak, 49505)



})
