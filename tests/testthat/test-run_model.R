context("test-run_model.R")


test_that("run_model correctly runs different models",
{
            tfinal = 120
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, tstart = 0, tfinal = tfinal, dt = 0.1, modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelfunction = 'simulate_sir_ode'
            result = run_model(modelsettings, modelfunction)
            #check that simulation ran until max time
            expect_equal(max(result[[1]]$dat$xvals), tfinal)
            
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, m = 0, n = 0, tmax = 100, rngseed = 123, modeltype = "_stochastic_", plotscale = 'y', nplots = 1,  nreps = 1)
            modelfunction = 'simulate_sir_stochastic'
            result = run_model(modelsettings, modelfunction)
            #check that simulation returned specific value of susceptible at end
            Sfinal = min(dplyr::filter(result[[1]]$dat, varnames == "S")$yvals)
            expect_equal(Sfinal, 153)
})