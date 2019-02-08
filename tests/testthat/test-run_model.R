context("test-run_model.R")


test_that("run_model correctly runs different models",
{
            tfinal = 120
            modelsettings =  list(U = 1e5,I=0,V=10, n = 0, dU = 0,dI = 1, dV = 2, b = 2e-05, p = 5, g = 1, tstart = 0,
                                  tfinal = tfinal, dt = 0.1, modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelfunction = 'simulate_basicvirus_ode'
            result = run_model(modelsettings, modelfunction)
            #check that simulation ran until max time
            expect_equal(max(result[[1]]$dat$xvals), tfinal)

            modelsettings =  list(U = 10000, I = 0, V = 10, n = 0,
                                  dU = 0, b = 1e-04, dI = 1, p = 10, dV = 2, rngseed = 115,
                                  tfinal = 100, modeltype = "_stochastic_", plotscale = 'y', nplots = 1,  nreps = 1)
            modelfunction = 'simulate_basicvirus_stochastic'
            result = run_model(modelsettings, modelfunction)
            #check that simulation returned specific value of susceptible at end
            Ufinal = min(dplyr::filter(result[[1]]$dat, varnames == "U")$yvals)
            expect_equal(Ufinal, 58)
})
