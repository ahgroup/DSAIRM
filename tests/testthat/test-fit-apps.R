context("test-fit-apps.R")


test_that("fit apps all run correctly",
{

            #test basic fit
            modelsettings =  list(U = 10000, I = 0, V = 10, X =1, n = 0, dU = 0, dI = 1, g = 1, p = 10, plow = 0.001, phigh = 1000, psim = 10, b = 1e-04, blow = 1e-6, bhigh = 1e-3, bsim = 1e-4, dV = 2, dVlow = 0.001, dVhigh = 1e3, dVsim = 10)
            modelsettings$usesimdata = FALSE
            modelsettings$noise = 0.01
            modelsettings$iter = 10
            modelsettings$solvertype = 1
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$plotscale = 'none'
            modelsettings$simfunction = 'simulate_basicmodel_fit'
            result = run_model(modelsettings)
            finaldatapoint = tail(result[[1]]$dat$yvals,1)
            testthat::expect_equal(finaldatapoint, 1)

})
