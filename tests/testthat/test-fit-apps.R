context("test-fit-apps.R")


test_that("fit apps all run correctly",
{

            #test basic fit app
            modelsettings =  list(U = 10000, I = 0, V = 10, X =1, n = 0, dU = 0, dI = 1, g = 1, p = 10, plow = 0.001, phigh = 1000, psim = 10, b = 1e-04, blow = 1e-6, bhigh = 1e-3, bsim = 1e-4, dV = 2, dVlow = 0.001, dVhigh = 1e3, dVsim = 10)
            modelsettings$iter = 10
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$usesimdata = 0
            modelsettings$plotscale = 'y'
            modelsettings$solvertype = 1
            modelsettings$simfunction = 'simulate_basicvirus_fit'

            result = run_model(modelsettings)
            finaldatapoint = tail(result[[1]]$dat$yvals,1)
            testthat::expect_equal(finaldatapoint, 1)

            modelsettings$usesimdata = 1
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot" )

            #test model comparison fit app
            modelsettings =  list(fitmodel = 1, iter = 10)
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$plotscale = 'both'
            modelsettings$simfunction = 'simulate_modelcomparison_fit'
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot" )
            testthat::expect_is(generate_plotly(result), "plotly" )

            #test model comparison fit app, 1nd model
            modelsettings =  list(fitmodel = 2, iter = 10)
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_modelcomparison_fit'
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot" )
            testthat::expect_is(generate_plotly(result), "plotly" )


            #test CI fit app - this one should not work
            modelsettings =  list()
            result = run_model(modelsettings)
            testthat::expect_equal(result, "List element simfunction must be provided.")

            #test CI fit app - this one should work
            modelsettings =  list()
            modelsettings$modeltype = "_fit_"
            modelsettings$simfunction = 'simulate_confint_fit'
            modelsettings$iter = 10
            modelsettings$nsample = 40
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot" )
            testthat::expect_is(generate_text(result), "html" )

            #test flu drug fit app
            modelsettings =  list()
            modelsettings$modeltype = "_fit_"
            modelsettings$simfunction = 'simulate_fludrug_fit'
            modelsettings$iter = 10
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot" )
            testthat::expect_is(generate_text(result), "html" )


})
