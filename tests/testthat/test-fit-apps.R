context("test-fit-apps.R")


test_that("fit apps all run correctly",
          {
            ###############################
            #test basic fit app
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicvirus_fit'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$U = 1e6
            modelsettings$V = 10
            modelsettings$iter = 10
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$usesimdata = 0
            modelsettings$plotscale = 'y'
            modelsettings$solvertype = 1

            result = run_model(modelsettings)
            finaldatapoint = tail(result[[1]]$dat$yvals, 1)
            testthat::expect_equal(finaldatapoint, 1)

            modelsettings$usesimdata = 1
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot")

            ###############################
            #test model comparison fit app
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_modelcomparison_fit'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            #change some defaults
            modelsettings$fitmodel = 1
            modelsettings$iter = 10
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$plotscale = 'both'
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot")
            testthat::expect_is(generate_plotly(result), "plotly")

            ###############################
            #test model comparison fit app, 2nd model
            modelsettings$simfunction = 'simulate_modelcomparison_fit'

            #change some defaults
            modelsettings$fitmodel = 2
            modelsettings$iter = 10
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot")
            testthat::expect_is(generate_plotly(result), "plotly")


            ###############################
            #test without providing name of function - this one should not work
            modelsettings =  list()
            result = run_model(modelsettings)
            testthat::expect_equal(result, "List element simfunction must be provided.")

            ###############################
            #test CI fit app - this one should work
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_confint_fit'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$modeltype = "_fit_"
            modelsettings$iter = 10
            modelsettings$nsample = 40
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot")
            testthat::expect_is(generate_text(result), "html")

            ###############################
            #test flu drug fit app
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_fludrug_fit'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$modeltype = "_fit_"
            modelsettings$iter = 10
            result = run_model(modelsettings)
            testthat::expect_is(generate_ggplot(result), "ggplot")
            testthat::expect_is(generate_text(result), "html")


          })
