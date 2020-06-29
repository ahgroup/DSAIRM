context("test-stochastic-virus-app.R")

test_that("test that stochastic virus app works",
          {

            modelsettings = list()

            modelsettings$U = 1e4
            modelsettings$I = 1
            modelsettings$V = 1
            modelsettings$n = 1e3
            modelsettings$dU = 0.1
            modelsettings$dI = 1
            modelsettings$dV = 2
            modelsettings$b = 1e-4
            modelsettings$p = 10
            modelsettings$g = 1

            modelsettings$rngseed = 100
            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1

            modelsettings$nplots = 1
            modelsettings$nreps = 5
            modelsettings$simfunction = c('simulate_basicvirus_ode', 'simulate_basicvirus_stochastic')
            modelsettings$plotscale = 'y'

            modelsettings$modeltype = '_ode_and_stochastic_'
            modelsettings$modeltypeUI = '_ode_'
            result = run_model(modelsettings)

            p = DSAIRM::generate_ggplot(result)
            expect_true( is.ggplot(p))

            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )

            modelsettings$modeltype = '_ode_and_stochastic_'
            modelsettings$modeltypeUI = '_stochastic_'
            result = run_model(modelsettings)
            p = DSAIRM::generate_ggplot(result)
            expect_true( is.ggplot(p))
            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )

            modelsettings$modeltype = '_ode_and_stochastic_'
            modelsettings$modeltypeUI = '_ode_and_stochastic_'
            result = run_model(modelsettings)
            p = DSAIRM::generate_ggplot(result)
            expect_true( is.ggplot(p))
            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )


            })

