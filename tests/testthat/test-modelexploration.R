context("test-modelexploration.R")

test_that("test that modelexploration app returns the proper plots",
          {

            modelsettings = list()

            modelsettings$samples = 10
            modelsettings$parmin = 1e-08
            modelsettings$parmax = 1e-05
            modelsettings$samplepar = "k"
            modelsettings$pardist = "lin"

            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1
            modelsettings$rngseed = 100

            modelsettings$modeltype = '_modelexploration_'
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_basicbacteria_modelexploration'
            modelsettings$plotscale = 'x'

            result = run_model(modelsettings)

            p = DSAIRM::generate_ggplot(result)
            expect_true( is.ggplot(p))

            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )


            modelsettings$pardist = "log"
            modelsettings$samplepar = "r"
            result = run_model(modelsettings)

            p = DSAIRM::generate_ggplot(result)
            expect_true( is.ggplot(p))

            })

