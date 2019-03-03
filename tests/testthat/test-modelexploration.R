context("test-modelexploration.R")

test_that("test that modelexploration app returns the proper plots",
          {

            modelsettings = list()

            modelsettings$B = 10
            modelsettings$I = 1
            modelsettings$g = 1
            modelsettings$Bmax = 1e+06
            modelsettings$dB = 0.1
            modelsettings$k = 1e-07
            modelsettings$r = 0.001
            modelsettings$dI = 1
            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1

            modelsettings$samples = 10
            modelsettings$parmin = 1e-08
            modelsettings$parmax = 1e-05
            modelsettings$samplepar = "k"
            modelsettings$pardist = "lin"


            modelsettings$rngseed = 100
            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1

            modelsettings$modeltype = '_modelexploration_'
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_modelexploration'
            modelsettings$plotscale = 'x'

            result = run_model(modelsettings)
            usplot = generate_ggplot(result)
            testthat::expect_is( usplot, "ggplot" )

            usplot = generate_plotly(result)
            testthat::expect_is( usplot, "plotly" )
            })

