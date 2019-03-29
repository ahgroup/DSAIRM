context("test-generate_usplot.R")

test_that("running US analysis app returns the proper plots",
          {

            modelsettings = list()

            modelsettings$Bmin = 1000
            modelsettings$Bmax = 1500
            modelsettings$Imin = 1
            modelsettings$Imax = 10
            modelsettings$Bmaxmin = 1e4
            modelsettings$Bmaxmax = 1e6
            modelsettings$dBmin = 0.1
            modelsettings$dBmax = 1
            modelsettings$kmin = 1e-7
            modelsettings$kmax = 1e-6
            modelsettings$rmin = 1e-3
            modelsettings$rmax = 1e-2
            modelsettings$dImin = 1
            modelsettings$dImax = 2
            modelsettings$gmean = 1
            modelsettings$gvar = 0.1

            modelsettings$samples = 10
            modelsettings$rngseed = 100
            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1

            modelsettings$modeltype = '_usanalysis_'
            modelsettings$nplots = 3
            modelsettings$ncols = 3
            modelsettings$simfunction = 'simulate_usanalysis'
            modelsettings$plotscale = 'y'
            modelsettings$samplepar = 'g'

            #test boxplots
            modelsettings$plottype = 'Boxplot'
            result = run_model(modelsettings)

            usplot = generate_ggplot(result)
            testthat::expect_is( usplot, "gtable" )

            usplot = generate_plotly(result)
            testthat::expect_is( usplot, "plotly" )

            ustext = generate_text(result)
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )

            #test scatterplots
            modelsettings$plottype = 'Scatterplot'
            result = run_model(modelsettings)

            testthat::expect_is( generate_ggplot(result), "gtable" )
            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )

            })

