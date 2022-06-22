context("test-generate_fctcalls.R")

test_that("generate_fctcalls produces the correct calls",
          {

            ########################################
            #run basic bacteria model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicbacteria_ode'
            modelsettings$simfunction2 = 'simulate_basicbacteria_discrete'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction2)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tfinal = 50
            modelsettings$modeltype = "_discrete_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ifinal = round(tail(result[[1]]$dat$yvals, 1))
            testthat::expect_equal(Ifinal, 13594)


            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicbacteria_ode'
            modelsettings$simfunction2 = 'simulate_basicbacteria_discrete'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tfinal = 50
            modelsettings$modeltype = "_ode_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ifinal = round(tail(result[[1]]$dat$yvals, 1))
            testthat::expect_equal(Ifinal, 13594)

            modelsettings =  list()
            modelsettings$simfunction = 'simulate_basicbacteria_ode'
            modelsettings$simfunction2 = 'simulate_basicbacteria_discrete'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction2)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tfinal = 50
            modelsettings$modeltype = "_ode_and_discrete_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1

            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Ifinal = round(tail(result[[1]]$dat$yvals, 1))
            testthat::expect_equal(Ifinal, 13594)


          })
