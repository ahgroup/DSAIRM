context("test-generate_ggplot.R")

test_that("generate_ggplot returns a ggplot",
          {

            #manual call of simulator
            simresult=DSAIRM::simulate_Basic_Virus_Model_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            p = DSAIRM::generate_ggplot(result)
            expect_true( is.ggplot(p))


            simresult = DSAIRM::simulate_Basic_Virus_Model_stochastic()
            result = vector("list", 1)
            result[[1]]=simresult
            expect_is(generate_ggplot(result), "ggplot" )

            simresult = DSAIRM::simulate_basicbacteria_ode()
            result = vector("list", 1)
            result[[1]]=simresult
            expect_is(generate_ggplot(result), "ggplot" )

            simresult = DSAIRM::simulate_basicbacteria_discrete()
            result = vector("list", 1)
            result[[1]]=simresult
            result[[1]]$title = "Hello"
            result[[1]]$legendlocation = "left"
            result[[1]]$ylab = NULL
            result[[1]]$xlab = NULL
            expect_is(generate_ggplot(result), "ggplot" )

            modelsettings =  list()
            modelsettings$simfunction = 'simulate_drugresistance_stochastic'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)

            modelsettings$modeltype = "_stochastic_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1

            result = run_model(modelsettings)
            expect_is(generate_ggplot(result), "ggplot" )


          }) #end tests for ggplot

