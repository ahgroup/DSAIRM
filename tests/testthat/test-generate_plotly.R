context("test-generate_plotly.R")

test_that("generate_plotly returns a plotly plot",
          {
            simresult=DSAIRM::simulate_basicvirus_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( DSAIRM::generate_plotly(result), "plotly" )



            #manual call of simulator
            simresult=DSAIRM::simulate_basicvirus_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( DSAIRM::generate_plotly(result), "plotly" )


            simresult = DSAIRM::simulate_basicvirus_stochastic()
            result = vector("list", 1)
            result[[1]]=simresult
            expect_is( DSAIRM::generate_plotly(result), "plotly" )

            simresult = DSAIRM::simulate_basicbacteria_ode()
            result = vector("list", 1)
            result[[1]]=simresult
            expect_is( DSAIRM::generate_plotly(result), "plotly" )

            simresult = DSAIRM::simulate_basicbacteria_discrete()
            result = vector("list", 1)
            result[[1]]=simresult
            result[[1]]$title = "Hello"
            result[[1]]$legendlocation = "left"
            result[[1]]$ylab = NULL
            result[[1]]$xlab = NULL
            expect_is( DSAIRM::generate_plotly(result), "plotly" )

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
            expect_is( DSAIRM::generate_plotly(result), "plotly" )




          })

