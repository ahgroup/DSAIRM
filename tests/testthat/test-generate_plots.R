context("test-generate_plots.R")

test_that("generate_plots returns a plot",
          {
            simresult=DSAIRM::simulate_basicvirus_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( DSAIRM::generate_plots(result), "ggplot" )
          })

