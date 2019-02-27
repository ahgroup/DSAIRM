context("test-generate_ggplot.R")

test_that("generate_ggplot returns a ggplot",
          {
            simresult=DSAIRM::simulate_basicvirus_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( DSAIRM::generate_ggplot(result), "ggplot" )
          })

