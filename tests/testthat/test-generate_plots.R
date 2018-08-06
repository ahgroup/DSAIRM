context("test-generate_plots.R")

test_that("generate_plots returns a plot without specifying anything",
{
  simresult=DSAIRM::simulate_basicbacteria()
  result = vector("list", 1)
  result[[1]]$dat = simresult$ts
  expect_is( DSAIRM::generate_plots(result), "ggplot" )
})

test_that("generate_plots returns a plot when choosing scatterplot or boxplot",
          {
            simresult=DSAIRM::simulate_usanalysis()
            result = vector("list", 1)
            dat = data.frame(xvals = simresult$dat$g, yvals = simresult$dat$Bpeak, varnames = "Bpeak")
            result[[1]]$dat = dat
            result[[1]]$plottype = "Scatterplot"
            expect_is( DSAIRM::generate_plots(result), "ggplot" )
            result[[1]]$plottype = "Boxplot"
            expect_is( DSAIRM::generate_plots(result), "ggplot" )
          })
