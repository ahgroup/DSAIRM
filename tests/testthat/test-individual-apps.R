context("test-individual-apps.R")


test_that("individual simulator functions run correctly",
{
            result <- simulate_modelvariants_ode(V = 100, k1 = 0 , k2 = 0, k3 = 1e-4)
            testthat::expect_equal(max(result$ts$A), 0)

            result <- simulate_pkpdmodel_ode(V = 100, txstart = 10, n = 1e5, dU = 1e-2)
            testthat::expect_equal(max(result$ts$V), 1039545)

            result <- simulate_drugresistance_stochastic(tfinal = 200, e = 0.5)
            testthat::expect_equal(max(result$ts$Vr), 344)

            result <- simulate_basicvirus_modelexploration(samples=5, samplepar='dI', parmin=1, parmax=10)
            testthat::expect_equal(round(max(result$dat$Vpeak),0), 52772)

})
