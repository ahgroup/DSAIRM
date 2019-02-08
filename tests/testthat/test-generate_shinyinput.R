context("test-generate_shinyinput.R")


test_that("generate_shinyinput correctly produces a shiny input structure",
{

            packagename = 'DSAIRM'
            otherinputs = NULL
            mbmodel = 'simulate_virusandir_ode'

            inputs = generate_shinyinput(mbmodel, otherinputs, packagename)
            #this element of the tag list needs to contain the word susceptible
            expect_true(grepl('uninfected',inputs[[2]][[1]][[3]]))

})
