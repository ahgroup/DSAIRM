# test plot to plotly
library(DSAIRM)
library(ggplot2)
library(plotly)

# simresult=DSAIRM::simulate_basicvirus_ode()
simresult=DSAIRM::simulate_basicbacteria_ode()
resultest = vector("list", 1)
resultest[[1]]$dat = simresult$ts
# DSAIRM::generate_plots(result)
generate_plotly(resultest) 
generate_plotly(result) 

save.image("test.Rdata")

