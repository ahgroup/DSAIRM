# test plot to plotly
library(DSAIRM)
library(ggplot2)
library(plotly)

simresult=DSAIRM::simulate_basicvirus_ode()
result = vector("list", 1)
result[[1]]$dat = simresult$ts
# DSAIRM::generate_plots(result)
generate_plotly(result)

