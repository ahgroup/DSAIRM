# Souce need code
source("generate_shinyinput.R")
# plot code
source("test_generate_plots_yang.R")

# test plot to plotly
# library(DSAIRM)
library(tidyverse)
library(plotly)

################################################
# simresult=DSAIRM::simulate_basicvirus_ode()
# produce data
simresult=DSAIRM::simulate_basicbacteria_ode()
# plot data structure
resultest = vector("list", 1)
resultest[[1]]$dat = simresult$ts
head(resultest[[1]]$dat)

################################################
# original ggplot
original_plot <- DSAIRM::generate_plots(resultest)
original_plot

# Plotly
plotly_plot <- generate_plotly(resultest) 
plotly_plot

# looks similar and acceptable in these test if data
# structure are same here

# Next test in app enviroment with new DSAIRM strcture
################################################
# problem !
# new data with test_plotly_yang.R cod
# (B,1) recurrence! so due to data structure
# Now need to check plot code for better fit plotly
# return to test_plotly_yang.R code

# app new data result[[1]]$dat
# data_from_app <<- result
## the problem had
head(data_from_app[[1]]$dat)
DSAIRM::generate_plots(data_from_app)
generate_plotly(data_from_app) 

# 1 basic code with app new data 
# form <- list(
#   title = "Time",
#   titlefont = f1,
#   showticklabels = TRUE,
#   tickangle = 45,
#   tickfont = f2,
#   exponentformat = "E"
# )


plot_ly(data_from_app[[1]]$dat, x = ~xvals , y = ~yvals)%>%
  add_lines(color = ~factor(IDvar), linetype = ~factor(IDvar)) %>% 
  layout(xaxis = list(title = "Time", size = 20), 
         yaxis = list(title = "Numbers", size = 20),
         # annotations = list(yref='paper',xref="paper",y=0.95,x=1.1, text="Compartments",showarrow=F),
         legend = list(y=1,x=0.95))

# 2 use browser to see how generate_plotly(data_from_app) work and try to add plotly
generate_plotly_0(data_from_app) 
generate_plotly_1(data_from_app) 
#
save.image("test.Rdata")

