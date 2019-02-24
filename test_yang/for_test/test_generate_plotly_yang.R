generate_plotly_1 <- function(res)
{


    #nplots contains the number of plots to be produced.
    nplots = length(res) #length of list

    allplots=list() #will hold all plots

    #lower and upper bounds for plots, these are used if none are provided by calling fuction
    lb = 1e-10;
    ub = 1e20;

    for (n in 1:nplots) #loop to create each plot
    { 

      resnow = res[[n]]

      #if a data frame called 'ts' exists, assume that this one is the data to be plotted
      #otherwise use the data frame called 'dat'
      #one of the 2 must exist, otherwise the function will not work
      if (!is.null(resnow$ts))
      {
        rawdat = resnow$ts #if a timeseries is sent in and no x- and y-labels provided, we set default 'Time' and 'Numbers'
        if (is.null(resnow$ylab)) {resnow$ylab = 'Numbers'}
        if (is.null(resnow$xlab)) {resnow$xlab = 'Time'}
      }
      else {
        rawdat = resnow$dat
      }

      plottype <- if( is.null(resnow$plottype) ){'Lineplot'} else { resnow$plottype } #if nothing is provided, we assume a line plot. That could lead to silly plots.
      
      #if the first column is called 'Time' (as returned from several of the simulators)
      #rename to xvals for consistency and so the code below will work
      if ( colnames(rawdat)[1] == 'Time' | colnames(rawdat)[1] == 'time' ) {colnames(rawdat)[1] <- 'xvals'}

      #for the plotting below, the data need to be in the form xvals/yvals/varnames
      #if the data is instead in xvals/var1/var2/var3/etc. - which is what the simulator functions produce
      #we need to re-format
      #if the data frame already has a column called 'varnames', we assume it's already properly formatted as xvals/yvals/varnames
      if ('varnames' %in% colnames(rawdat))
      {
        dat = rawdat
      }
      else
      {
        #using tidyr to reshape
        #dat = tidyr::gather(rawdat, -xvals, value = "yvals", key = "varnames")
        #using basic reshape function to reformat data
        dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], 
                             v.names = 'yvals', 
                             timevar = "varnames", 
                             times = colnames(rawdat)[-1], 
                             direction = 'long', 
                             new.row.names = NULL); 
        dat$id <- NULL
      }
      #########
      #code variable names as factor and level them so they show up right in plot - factor is needed for plotting and text
      mylevels = unique(dat$varnames)
      dat$varnames = factor(dat$varnames, levels = mylevels)
      #########
      # #see if user/calling function supplied x- and y-axis transformation information
      if (is.null(resnow$xscale)) {
        xscaletrans = "linear"
      }
      if (resnow$xscale == "identity") {
        xscaletrans = "linear"
      }
      if (resnow$xscale == "log10") {
        xscaletrans = "log"
      }
      if (is.null(resnow$yscale)) {
        yscaletrans = "linear"
      }
      if (resnow$yscale == "identity") {
        yscaletrans = "linear"
      }
      if (resnow$yscale == "log10") {
        yscaletrans = "log"
      }
      #########
      #if we want a plot on log scale, set any value in the data at or below 0 to some small number
      if (xscaletrans !='identity') {dat$xvals[dat$xvals<=0]=lb}
      if (yscaletrans !='identity') {dat$yvals[dat$yvals<=0]=lb}
      #########
      #if exist, apply user-supplied x- and y-axis limits
      #if min/max axes values are not supplied
      #we'll set them here to make sure they are not crazy high or low
      xmin <- if(is.null(resnow$xmin)) {max(lb,min(dat$xvals))} else  {resnow$xmin}
      ymin <- if(is.null(resnow$ymin)) {max(lb,min(dat$yvals))} else  {resnow$ymin}
      xmax <- if(is.null(resnow$xmax)) {min(ub,max(dat$xvals))} else  {resnow$xmax}
      ymax <- if(is.null(resnow$ymax)) {min(ub,max(dat$yvals))} else  {resnow$ymax}
      #########
      #set line size as given by app or to 1.5 by default
      linesize = ifelse(is.null(resnow$linesize), 2, resnow$linesize)
      #########
      #if the IDvar variable exists, use it for further stratification, otherwise just stratify on varnames
      if ( is.null(dat$IDvar) )
      {
        py1 <- dat %>% 
          plotly::plot_ly()
      }
      else
      {
        py1 <- dat %>% group_by(IDvar) %>%
          plotly::plot_ly(x = ~xvals)
      }
      #########
      if (plottype == 'Scatterplot')
      {
        py2 <- py1 %>% add_markers(x = ~xvals , y = ~yvals, color = ~varnames, symbol = ~varnames)
      }
      if (plottype == 'Boxplot')
      {
        py2 <- py1 %>% plotly::add_boxplot(y = ~yvals,color = ~varnames)
      }
      #########
      if (plottype == 'Lineplot') #if nothing is provided for plottype, we assume a lineplot is wanted
      {
        py2 <- py1 %>% 
          plotly::add_trace(x = ~xvals ,y = ~yvals, 
                            type = 'scatter', mode = 'lines+markers', linetype = ~varnames,symbol=~varnames,
                            line = list(color = ~varnames, width = linesize),
                            marker = list(size = linesize*3))
      }
      #########
      if (plottype == 'Mixedplot')
      {
        py1a <- py1 %>% 
          plotly::add_trace(data = dplyr::filter(dat,style == 'line'), 
                            x = ~xvals, y = ~yvals, 
                            type = 'scatter', mode = 'lines+markers', linetype = ~varnames,symbol=~varnames,
                            line = list(color = ~varnames, width = linesize, symbols = ~varnames),
                            marker = list(size = linesize*3)) 
        
        py2 <- py1a %>% 
          add_markers(data = dplyr::filter(dat,style == 'point'),
                      x = ~xvals, y = ~yvals, color = ~varnames,
                      marker = list(size = linesize*3))

      }
      #########
      #no numbering/labels on x-axis for boxplots
      if (plottype == 'Boxplot')
      {
        py3 <- py2 %>% layout(xaxis = list(showticklabels = FALSE))
      }
      else
      {
        if (resnow$xscale == "log10") {
          py3 <- py2 %>% layout(xaxis = list(range = c(log(xmin),log(xmax)), type = xscaletrans ))
        }
        else{
          py3 <- py2 %>% layout(xaxis = list(range = c(xmin,xmax), type = xscaletrans ))
        }
        
        if (!is.null(resnow$xlab)) { 
          py3 = py3 %>% layout(xaxis = list(title=resnow$xlab, size = 18, type = xscaletrans)) }
      }
      #########
      if (resnow$yscale == "log10") {
        py4 = py3 %>% layout(yaxis = list(range = c(log(ymin),log(ymax)), type = yscaletrans) )
      }
      else{
        py4 = py3 %>% layout(yaxis = list(range = c(ymin,ymax), type = yscaletrans) )
      }
      if (!is.null(resnow$ylab)) { 
        py4 = py4 %>% 
        layout(yaxis = list(title=resnow$ylab, type = yscaletrans)) }
      #########
      #apply title if provided
      if (!is.null(resnow$title))
      {
        py4 = py4 %>% layout(title = resnow$title)
      }
      #########      
      py4 = py4 %>% layout(legend = list(font = list(size = 14)),
                           yaxis = list(titlefont = list(size = 18)),
                           xaxis = list(titlefont = list(size = 18)))
      pfinal = py4
      allplots[[n]] = pfinal
    } #end loop over individual plots

    #using gridExtra pacakge for multiple plots, ggplot for a single one
    #potential advantage is that for a single ggplot, one could use interactive features
    #such as klicking on point and displaying value
    #currently not implemented
    #cowplot is an alternative to arrange plots.
    #There's a reason I ended up using grid.arrange() instead of cowplot but I can't recall
    # browser()
    if (n>1)
    {
      #number of columns needs to be stored in 1st list element
      gridExtra::grid.arrange(grobs = allplots, ncol = res[[1]]$ncol)

    }
    if (n==1)
    {
      print(pfinal)
    }
}
