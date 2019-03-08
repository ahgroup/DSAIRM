#' @title A helper function that takes result from the simulators and produces plots
#'
#' @description This function generates plots to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list.
#' @param res A list structure containing all simulation results that are to be plotted.
#'    The length of the list indicates the number of separate plots to make.
#'    Each list entry corresponds to one plot and
#'    needs to contain the following information/elements: \cr
#'    1. A data frame called "dat" or "ts". If the data frame is "ts" it is assumed to be
#'    a time series and by default a line plot will be produced and labeled Time/Numbers.
#'    For plotting, the data needs to be in a format with one column called xvals, one column yvals,
#'    one column called varnames that contains names for different variables.
#'    Varnames needs to be a factor variable or will be converted to one.
#'    If a column 'varnames' exist, it is assumed the data is in the right format. Otherwise it will be transformed.
#'    An optional column called IDvar can be provided for further grouping (i.e. multiple lines for stochastic simulations).
#'    If plottype is 'mixedplot' an additional column called 'style' indicating line or point plot
#'    for each variable is needed. \cr
#'    2. Meta-data for the plot, provided in the following variables: \cr
#'    optional: plottype - One of "Lineplot" (default is nothing is provided),"Scatterplot","Boxplot", "Mixedplot". \cr
#'    optional: xlab, ylab - Strings to label axes. \cr
#'    optional: xscale, yscale - Scaling of axes, valid ggplot expression, e.g. "identity" or "log10". \cr
#'    optional: xmin, xmax, ymin, ymax - Manual min and max for axes. \cr
#'    optional: makelegend - TRUE/FALSE, add legend to plot. Assume true if not provided. \cr
#'    optional: legendtitle - Legend title, if NULL/not supplied, default is used \cr
#'    optional: legendlocation - if "left" is specified, top left. Otherwise top right. \cr
#'    optional: linesize - Width of line, numeric, i.e. 1.5, 2, etc. set to 1.5 if not supplied. \cr
#'    optional: title - A title for each plot. \cr
#'    optional: for multiple plots, specify res[[1]]$ncols to define number of columns \cr
#'
#' @return A plotly plot structure for display in a Shiny UI.
#' @details This function is called by the Shiny server to produce plots returned to the Shiny UI.
#' Create plots run the simulation with default parameters just call the function:
#' result <- simulate_basicbacteria()
#' plot <- generate_plotly(result)
#' @import plotly
#' @importFrom stats reshape
#' @author Yang Ge, Andreas Handel
#' @export

generate_plotly <- function(res)
{

    #nplots contains the number of plots to be produced.
    nplots = length(res) #length of list

    allplots=list() #will hold all plots

    #lower and upper bounds for plots, these are used if none are provided by calling function
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
            #using basic reshape function to reformat data
        dat = stats::reshape(rawdat, varying = colnames(rawdat)[-1], v.names = 'yvals', timevar = "varnames", times = colnames(rawdat)[-1], direction = 'long', new.row.names = NULL)
		dat$id <- NULL
      }

      #code variable names as factor and level them so they show up right in plot - factor is needed for plotting and text
      mylevels = unique(dat$varnames)
      dat$varnames = factor(dat$varnames, levels = mylevels)

      #see if user/calling function supplied x- and y-axis transformation information
      xscaletrans <- ifelse(is.null(resnow$xscale), 'identity',resnow$xscale)
      yscaletrans <- ifelse(is.null(resnow$yscale), 'identity',resnow$yscale)


      #if exist, apply user-supplied x- and y-axis limits
      #if min/max axes values are not supplied
      #we'll set them here to make sure they are not crazy high or low
      xmin <- if(is.null(resnow$xmin)) {max(lb,min(dat$xvals))} else  {resnow$xmin}
      ymin <- if(is.null(resnow$ymin)) {max(lb,min(dat$yvals))} else  {resnow$ymin}
      xmax <- if(is.null(resnow$xmax)) {min(ub,max(dat$xvals))} else  {resnow$xmax}
      ymax <- if(is.null(resnow$ymax)) {min(ub,max(dat$yvals))} else  {resnow$ymax}

      #if we want a plot on log scale, set any value in the data at or below 0 to some small number
      #also re-scale min and max and rename from log10 (used for ggplot) to log
      if (xscaletrans !='identity')
      {
        dat$xvals[dat$xvals<=0]=lb
        xscaletrans = "log"
        xmin = log10(xmin); xmax=log10(xmax)
      }
      if (yscaletrans !='identity')
      {
        dat$yvals[dat$yvals<=0]=lb
        yscaletrans = "log"
        ymin = log10(ymin); ymax=log10(ymax)
      }


      #set line size as given by app or to 1.5 by default
      linesize = ifelse(is.null(resnow$linesize), 3, resnow$linesize)

      #if the IDvar variable exists, use it for further stratification, otherwise just stratify on varnames
      if ( is.null(dat$IDvar) )
      {
        py1 <- plotly::plot_ly(dat)
      }
      else
      {
        py1 <-  plotly::plot_ly(dplyr::group_by(dat, IDvar), x = ~xvals)
      }

      ###choose between different types of plots
      if (plottype == 'Scatterplot')
      {
        py2 <- plotly::add_markers(py1, x = ~xvals , y = ~yvals, color = ~varnames, symbol = ~varnames)
      }
      if (plottype == 'Boxplot')
      {
        py2 <- plotly::add_boxplot(py1, y = ~yvals, name = ~varnames)
      }

      if (plottype == 'Lineplot')
      {
        py2 <- plotly::add_trace(py1, x = ~xvals ,y = ~yvals,
                            type = 'scatter', mode = 'lines', linetype = ~varnames,
                            line = list(color = ~varnames, width = linesize))
      }

      ###
      if (plottype == 'Mixedplot')
      {
        py1a <- plotly::add_trace(py1, data = dplyr::filter(dat,style == 'line'),
                            x = ~xvals, y = ~yvals,
                            type = 'scatter', mode = 'lines', linetype = ~varnames,
                            line = list(color = ~varnames, width = linesize))

        py2 <- plotly::add_markers(py1a, data = dplyr::filter(dat,style == 'point'),
                      x = ~xvals, y = ~yvals, color = ~varnames,
                      marker = list(size = linesize*3))
      }

      #set x-axis. no numbering/labels on x-axis for boxplots
      if (plottype == 'Boxplot')
      {
        py3 <- plotly::layout(py2, xaxis = list(showticklabels = F))
      }
      else
      {
          py3 <- plotly::layout(py2, xaxis = list(range = c(xmin,xmax), type = xscaletrans ))
          if (!is.null(resnow$xlab)) {
			    py3 <- plotly::layout(py3, xaxis = list(title=resnow$xlab, size = 18))
		      }
      }

      #apply y-axis and if provided, label
      py4 = plotly::layout(py3, yaxis = list(range = c(ymin,ymax), type = yscaletrans) )
	    if (!is.null(resnow$ylab)) {
          py4 <- plotly::layout(py4, yaxis = list(title=resnow$ylab, size = 18))
      }

      #apply title if provided
      if (!is.null(resnow$title))
      {
        py4 = plotly::layout(py4, title = resnow$title)
      }


      #do legend if TRUE or not provided
      if (!is.null(resnow$makelegend) && resnow$makelegend == FALSE)
      {
        py4 = plotly::layout(py4, showlegend = FALSE)
      }


      pfinal = py4
      allplots[[n]] = pfinal
    } #end loop over individual plots

    if (n>1)
    {
      resultplot <-  plotly::subplot(allplots, titleY = TRUE, titleX = TRUE)
    }
    if (n==1)
    {
      resultplot <- pfinal
    }
     #browser()
    return(resultplot)
}
