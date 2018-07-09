#' @title A helper function that takes result from the simulators and produces plots
#'
#' @description This function generates plots to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param res a list structure containing all simulation results that are to be plotted
#'    the length of the list indicates the number of separate plots to make
#'    each list entry corresponds to one plot
#'    each list entry needs to contain the following information/elements:
#'    1. a data frame called "dat" with one column called xvals, one column yvals,
#'    one column called varnames that contains names for different variables.
#'    varnames needs to be a factor variable or will be converted to one.
#'    Optional, one column called IDvar for further grouping (i.e. multiple lines for stochastic simulations).
#'    If plottype is 'mixedplot' an additional column called 'style' indicating line or point plot
#'    for each variable is needed.
#'    2. meta-data for the plot, provided in the following variables:
#'    optional: plottype - one of "Lineplot" (default is nothing is provided),"Scatterplot","Boxplot", "Mixedplot".
#'    optional: xlab, ylab - strings to label axes.
#'    optional: xscale, yscale - scaling of axes, valid ggplot2 expression, e.g. "identity" or "log10".
#'    optional: xmin, xmax, ymin, ymax - manual min and max for axes.
#'    optional: legendtitle - legend title, if NULL/not suppied no legend will be plotted.
#'    optional: linesize - width of line, numeric, i.e. 1.5, 2, etc. set to 1.5 if not supplied.
#'
#' @return a plot structure for display in a shiny UI
#' @details This function is called by the shiny server to produce plots returned to the shiny UI
#' Create plots run the simulation with default parameters just call this function
#' result <- simulate_basicbacteria()
#' @author Andreas Handel
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @export

generate_plots <- function(res)
{

    #nplots contains the number of plots to be produced.
    nplots = length(res) #length of list

    allplots=list() #will hold all plots

    #lower and upper bounds for plots, these are used if none are provided by calling fuction
    lb = 1e-10;
    ub = 1e20;

    for (n in 1:nplots) #loop to create each plot
    {
      plottype <- if(is.null(res[[n]]$plottype)) {'Lineplot'} else  {res[[n]]$plottype} #if nothing is provided, we assume a line plot. That could lead to silly plots.
      dat = res[[n]]$dat

      #code variable names as factor and level them so they show up right in plot - factor is needed for plotting and text
      mylevels = unique(dat$varnames)
      dat$varnames = factor(dat$varnames, levels = mylevels)


      #see if user/calling function supplied x- and y-axis transformation information
      xscaletrans <- ifelse(is.null(res[[n]]$xscale), 'identity',res[[n]]$xscale)
      yscaletrans <- ifelse(is.null(res[[n]]$yscale), 'identity',res[[n]]$yscale)

      #if we want a plot on log scale, set any value in the data at or below 0 to some small number
      if (xscaletrans !='identity') {dat$xvals[dat$xvals<=0]=lb}
      if (yscaletrans !='identity') {dat$yvals[dat$yvals<=0]=lb}

      #if exist, apply user-supplied x- and y-axis limits
      #if min/max axes values are not supplied
      #we'll set them here to make sure they are not crazy high or low
      xmin <- if(is.null(res[[n]]$xmin)) {lb} else  {res[[n]]$xmin}
      ymin <- if(is.null(res[[n]]$ymin)) {lb} else  {res[[n]]$ymin}
      xmax <- if(is.null(res[[n]]$xmax)) {min(ub,max(res[[n]]$dat$xvals))} else  {res[[n]]$xmax}
      ymax <- if(is.null(res[[n]]$ymax)) {min(ub,max(res[[n]]$dat$yvals))} else  {res[[n]]$ymax}

      #set line size as given by app or to 1.5 by default
      linesize = ifelse(is.null(res[[n]]$linesize), 1.5, res[[n]]$linesize)

      #if the IDvar variable exists, use it for further stratification, otherwise just stratify on varnames
      if (is.null(dat$IDvar))
      {
        p1 = ggplot2::ggplot(dat, ggplot2::aes(x = xvals, y = yvals, color = varnames, linetype = varnames, shape = varnames) )
      }
      if (!is.null(dat$IDvar))
      {
        p1 = ggplot2::ggplot(dat, ggplot2::aes(x = xvals, y = yvals, color = varnames, linetype = varnames, group = IDvar) )
      }

      if (plottype == 'Scatterplot')
      {
        p2 = p1 + ggplot2::geom_point( size = linesize, na.rm=TRUE)
      }
      if (plottype == 'Boxplot')
      {
        p2 = p1 + ggplot2::geom_boxplot()
      }
      if (plottype == 'Lineplot') #if nothing is provided for plottype, we assume a lineplot is wanted
      {
        p2 = p1 + ggplot2::geom_line(size = linesize, na.rm=TRUE)
      }
      if (plottype == 'Mixedplot')
      {
        #a mix of lines and points. for this, the dataframe needs to contain an extra column indicating line or point
        p1a = p1 + ggplot2::geom_line(data = dplyr::filter(dat,style == 'line'), size = linesize)
        p2 = p1a + ggplot2::geom_point(data = dplyr::filter(dat,style == 'point'), size = 2.5*linesize)
      }



      #no numbering/labels on x-axis for boxplots
      if (plottype == 'Boxplot')
      {
        p3 = p2 + ggplot2::scale_x_continuous(trans = xscaletrans, limits=c(xmin,xmax), breaks = NULL, labels = NULL)
      }
      if (plottype != 'Boxplot')
      {
        p3 = p2 + ggplot2::scale_x_continuous(trans = xscaletrans, limits=c(xmin,xmax))
        if (!is.null(res[[n]]$xlab)) { p3 = p3 + ggplot2::xlab(res[[n]]$xlab) }
      }

      #apply y-axis
      p5 = p3 + ggplot2::scale_y_continuous(trans = yscaletrans, limits=c(ymin,ymax))
      if (!is.null(res[[n]]$ylab)) { p5 = p5 + ggplot2::ylab(res[[n]]$ylab) }

      #do legend
      if (is.null(res[[n]]$legend))
      {
        p6 = p5 + ggplot2::theme(legend.position="none")
      }
      else
      {
        p6 = p5 + ggplot2::theme(legend.key.width = grid::unit(3,"line")) + ggplot2::scale_colour_discrete(name  = res[[n]]$legend) + ggplot2::scale_linetype_discrete(name = res[[n]]$legend) + ggplot2::scale_shape_discrete(name = res[[n]]$legend)
      }

      pfinal = p6
      allplots[[n]] = pfinal

    } #end loop over individual plots

    #using gridExtra pacakge for multiple plots, ggplot for a single one
    #potential advantage is that for a single ggplot, one could use interactive features
    #such as klicking on point and displaying value
    #currently not implemented
    #cowplot is an alternative to arrange plots.
    #There's a reason I ended up using grid.arrange() instead of cowplot but I can't recall

    if (n>1)
    {
      #number of columns needs to be stored in 1st list element
      gridExtra::grid.arrange(grobs = allplots, ncol = res[[1]]$ncol)
      #cowplot::plot_grid(plotlist = allplots, ncol = res[[1]]$ncol)

    }
    if (n==1)
    {
      graphics::plot(pfinal)
    }
}
