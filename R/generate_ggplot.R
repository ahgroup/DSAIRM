#' @title A helper function that takes simulation results and produces ggplot plots
#'
#' @description This function generates plots to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list.
#' @param res A list structure containing all simulation results that are to be plotted.
#'    The length of the main list indicates the number of separate plots to make.
#'    Each list entry is itself a list, and corresponds to one plot and
#'    needs to contain the following information/elements: \cr
#'    1. A data frame list element called "dat" or "ts". If the data frame is "ts" it is assumed to be
#'    a time series and by default a line plot will be produced and labeled Time/Numbers.
#'    For plotting, the data needs to be in a format with one column called xvals, one column yvals,
#'    one column called varnames that contains names for different variables.
#'    Varnames needs to be a factor variable or will be converted to one.
#'    If a column 'varnames' exist, it is assumed the data is in the right format. Otherwise it will be transformed.
#'    An optional column called IDvar can be provided for further grouping (i.e. multiple lines for stochastic simulations).
#'    If plottype is 'mixedplot' an additional column called 'style' indicating line or point plot
#'    for each variable is needed. \cr
#'    2. Meta-data for the plot, provided in the following variables: \cr
#'    optional: plottype - One of "Lineplot" (default if nothing is provided),"Scatterplot","Boxplot", "Mixedplot". \cr
#'    optional: xlab, ylab - Strings to label axes. \cr
#'    optional: xscale, yscale - Scaling of axes, valid ggplot2 expression, e.g. "identity" or "log10". \cr
#'    optional: xmin, xmax, ymin, ymax - Manual min and max for axes. \cr
#'    optional: makelegend - TRUE/FALSE, add legend to plot. Assume true if not provided. \cr
#'    optional: legendtitle - Legend title, if NULL/not supplied, default is used \cr
#'    optional: legendlocation - if "left" is specified, top left. Otherwise top. \cr
#'    optional: linesize - Width of line, numeric, i.e. 1.5, 2, etc. set to 1.5 if not supplied. \cr
#'    optional: pallette - overwrite plot colors by providing a vector of color names or hex numbers to be used for the plot. \cr
#'    optional: title - A title for each plot. \cr
#'    optional: for multiple plots, specify res[[1]]$ncols to define number of columns \cr
#'
#' @return A ggplot plot structure for display in a Shiny UI.
#' @details This function can be called to produce plots, i.e. those displayed for each app.
#' The input needed by this function is produced by either calling the \code{\link{run_model}} function (as done when going through the UI)
#' or manually transforming the output from a simulate_ function into the correct list structure as explained here.
#' @rawNamespace import(ggplot2, except = last_plot)
#' @importFrom stats reshape
#' @importFrom gridExtra grid.arrange
#' @importFrom rlang .data
#' @author Andreas Handel
#' @export

generate_ggplot <- function(res)
{

    # change ggplot color palette to color-blind friendly
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
    # I added more colors at the end to have 12, enough for all simulations
    # the ones I added are likely not color-blind friendly but rarely used in the app
    cbfpalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#00523B","#D5C711","#0019B2","#cc0000")

    #nplots contains the number of plots to be produced.
    nplots = length(res) #length of list

    allplots=list() #will hold all plots


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

      #if nothing is provided, we assume a line plot. That could lead to silly plots.
      plottype <- if(is.null(resnow$plottype)) {'Lineplot'} else  {resnow$plottype}

      #if the first column is called 'Time' (as returned from several of the simulators)
      #rename to xvals for consistency so the code below will work
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
      dat$varnames = factor(dat$varnames, levels = mylevels, ordered = TRUE)

      #see if user/calling function supplied x- and y-axis transformation information
      xscaletrans <- ifelse(is.null(resnow$xscale), 'identity',resnow$xscale)
      yscaletrans <- ifelse(is.null(resnow$yscale), 'identity',resnow$yscale)

      #lower and upper bounds for plots, these are used if none are provided by calling function
      lb = 1e-10
      ub = 1e20

      #if we want a plot on log scale, set any value in the data at or below 0 to some small number
      if (xscaletrans !='identity') {dat$xvals[dat$xvals<=0]=lb}
      if (yscaletrans !='identity') {dat$yvals[dat$yvals<=0]=lb}

      #if exist, apply user-supplied x- and y-axis limits
      #if min/max axes values are not supplied
      #we'll set them here to make sure they are not crazy high or low
      xmin <- if(is.null(resnow$xmin)) {max(lb,min(dat$xvals))} else  {resnow$xmin}
      ymin <- if(is.null(resnow$ymin)) {max(lb,min(dat$yvals))} else  {resnow$ymin}
      xmax <- if(is.null(resnow$xmax)) {min(ub,max(dat$xvals))} else  {resnow$xmax}
      ymax <- if(is.null(resnow$ymax)) {min(ub,max(dat$yvals))} else  {resnow$ymax}

      #set line size as given by app or to 1.5 by default
      linesize = ifelse(is.null(resnow$linesize), 1.5, resnow$linesize)


      #if the IDvar variable exists, use it for further stratification, otherwise just stratify on varnames
	    #the unusual notation for the aes settings is needed for use inside a package
      #see here: https://ggplot2.tidyverse.org/dev/articles/ggplot2-in-packages.html
      if (is.null(dat$IDvar))
	    {
	      p1 = ggplot2::ggplot(dat, ggplot2::aes(x = .data$xvals) )
	    }
      else
      {
        p1 = ggplot2::ggplot(dat, ggplot2::aes(x = .data$xvals, group = .data$IDvar) )
      }

      ###choose between different types of plots
      if (plottype == 'Scatterplot')
      {
        p2 = p1 + ggplot2::geom_point(data = dat, aes(y = .data$yvals, color = factor(.data$varnames, ordered = FALSE), shape = factor(.data$varnames, ordered = FALSE)), size = linesize, na.rm=TRUE)
      }
      if (plottype == 'Boxplot')
      {
        p2 = p1 + ggplot2::geom_boxplot(data = dat, aes( y = .data$yvals, color = as.factor(.data$varnames)), size = linesize, na.rm=TRUE)
      }
      if (plottype == 'Lineplot')
      {
        p2 = p1 + ggplot2::geom_line(data = dat, aes( y = .data$yvals, color = as.factor(.data$varnames), linetype = as.factor(.data$varnames)), size = linesize, na.rm=TRUE)
      }
      if (plottype == 'Mixedplot')
      {
        #a mix of lines and points. for this, the dataframe needs to contain an extra column indicating line or point
        p1a = p1 + ggplot2::geom_line(data = dplyr::filter(dat,style == 'line'), aes( y = .data$yvals, color = as.factor(.data$varnames), linetype = as.factor(.data$varnames)), size = linesize)
        p2 = p1a + ggplot2::geom_point(data = dplyr::filter(dat,style == 'point'), aes( y = .data$yvals, color = as.factor(.data$varnames), shape = factor(.data$varnames, ordered = FALSE)), size = 2.5*linesize)
      }


  	 #set x-axis. no numbering/labels on x-axis for boxplots
  	 if (plottype == 'Boxplot')
     {
          p3 = p2 + ggplot2::scale_x_continuous(trans = xscaletrans, limits=c(xmin,xmax), breaks = NULL, labels = NULL)
          p3 = p3 + ggplot2::labs(x = NULL)
      }
      else
      {
          p3 = p2 + ggplot2::scale_x_continuous(trans = xscaletrans, limits=c(xmin,xmax))
          if (!is.null(resnow$xlab)) { p3 = p3 + ggplot2::xlab(resnow$xlab) }
      }

      #apply y-axis and if provided, label
      p4 = p3 + ggplot2::scale_y_continuous(trans = yscaletrans, limits=c(ymin,ymax))
      if (!is.null(resnow$ylab)) { p4 = p4 + ggplot2::ylab(resnow$ylab) }

      #apply title if provided
      if (!is.null(resnow$title))
      {
        p4 = p4 + ggplot2::ggtitle(resnow$title)
      }

      #modify overall theme
      p5 = p4 + ggplot2::theme_bw(base_size = 18)

      #default palette is set, overwritten if user provided
      plotpalette = cbfpalette
      if (!is.null(resnow$palette)) {plotpalette = resnow$palette }

      #do legend if TRUE or not provided
      if (is.null(resnow$makelegend) || resnow$makelegend)
      {
        if (!is.null(resnow$legendlocation) && resnow$legendlocation == "left")
        {
             legendlocation = c(0,1)
        }
        else #default placement on top
        {
           legendlocation = "top"
        }
        legendtitle = ifelse(is.null(resnow$legendtitle), "Variables", resnow$legendtitle)


        if (plottype != 'Mixedplot')
        {
          nvars = length(unique(dat$varnames))
          p5a = p5 + ggplot2::guides(col = ggplot2::guide_legend(nrow=2, byrow=TRUE,title.position = 'left'))
          p5b = p5a + ggplot2::theme(legend.position = legendlocation) #default is top
          p5c = p5b + ggplot2::theme(legend.key.width = grid::unit(3, "line")) #line thickness
          p5d = p5c + ggplot2::scale_colour_manual(name = legendtitle, values=plotpalette[1:nvars]) #color for each variable
          p5e = p5d + ggplot2::scale_linetype_discrete(name = legendtitle) #line type for each variable
          pfinal = p5e + ggplot2::scale_shape_discrete(name = legendtitle) #symbol type for symbols
        }
        if (plottype == 'Mixedplot')
        {
          #trying to get legend right for combined line and symbol plots
          #not fully working yet
          #for data/symbols, legend still shows both lines and symbols, no matter what the plot is
          # Compute the number of types and methods
          npoints = length(unique(dplyr::filter(dat,style == 'point')$varnames))
          nlines = length(unique(dplyr::filter(dat,style == 'line')$varnames))
          p5a = p5 + ggplot2::guides(col = ggplot2::guide_legend(nrow=2, byrow=TRUE,title.position = 'left'))
          p5b = p5a + ggplot2::theme(legend.position = legendlocation) #default is top
          p5c = p5b + ggplot2::theme(legend.key.width = grid::unit(3, "line")) #line thickness
          p5d = p5c + ggplot2::scale_colour_manual(name = legendtitle, values=plotpalette[1:(nlines+npoints)]) #color for each variable
          p5e = p5d + ggplot2::scale_linetype_discrete(name = legendtitle, guide = "none") #symbol type for symbols; here is some trickery to make the legend look combined (turn off legend title/name)
          pfinal = p5e + ggplot2::scale_shape_discrete(name = "", guide = "none") #symbol type for symbols
        }
      } #end doing legend
      else
      {
        pfinal = p5 + ggplot2::theme(legend.position="none") + ggplot2::scale_colour_manual(values=plotpalette)
      }

      allplots[[n]] = pfinal



    } #end loop over individual plots

    #using gridExtra pacakge for multiple plots, ggplot for a single one
    #potential advantage is that for a single ggplot, one could use interactive features
    #such as klicking on point and displaying value
    #currently not implemented
    #cowplot is an alternative to arrange plots.
    #There's a reason I ended up using grid.arrange() instead of cowplot but I can't recall

    if (nplots>1)
    {
      #number of columns needs to be stored in 1st list element
      resultplot <- gridExtra::grid.arrange(grobs = allplots, ncol = res[[1]]$ncols)
      #resultplot <- gridExtra::arrangeGrob(grobs = allplots, ncol = res[[1]]$ncols)
      #cowplot::plot_grid(plotlist = allplots, ncol = res[[1]]$ncol)

    }
    if (nplots==1)
    {
      resultplot <- pfinal
    }
    return(resultplot)
}
