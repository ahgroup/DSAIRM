#' @title A helper function that takes result from the simulators and produces plots
#'
#' @description This function generates plots to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param input the shiny app input structure, mainly as a pass-through
#' @param output the shiny app output structure, mainly as a pass-through
#' @param allres a list containing all simulation results that are to be plotted
#'    the length of the list indicates the number of separate plots to make
#'    each list entry corresponds to one plot
#'    each list entry needs to contain the following information
#'    1. a data frame called dat with one column called xvals, one column yvals,
#'    one column called varnames that contains names for different variables
#'    and one column called IDvar for further grouping (i.e. multiple lines for stochastic simulations)
#'    supplying IDvar is optional, if it is missing, this grouping is ignored
#'    if plottype is 'mixedplot' and additional column called 'style' indicating line or point is needed
#'    2. meta-data for the plot, provided in the following variables:
#'    plottype - one of "Lineplot","Scatterplot","Boxplot", "Mixedplot" - required
#'    xlab, ylab - strings to label axes - required
#'    xscale, yscale - scaling of axes, valid ggplot2 expression, e.g. "identity" or "log10" - required
#'    xmin, xmax, ymin, ymax - min and max for axes - optional, if not provided will be set automatically
#'    legendtitle - legend title - if not suppied a legend will not be plotted
#'    linesize - width of line, numeric, i.e. 1.5, 2, etc. - optional, set to 1.5 if not supplied
#'
#' @return output a list with plots for display in a shiny UI
#' @details This function is called by the shiny server to produce plots returned to the shiny UI
#' @author Andreas Handel
#' @export


generate_plots <- function(input,output,allres)
{

  # this function produces all plots
  # the resulting plot is a ggplot object and saved in the "plot" placeholder of the output variable
  output$plot <- renderPlot({
    input$submitBtn

    res=isolate(allres()) #list of all results that are to be turned into plots

    #nplots contains the number of plots to be produced.
    nplots = length(res) #length of list

    allplots=list() #will hold all plots

    for (n in 1:nplots) #loop to create each plot
    {
      plottype = res[[n]]$plottype
      dat = res[[n]]$dat

      #set line size as given by app or to 1.5 by default
      linesize = ifelse(is.null(res[[n]]$linesize), 1.5, res[[n]]$linesize)

      #if the IDvar variable exists, use it for further stratification, otherwise just stratify on varnames
      if (is.null(dat$IDvar))
      {
        p1 = ggplot2::ggplot(dat, ggplot2::aes(x = xvals, y = yvals, color = varnames, linetype = varnames) )
      }
      if (!is.null(dat$IDvar))
      {
        p1 = ggplot2::ggplot(dat, ggplot2::aes(x = xvals, y = yvals, color = varnames, group = IDvar) )
      }

      if (plottype == 'Scatterplot')
      {
        p2 = p1 + ggplot2::geom_point()
        p3 = p2 + ggplot2::labs(x = res[[n]]$xlab, y = res[[n]]$ylab)
        p4 = p3 + ggplot2::scale_x_continuous(trans = res[[n]]$xscale, limits = c(res[[n]]$xmin,res[[n]]$xmax))
        p5 = p4 + ggplot2::scale_y_continuous(trans = res[[n]]$yscale, limits = c(res[[n]]$ymin,res[[n]]$ymax))
      }
      if (plottype == 'Lineplot')
      {
        p2 = p1 + ggplot2::geom_line(size = linesize)
        p3 = p2 + ggplot2::labs(x = res[[n]]$xlab, y = res[[n]]$ylab)
        p4 = p3 + ggplot2::scale_x_continuous(trans = res[[n]]$xscale, limits = c(res[[n]]$xmin,res[[n]]$xmax))
        p5 = p4 + ggplot2::scale_y_continuous(trans = res[[n]]$yscale, limits = c(res[[n]]$ymin,res[[n]]$ymax))
      }
      if (plottype == 'Boxplot')
      {
        p5 = p1 + ggplot2::geom_boxplot()
      }
      if (plottype == 'Mixedplot')
      {
        #a mix of lines and points. for this, the dataframe needs to contain an extra column indicating line or point
        p2 = p1 + ggplot2::geom_line(data = dplyr::filter(dat,style == 'line'), size = linesize)
        p3 = p2 + ggplot2::geom_point(data = dplyr::filter(dat,style == 'point'), size = linesize)
        p4 = p3 + ggplot2::labs(x = res[[n]]$xlab, y = res[[n]]$ylab)
        p4a = p4 + ggplot2::scale_x_continuous(trans = res[[n]]$xscale, limits = c(res[[n]]$xmin,res[[n]]$xmax))
        p5 = p4a + ggplot2::scale_y_continuous(trans = res[[n]]$yscale, limits = c(res[[n]]$ymin,res[[n]]$ymax))
      }

      if (is.null(res[[n]]$legend))
      {
        p6 = p5 + ggplot2::theme(legend.position="none")
      }
      else
      {
        p6 = p5 + theme(legend.key.width = unit(3,"line")) + scale_colour_discrete(name  = res[[n]]$legend) + scale_linetype_discrete(name = res[[n]]$legend)
      }

      #browser()

      pfinal = p6
      allplots[[n]] = pfinal

    } #end loop over individual plots

    #using gridExtra pacakge for multiple plots, ggplot for a single one
    #potential advantage is that for a single ggplot, one could use interactive features
    #such as klicking on point and displaying value
    #some code for that is in the basi virus app, but currently not working

    if (n>1)
    {
      #number of columns needs to be stored in 1st list element
      gridExtra::grid.arrange(grobs = allplots, ncol = res[[1]]$ncol)
      #cowplot::plot_grid(my_grobs, ncol = res[[1]]$ncol)
      #cowplot::plot_grid(plotlist = allplots, ncol = res[[1]]$ncol)

    }
    if (n==1)
    {
      plot(pfinal)
    }
  }, width = 'auto', height = 'auto'
  ) #finish render-plot statement
}
