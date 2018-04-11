#' @title A helper function that takes result from the simulators and produces plots
#'
#' @description This function generates plots to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param input the shiny app input structure
#' @param output the shiny app output structure
#' @param allres a list containing all simulation results.
#'    the length of the list indicates number of separate plots to make, each list entry is one plot and one set of text output
#'    for each list entry (e.g. allres[[i]]$type), 'type' specifies the kind of plot
#'    allres[[i]]$dat contains a dataframe in tidy/ggplot format for plotting. one column is called xvals, one column yvals, further columns are stratifiers/aesthetics, e.g. names of variables or number of run for a given variable
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

    res=isolate(allres()) #get results for processing

    #nplots contains the number of plots to be produced.
    nplots = length(res) #length of list

    allplots=vector("list",nplots) #will hold all plots

    #browser()

    for (n in 1:nplots) #loop to create each plot
    {
      plottype = res[[n]]$plottype
      dat = res[[n]]$dat
      legend = res[[n]]$legend
      xscale = res[[n]]$xscale
      yscale = res[[n]]$yscale
      p1 = ggplot2::ggplot(dat, ggplot2::aes(x = xvals, y = yvals, color = varnames) )


      #use limits for axes plotting if provided by shiny app
      xmin=res[[n]]$xmin;
      xmax=res[[n]]$xmax;
      ymin=res[[n]]$ymin;
      ymax=res[[n]]$ymax;


      if (plottype == 'Scatterplot') {p2 = p1 + ggplot2::geom_point() }
      if (plottype == 'Lineplot') {p2 = p1 + ggplot2::geom_line(size = 1.5)  }
      if (plottype == 'Boxplot') {p2 = p1 + ggplot2::geom_boxplot()}

      p2a = p2 + ggplot2::labs(x = res[[n]]$xlab, y = res[[n]]$ylab)

      p3 = p2a + ggplot2::scale_x_continuous(trans = xscale, limits = c(xmin,xmax)) + ggplot2::scale_y_continuous(trans = yscale, limits = c(ymin,ymax))



      if (legend == FALSE) { p4 = p3 + ggplot2::theme(legend.position="none") }
      else { p4 = p3 }


      pfinal = p4
      allplots[[n]] = pfinal



    } #end loop over individual plots

    #using cowplot for multiple plots, ggplot for a single one
    #potential advantage is that for a single ggplot, one could use interactive features
    #such as klicking on point and displaying value
    #some code for that is in the basi virus app, but currently not working

    if (n>1)
    {
      #number of columns needs to be stored in 1st list element
      cowplot::plot_grid(plotlist = allplots, ncol = res[[1]]$ncol)
    }
    if (n==1)
    {
      plot(pfinal)
    }
  }, width = 'auto', height = 'auto'
  ) #finish render-plot statement
}
