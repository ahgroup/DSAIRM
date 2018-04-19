#' @title A helper function that takes result from the simulators and produces text output
#'
#' @description This function generates text to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param input the shiny app input structure
#' @param output the shiny app output structure
#' @param allres a list containing all simulation results.
#'    the length of the list indicates number of separate text blocks to make (one for each figure)
#'    for each list entry (e.g. allres[[i]]$type), 'type' specifies the kind of plot
#'    allres[[i]]$dat contains a dataframe in tidy/ggplot format for plotting. one column is called xvals,
#'    one column yvals, further columns are stratifiers/aesthetics,
#'    e.g. names of variables or number of run for a given variable
#'    the default is to return min, max and final value for each variable shown in a plot
#'    the default can be overwritten by manually supplying - from the shiny app - the output
#'    in that case, it is simply passed through and this function doesn't do much
#' @return output a list with  text for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the shiny UI
#' @author Andreas Handel
#' @export

generate_text <- function(input,output,allres)
{

  # Use the result returned from the simulator to compute some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderText({

    input$submitBtn

    res=isolate(allres())

    #nplots contains the number of plots to be produced.
    #for each plot, text output is produced separately
    nplots = length(res) #length of list

    alltext <- NULL #will hold all text outputs

    #each plot will be processed separately and text for each produced and placed in a list entry
    #using the same variable groupings as for the plots
    for (vn in 1:nplots)
    {
      #for each plot, get names of variables
      dat <- res[[vn]]$dat
      allvarnames = unique(dat$varnames)
      nvars = length(allvarnames)
      plottype = res[[vn]]$plottype
      xlabel =  res[[vn]]$xlab
      ylabel =  res[[vn]]$ylab


      #for each plot, process each variable by looping over them
      for (nn in  1:nvars)
      {
        #data for a given variable
        currentvar = allvarnames[[nn]]
        vardat = dplyr::filter(dat, varnames == currentvar)


        if (plottype == 'Lineplot')
        {
          #check if multiple runs are done
          #unless the data frame has a column indicating the number of runs, assume it's 1
          nreps = 1
          if ('nreps' %in% colnames(dat) ) {nreps=max(dat$nreps)}

          resmax = 0; resmin = 0; resfinal = 0;
          for (n1 in 1:nreps) #average over reps (if there are any)
          {
            #pull out each simulation/repetition
            currentsim = dplyr::filter(vardat, nreps == n1)
            nrows = nrow(currentsim) #number of entries in time-series matrix - can be different for every run

            resmax = resmax + max(currentsim$yvals)
            resmin = resmin + min(currentsim$yvals)
            resfinal = resfinal + currentsim$yvals[nrows]
          } #finish loop over reps

          #produce 2 types of text outcomes: for time-series/lineplots, report min/max/final of each plotted variable
          #for scatterplots, report correlation between x and every y-value

          #store values for each variable
          maxvals = round(resmax/nreps,2) #mean across simulations (for stochastic models)
          minvals = round(resmin/nreps,2) #mean across simulations (for stochastic models)
          numfinal = round(resfinal/nreps,2) #mean for each variable
          newtxt <- paste('Minimum / Maximum / Final value of ',currentvar,': ',minvals,' / ', maxvals,' / ',numfinal,sep='')
        } #finish creating text outpot for lineplot/time-series

        if (plottype == 'Scatterplot' )
        {
            rcc = stats::cor.test(vardat[,1],y=vardat[,2], alternative = c("two.sided"), method = c("spearman"))
            newtxt = paste('RCC between',xlabel,' and ',ylabel,' is:',as.character(round(rcc$estimate,3)))
        }

        if (plottype == 'Boxplot' )
        {
          newtxt = ""
        }


        if (nn == 1) {txt <- paste(newtxt)}
        if (nn > 1) {txt <- paste(txt, newtxt, sep = "<br/>")}
      } #end loop over all variables
    alltext <- paste(alltext, txt, sep = "<br/>" ) #add text blocks together
    } #finishes loop over sets of variables

    finaltxt <- '<hr> <i> For stochastic simulation scenarios, values shown are the mean over all simulations. </i>'
    resulttxt <- paste(alltext, finaltxt, sep = "")
    HTML(resulttxt)
  }) #end text output

}
