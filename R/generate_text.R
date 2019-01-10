#' @title A helper function that takes result from the simulators and produces text output
#'
#' @description This function generates text to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list.
#' @param res A list structure containing all simulation results that are to be processed.
#'    This function is meant to be used together with generate_plots() and requires similar input information.
#'    See the generate_plots() function for most details.
#'    Specific entries for this function are 'maketext', 'showtext' and 'finaltext'.
#'    If 'maketext' is set to TRUE (or not provided) the function processes the data corresponding to each plot
#'    and reports min/max/final values (lineplots) or correlation coefficient (scatterplot)
#'    If 'maketext' is FALSE or missing, no text based on the data is generated.
#'    If the entries 'showtext' or 'finaltext' are present, their values
#'    will be returned for each plot or for all together.
#'    The overall message of finaltext should be in the 1st plot.
#' @return HTML formatted text for display in a Shiny UI.
#' @details This function is called by the Shiny server to produce output returned to the Shiny UI.
#' @author Andreas Handel
#' @importFrom stats median
#' @export

generate_text <- function(res)
{

    #nplots contains the number of plots to be produced.
    #for each plot, text output is produced separately
    nplots = length(res) #length of list

    alltext <- NULL #will hold all text outputs

    #each plot will be processed separately and text for each produced and placed in a list entry
    #using the same variable groupings as for the plots
    for (n in 1:nplots)
    {

      resnow = res[[n]]


      #if a data frame called 'ts' exists, assume that this one is the data to be plotted
      #otherwise use the data frame called 'dat'
      #one of the 2 must exist, otherwise the function will not work
      if (!is.null(resnow$ts))
      {
        rawdat = resnow$ts
      }
      else {
        rawdat = resnow$dat
      }

      #if nothing is provided, we assume a line plot. That could lead to silly text returns.
      plottype <- if(is.null(resnow$plottype)) {'Lineplot'} else  {resnow$plottype}

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
        dat = tidyr::gather(rawdat, -xvals, value = "yvals", key = "varnames")
      }

      #code variable names as factor and level them so they show up right
      #factor is needed for plotting and text
      mylevels = unique(dat$varnames)
      dat$varnames = factor(dat$varnames, levels = mylevels)

      allvarnames = levels(dat$varnames)
      nvars = length(allvarnames)

      #labels, only used in correlation plots
      xlabel =  resnow$xlab
      ylabel =  resnow$ylab

      #if missing or false, we won't create text based on data as described below
      if (is.null(resnow$maketext) || resnow$maketext == FALSE) {maketext = FALSE} else {maketext = TRUE}

      if (maketext == TRUE) #if the app wants text display based on result processing, do the stuff below
      {
        #for each plot, process each variable by looping over them
        for (nn in  1:nvars)
        {
          #data for a given variable
          currentvar = allvarnames[[nn]]
          vardat = dplyr::filter(dat, varnames == currentvar)
          #for lineplots, we show the min/max/final for each variable
          if (plottype == 'Lineplot')
          {
            #check if multiple runs are done
            #unless the data frame has a column indicating the number of runs, assume it's 1
            nreps = 1
            if ('nreps' %in% colnames(vardat) ) {nreps=max(vardat$nreps)}

            resmax = 0; resmin = 0; resfinal = 0;
            for (n1 in 1:nreps) #average over reps (if there are any)
            {

              #pull out each simulation/repetition
              currentsim = dplyr::filter(vardat, nreps == n1)
              nrows = nrow(currentsim) #number of entries in time-series matrix - can be different for every run

              resmax = resmax + max(currentsim$yvals)
              resmin = resmin + min(currentsim$yvals)
              resfinal = resfinal + currentsim$yvals[nrows]
              #browser()
            } #finish loop over reps


            #store values for each variable
            maxvals = format(resmax/nreps, digits =2, nsmall = 2) #mean across simulations (for stochastic models)
            minvals = format(resmin/nreps, digits =2, nsmall = 2) #mean across simulations (for stochastic models)
            numfinal = format(resfinal/nreps, digits =2, nsmall = 2) #mean for each variable
            newtxt <- paste('Minimum / Maximum / Final value of ',currentvar,': ',minvals,' / ', maxvals,' / ',numfinal,"<br/>",sep='')
          } #finish creating text outpot for lineplot/time-series

          #for scatterplots, report correlation between x and every y-value
          if (plottype == 'Scatterplot' )
          {
            rcc = stats::cor.test(vardat[,1],y=vardat[,2], alternative = c("two.sided"), method = c("spearman"))
            newtxt = paste('Rank Cor. Coef. between',xlabel,' and ',ylabel,' is:',format(rcc$estimate, digits = 2, nsmall = 2),"<br/>")
          }

          if (plottype == 'Boxplot' )
          {
            mymin = format(min(vardat$yvals), digits =2, nsmall = 2)
            mymean = format(mean(vardat$yvals), digits =2, nsmall = 2)
            mymedian = format(stats::median(vardat$yvals), digits =2, nsmall = 2)
            mymax = format(max(vardat$yvals), digits =2, nsmall = 2)
            newtxt = paste('Min/Mean/Median/Max for ',ylabel,': ',mymin,' / ',mymean,' / ', mymedian,' / ',mymax,"<br/>")
          }
          if (plottype == 'Mixedplot' )
          {
            newtxt = ""
          }
          alltext <- paste(alltext, newtxt)
          #browser()
        } #end loop over all variables for a given plot

      } #ends maketext block which is only entered if TRUE


      #if the result structure has a text entry for a given plot, use that in addition to the
      if (!is.null(resnow$showtext))
      {
        alltext = paste(alltext, resnow$showtext, "<br/>")
      }

    } #finishes loop over all plots


    #as requested by app, add additional final text at bottom
    if (!is.null(res[[1]]$finaltext))
    {
      finaltext <- res[[1]]$finaltext
      alltext <- paste(alltext, finaltext)
    }

    shiny::HTML(alltext)
} #end function
