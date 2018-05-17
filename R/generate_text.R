#' @title A helper function that takes result from the simulators and produces text output
#'
#' @description This function generates text to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param res a list containing all simulation results and other information
#'    if the list entry 'maketext'  exists for a given plot (list element in allres)
#'    the data needs to follow the structure described in the generate_plots function
#'    if 'maketext' is false, this function only uses - if present' the entries 'showtext'
#'    for each plot and finaltext of the 1st list elemnt for an overall message/text
#'    see the generate_plots function for more details on the structure of the list
#' @return HTML formatted text for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the shiny UI
#' @author Andreas Handel
#' @export

generate_text <- function(res)
{

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
      allvarnames = levels(dat$varnames)
      nvars = length(allvarnames)
      plottype = res[[vn]]$plottype
      xlabel =  res[[vn]]$xlab
      ylabel =  res[[vn]]$ylab

      txt = '' #no text to start out

      if (res[[vn]]$maketext == TRUE) #if the plot wants text display based on result processing, do the stuff below
      {
        #for each plot, process each variable by looping over them
        for (nn in  1:nvars)
        {
          #data for a given variable
          currentvar = allvarnames[[nn]]
          vardat = dplyr::filter(dat, varnames == currentvar)

          if (plottype == 'Lineplot') #for lineplots, we show the min/max/final for each variable
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
            maxvals = format(resmax/nreps, digits =2, nsmall = 2) #mean across simulations (for stochastic models)
            minvals = format(resmin/nreps, digits =2, nsmall = 2) #mean across simulations (for stochastic models)
            numfinal = format(resfinal/nreps, digits =2, nsmall = 2) #mean for each variable
            newtxt <- paste('Minimum / Maximum / Final value of ',currentvar,': ',minvals,' / ', maxvals,' / ',numfinal,sep='')
          } #finish creating text outpot for lineplot/time-series

          if (plottype == 'Scatterplot' )
          {
            rcc = stats::cor.test(vardat[,1],y=vardat[,2], alternative = c("two.sided"), method = c("spearman"))
            newtxt = paste('Rank Cor. Coef. between',xlabel,' and ',ylabel,' is:',format(rcc$estimate, digits =3, nsmall = 2))
          }

          if (plottype == 'Boxplot' )
          {
            mymin = format(min(vardat$yvals), digits =2, nsmall = 2)
            mymean = format(mean(vardat$yvals), digits =2, nsmall = 2)
            mymedian = format(median(vardat$yvals), digits =2, nsmall = 2)
            mymax = format(max(vardat$yvals), digits =2, nsmall = 2)
            newtxt = paste('Min/Mean/Median/Max for ',xlabel,' / ',ylabel,': ',mymin,' / ',mymean,' / ', mymedian,' / ',mymax,' / ')
          }
          if (plottype == 'Mixedplot' )
          {
            newtxt = ""
          }

          txt <- paste(txt, newtxt, sep = "<br/>")
          #browser()
        } #end loop over all variables for a given plot

      } #ends maketext block which is only entered if TRUE


      #if the result structure has a text entry for a given plot, use that in addition to the
      if (!is.null(res[[vn]]$showtext))
      {
        txt = paste(txt, res[[vn]]$showtext, sep = "<br/>" )
      }

      alltext <- paste(alltext, txt, sep = " " ) #add text blocks together

    } #finishes loop over all plots

    #as requested by app, add additional final text at bottom
    if (!is.null(res[[1]]$finaltext))
    {
      finaltext <- res[[1]]$finaltext
      alltext <- paste(alltext, finaltext, sep = "<hr/>")
    }
    HTML(alltext)
} #end function
