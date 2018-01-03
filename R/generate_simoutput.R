#' @title A helper function that takes result from the simulators and produces plots and text output
#'
#' @description This function generates plots and text to be displayed in the Shiny UI.
#' This is a helper function. This function processes results returned from the simulation, supplied as a list
#' @param input the shiny app input structure
#' @param output the shiny app output structure
#' @param allres a list containing all simulation results.
#'    the length of the list indicates number of separate outputs (plots and text) to make, each list entry is one plot and one set of text output
#'    for each list entry (e.g. allres[[i]]$type), 'type' specifies the kind of plot
#'    allres[[i]]$dat contains a dataframe in tidy/ggplot format for plotting. one column is called xvals, one column yvals, further columns are stratifiers/aesthetics, e.g. names of variables or number of run for a given variable
#' @return output a list with plot, text and warn elements for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the shiny UI
#' @author Andreas Handel
#' @export

generate_simoutput <- function(input,output,allres)
{


  # this function produces all plots
  # the resulting plot is a ggplot/ggpubr object and saved in the "plot" placeholder of the output variable
  output$plot <- renderPlot({
    input$submitBtn

    #nplots contains the number of plots to be produced.
    nplots = length(allres) #length of list

    allplots=list() #will hold all plots

    browser()

    for (n in 1:nplots) #loop to create each plot
    {
      plottype = allres[[n]]$type
      dat = allres[[n]]$dat
      allplots[n] = ggpubr::ggline(dat, aes(x=xvals, y=yvals, color=variables))
      #keep working here

    } #end loop over individual plots

    ggarrange(allplots)

    } #finish render-plot statement
    , width = 'auto', height = 'auto'
  ) #end the output$plot function which produces the plot

  # Use the result "res" returned from the simulator to compute some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({

    #number of repetitions for a given setting
    nreps = allres()$nreps

    #nplots contains the number of plots to be produced.
    #Buy default it's a single plot
    #some apps ask for more than 1 plot, this is then sent in as a list to this function
    #check if user provided a list of variables to be processed separately
    nplots = 1;
    varlist = allres()$varlist
    if (!is.null(varlist))
    {
      nplots = length(varlist)
    }


    #process sets of variables independently
    alltext <- ""

    #if multiple plots are requested, text output for variables will be processed
    #using the same variable groupings as for the plots
    for (vn in 1:nplots)
    {
      #for multiple plots, names of variables to be plotted as passed in by varlist, otherwise names are just all column names (minus time)
      ifelse(nplots>1, varnames <- unlist(varlist[vn]), varnames <- colnames(allres()[[1]])[-1] )

      resfinal = rep(0,length(varnames))
      resmax = rep(0,length(varnames))
      resmin = rep(0,length(varnames))
      resfracfinal = rep(0,length(varnames))
      for (n1 in 1:nreps) #add all final values
      {

        currentsim = allres()[[n1]]
        nrows = nrow(currentsim) #number of entries in time-series matrix - can be different for every run
        currfinal = currentsim[nrows,varnames] #final number for each variable of interest
        #min and max for each variable
        if (length(varnames)>1)
        {
          resmax = resmax + apply(currentsim[,varnames],2,max);
          resmin = resmin + apply(currentsim[,varnames],2,min);
        }
        if (length(varnames)==1) #for a single variable, we have a vector and the apply function does not work
        {
          resmax = resmax + max(currentsim[,varnames]);
          resmin = resmin + min(currentsim[,varnames]);
        }

        resfinal = resfinal + currfinal #total numbers
        resfracfinal = resfracfinal + currfinal / sum(currfinal) #add up fractions
      }
      resmax = resmax/nreps; #mean across simulations (for stochastic models)
      resmin = resmin/nreps; #mean across simulations (for stochastic models)
      resfinal = resfinal/nreps #mean for each variable
      resfracfinal = resfracfinal/nreps #mean for each variable


      for (nn in 1:length(varnames))
      {
        maxval = round(resmax[nn],2)
        minval = round(resmin[nn],2)
        numfinal = round(resfinal[nn], 2)
        fracfinal = round(resfracfinal[nn], 2)
        newtxt1 <- paste('Minimum and Maximum of ',varnames[nn],' during simulation: ',minval,' and ', maxval,sep='')
        newtxt2 <- paste('Number and Fraction of ',varnames[nn],' at end of simulation: ',numfinal,' and ',fracfinal,sep='')
        if (nn == 1) {txt <- paste(newtxt1, newtxt2, sep = "<br/>")}
        if (nn > 1) {txt <- paste(txt, newtxt1, newtxt2, sep = "<br/>")}
      }
    alltext <- paste(alltext, txt, sep = "<hr>" ) #add text blocks together

    } #finishes loop over sets of variables

    finaltxt <- '<hr> <i> For stochastic simulation scenarios, values shown are the mean over all simulations. </i>'
    resulttxt <- paste(alltext, finaltxt, sep = "")
    HTML(resulttxt)
  }) #end text output

  # At last, if we have any warnings or error from the simulator we can show them here
  # That text will be shown in red in the UI ("warn" placeholder will be used)
  output$warn <- renderUI({
    warntxt <- ""
    if(length(utils::data()$warns) == 0){

    }else{
      warntxt <- paste(warntxt, "Warnings:", sep = "<br/>")
      for (i in 1:length(utils::data()$warns)){
        warntxt <- paste(warntxt, utils::data()$warns[[i]], sep = "<br/>")
      }
    }
    HTML(warntxt)
  })
}
