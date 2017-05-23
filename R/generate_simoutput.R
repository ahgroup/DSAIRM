#' @title A helper function that takes result from the simulators and produces plots and text output
#'
#' @description This function generates plots and text to be displayed in the Shiny UI. 
#' This is a helper function. This function processes multiple simulation runs, supplied as a list
#' @param input the shiny app input structure
#' @param output the shiny app output structure
#' @param allres multiple runs of the simulation, supplied as a list. each entry in the list is expected to be a matrix 
#' @param varlist an optional list of vectors containing the names of variables 
#'    that should be plotted and processed independently. if not supplied, all variables will be shown in one plot.
#'    Also, final results for all variables will either be computed according to the groups of variable/compartment 
#'    names provided in varlist or if not provided, all variables will be used. 
#' @return output a list with plot, text and warn elements for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the shiny UI
#' @author Andreas Handel
#' @export


generate_simoutput <- function(input,output,allres,varlist = NULL)
{
  
  #check if user provided list of variables to be processed separately
  nplots = 1; 
  if (!is.null(varlist))
  {
    nplots = length(varlist)
  }
  
      
  # Here, we use the result returned from the ode solver to produce the plot
  # the resulting plot is saved in the "plot" placeholder of the output variable
  output$plot <- renderPlot({
    input$submitBtn
    
    tmax = isolate(input$tmax)

    #make potentially more than one plot
    graphics::par(mfrow=c(nplots,1))
    
    #loop over variable sets for which to produce plots
    for (vn in 1:nplots)
    {
      #for multiple plots, names of variables to be plotted as passed in by varlist, otherwise naems are just all column names (minus time) 
      #for some reason only the <- operator works, not the = operator
      ifelse(nplots>1, varnames <- unlist(varlist[vn]), varnames <- colnames(allres()[[1]])[-1] )

      #if the app doesn't have an nreps setting, assign repetition = 1, otherwise use nreps setting
      nreps = ifelse(is.null(isolate(input$nreps)),1,isolate(input$nreps)) 
      
      #process first simulation to build plot
      res = allres()[[1]]      
      ymax = max(res[,-1])
      tvec = res[,1]

      #browser()
      
      mycols=c("blue",'orange','red','green','black','magenta','cyan')
      #plot the 1st line
      graphics::plot(tvec,res[,varnames[1]],type="l",xlab="time",ylab="",col=mycols[1],lwd=1,log="",xlim=c(0,tmax),ylim=c(0,ymax),main="Time Series")
      
      if (length(varnames)>1) #plot additional lines if there is more than 1 variable to be plotted
      {
        for (nn in 2:length(varnames))
        {
          graphics::lines(tvec,res[,varnames[nn]],type="l",col=mycols[nn],lwd=1,lty=1)
        }
      }
      graphics::legend("right", varnames,col = mycols,lty=c(1),lwd=2)
      
      
      #loop over each additional simulation
      if (nreps>1)
      {
        #results are added to plot
        for (n1 in 2:nreps)
        {
          res = allres()[[n1]]      
          tvec = res[,1]
          graphics::lines(tvec,res[,varnames[1]],type="l",col=mycols[1],lwd=1,lty=1) #first variable for each new simulation
          if (length(varnames)>1) #plot additional lines if there is more than 1 variable to be plotted
          {
            for (nn in 2:length(varnames))
            {
              graphics::lines(tvec,res[,varnames[nn]],type="l",col=mycols[nn],lwd=1,lty=1)
            }
          } #done adding additional variables
        } #done additing lines from additional runs
      } #end loop over addition simulation replications
   
    } #end loop over individual plots
     
    } #finish render-plot statement
    , width = 'auto', height = 'auto'
  ) #end plot
  
  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({

    
    #if the app doesn't have an nreps setting, assign repetition = 1, otherwise use nreps setting
    nreps = ifelse(is.null(isolate(input$nreps)),1,isolate(input$nreps)) 
    
    #process sets of variables independently
    alltext <- ""
    
    for (vn in 1:nplots)
    {    
      #for multiple plots, names of variables to be plotted as passed in by varlist, otherwise names are just all column names (minus time) 
      ifelse(nplots>1, varnames <- unlist(varlist[vn]), varnames <- colnames(allres()[[1]])[-1] )
      
      resfinal = rep(0,length(varnames)) 
      resfracfinal = rep(0,length(varnames)) 
      for (n1 in 1:nreps) #add all final values
      {
        currentsim = allres()[[n1]]
        nrows = nrow(currentsim) #number of entries in time-series matrix - can be different for every run
        currfinal = currentsim[nrows,varnames] #final number for each variable of interest
        resfinal = resfinal + currfinal #total numbers
        resfracfinal = resfracfinal + currfinal / sum(currfinal) #add up fractions
      }  
      resfinal = resfinal/nreps #mean for each variable, take out time
      resfracfinal = resfracfinal/nreps #mean for each variable, take out time
      
      txt <- ""
      for (nn in 1:length(varnames))
      {
        numfinal = round(resfinal[nn], 2);
        fracfinal = round(resfracfinal[nn], 2)
        newtxt <- paste('Number and Fraction of ',varnames[nn],' at end of simulation: ',numfinal,' and ',fracfinal,sep='')
        txt <- paste(txt, newtxt, sep = "<br/>")
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
