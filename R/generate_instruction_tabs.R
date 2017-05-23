#' @title A helper function for the UI part of the shiny apps
#'
#' @description This function generates the tabs with content for each Shiny app. This is a helper function.
#' @return tablist a list of tabs for display in a shiny UI
#' @details This function is called by the shiny UIs to populate the documentation and information tabs
#' @author Andreas Handel
#' @export


############################################################
##generate the tabs with the instruction and other content for each shiny app
##written by Andreas Handel, ahandel@uga.edu, last change: 10/17/2016
############################################################

generate_instruction_tabs <- function()
{

    #This outputs the instructions/information which are saved in html files in the www subfolder
    #get number of files that need to be turned into tabs
    filesfortabs = list.files(path='./www/')
    #panel that includes all tabs
    tablist = NULL
    for (n in 3 : length(filesfortabs)) #1st and 2nd file are footer and header
    {
        tabfilename = paste('./www/',filesfortabs[n],sep='') #get file name
        htmlcontent = shiny::includeHTML(tabfilename) #read in as html

        txtcontent = readLines(tabfilename) #read in as text to pull out title
        mypattern = '<h2>([^<]*)</h2>' #next 3 lines pull out title of tab inside h2 tag
        matches = grep(mypattern, txtcontent, value=TRUE)
        tabtitle = gsub(mypattern,'\\1',matches)
        tabtitle2 = gsub("&#13;", "", tabtitle) #this weird character remains for some reason, need to remove by hand

        tablist[[n-2]] = shiny::tabPanel(tabtitle2, htmlcontent, icon = NULL) #save title and content as shiny tabPanel
    }
  return(tablist)
}
