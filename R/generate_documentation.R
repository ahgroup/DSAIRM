#' @title A helper function for the UI part of the Shiny apps
#'
#' @description This function take the documentation provided as html file
#' and extracts sections
#' to generate the tabs with content for each Shiny app.
#' This is a helper function and only useful for this package.
#' @param appName name of app for which documentation should be created
#' @return tablist A list of tabs for display in a Shiny UI.
#' @details This function is called by the Shiny UIs to populate the documentation and information tabs.
#' @author Andreas Handel
#' @export

generate_documentation <- function(appName)
{
    #find location of HTML ducomentation file
    appdir = system.file("DSAIRMapps", package = "DSAIRM")
    htmlfile = paste0(appdir,'/',appName,'/',appName,'_Documentation.html')

    #take HTML file and split it into components for each tab
    tablist = NULL
    tabtitles = c('Overview','The Model','What to do','Further Information')
    html.raw <- XML::htmlTreeParse(htmlfile, useInternalNodes = TRUE, encoding='UTF-8')
    shinyblocks = XML::getNodeSet(html.raw, "//div[@id[starts-with(., 'shinytab')]]")
    for (i in 1:4)
    {
      subDoc <- XML::xmlDoc(shinyblocks[[i]])
      content <- XML::xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]", XML::saveXML, encoding='UTF-8')
      htmlcontent = shiny::HTML(content[[1]])
      tablist[[i]] = shiny::tabPanel(tabtitles[i], htmlcontent, icon = NULL)
    }
    #browser()
    return(tablist)
}


