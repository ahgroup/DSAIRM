#' @title A helper function for the UI part of the shiny apps
#'
#' @description This function take the documentation provided as html file
#' it then takes the html file and extracts sections
#' to generate the tabs with content for each Shiny app.
#' This is a helper function and only useful for this package.
#' @return tablist a list of tabs for display in a shiny UI
#' @details This function is called by the shiny UIs to populate the documentation and information tabs
#' @author Andreas Handel
#' @export

generate_documentation <- function()
{
    #take HTML file and split it into components for each tab
    currentdir = getwd()
    tablist = NULL
    tabtitles = c('Overview','The Model','What to do','Further Information')

    htmlfile = list.files(path = currentdir, pattern = "\\.html$")
    #htmldoc = xml2::read_html(htmlfile) #this part works using xml2, but then I can't further process it
    #shinyblocks = xml2::xml_find_all(htmldoc,xpath = "//div[@id[starts-with(., 'shinytab')]]")

    html.raw <- XML::htmlTreeParse(htmlfile, useInternalNodes = TRUE)
    shinyblocks = XML::getNodeSet(html.raw, "//div[@id[starts-with(., 'shinytab')]]")
    for (i in 1:4)
    {
      subDoc <- XML::xmlDoc(shinyblocks[[i]])
      content <- XML::xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]", XML::saveXML)
      htmlcontent = shiny::HTML(content[[1]])
      tablist[[i]] = shiny::tabPanel(tabtitles[i], htmlcontent, icon = NULL)
    }
    return(tablist)
}


