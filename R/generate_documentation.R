#' @title A helper function which processes and displays the documentation part for each app
#'
#' @description This function take the documentation provided as html file
#' and extracts sections
#' to generate the tabs with content for each Shiny app.
#' This is a helper function and only useful for this package.
#' @param docfilename full path and name to html file containing the documentation
#' @return tablist A list of tabs for display in a Shiny UI.
#' @details This function is called by the Shiny UIs to populate the documentation and information tabs.
#' @author Andreas Handel
#' @export

generate_documentation <- function(docfilename)
{
    #take HTML file and split it into components for each tab
    tablist = NULL
    tabtitles = c('Overview','The Model','What to do','Further Information')
    html.raw <- XML::htmlTreeParse(docfilename, useInternalNodes = TRUE, encoding='UTF-8')
    shinyblocks = XML::getNodeSet(html.raw, "//div[@id[starts-with(., 'shinytab')]]")
    for (i in 1:4)
    {
      subDoc <- XML::xmlDoc(shinyblocks[[i]])
      content <- XML::xpathApply(subDoc, "//div[@id[starts-with(., 'shinytab')]]", XML::saveXML, encoding='UTF-8')
      htmlcontent = shiny::HTML(content[[1]])
      #remove the main headings since they are shown on the tab titles
      pattern = "<h2>.+</h2>" #everything between the <h2> elements
      htmlcontent = gsub(pattern,"<br>",htmlcontent)
      taskhtml1 =  actionButton("detachtasks", "Float Task List", class="submitbutton")
      taskhtml2 =  actionButton("destroytasks", "Remove Task Float", class="submitbutton")
      if (tabtitles[i] == "What to do")
      {
        htmlcontent = tagList(taskhtml1, taskhtml2, htmlcontent)
      }
      tablist[[i]] = shiny::tabPanel(tabtitles[i], htmlcontent, icon = NULL)
    }
    #tablist[[i+1]] = shiny::tabPanel("Float Tasks", taskhtml, icon = NULL)
    #browser()
    return(tablist)
}


