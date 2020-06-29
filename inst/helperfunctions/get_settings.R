#helper function that returns app settings for each Rmd doc file

get_settings <- function(currentrmdfile, appdocdir, package)
{
  appName = gsub("_documentation.Rmd" ,"",currentrmdfile)
  #load table that has all the app information
  filename = system.file(appdocdir, "apptable.tsv", package = "DSAIDE")
  at = read.table(file = filename, sep = '\t', header = TRUE)
  appsettings = as.list(at[which(at$shorttitle == appName),])
  #a few apps have 2 simulator functions, combine here into vector
  if (length(appsettings$simfunction2) > 0) {appsettings$simfunction = c(appsettings$simfunction,appsettings$simfunction2)}

  return(appsettings)

}
