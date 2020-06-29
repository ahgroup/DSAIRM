#helper function that writes task text for each app into the Rmd/html file
write_tasktext <- function(alltasks,allrecord)
{

  # Make a single file using join_all
  alltext<-dplyr::full_join(alltasks,allrecord, by="TaskID")

  ntasks = max(alltasks$TaskID)

  for (n in 1:ntasks)
  {

    rtext<-dplyr::filter(alltext, TaskID==n)

    writeLines(c(paste('### Task ',n), '\n', paste(unique(rtext["TaskText"])), '\n',  '**Record**', '\n'))

    for (k in 1:nrow(rtext)){
      writeLines(c(paste("*",rtext[k,"Record"]), '\n'))
    }

  }
}
