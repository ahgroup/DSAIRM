#writes task table for an app into the current directory
save_tasktable <- function(alltasks,allrecord,appsettings)
{
  # Make a single file using join_all
  alltext<-dplyr::full_join(alltasks,allrecord, by="TaskID")

  # Make a unique ID for each of the items recorded for the tasks
  alltext<-dplyr::mutate(alltext, RecordID = paste("T",TaskID,"R",RecordID, sep = ""))

  write.table(alltext, paste0(appsettings$appid,"_tasktable.tsv"), append = FALSE, sep = "\t", row.names = F, col.names = TRUE)

}
