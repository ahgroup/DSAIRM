#helper function that sets up task tables to be filled
make_tasktable <- function(ntasks,nrecord,appsettings)
{
  alltasks = data.frame(quizID=rep(paste0("dsaide_",appsettings$shorttitle),ntasks),
                        AppTitle = appsettings$apptitle,
                        AppID = appsettings$appid,
                        TaskID = 1:ntasks,
                        TaskText =  rep("",ntasks))

  allrecord = data.frame(TaskID = 1:nrecord,
                         RecordID = 1:nrecord,
                         Record = rep("",nrecord),
                         Type = rep("", nrecord) ,
                         Note = rep("",nrecord),
                         Answer = rep("",nrecord),
                         Fuzzy = rep(0,nrecord),
                         Review = rep(0,nrecord))
  tablist = list()
  tablist[[1]] = alltasks
  tablist[[2]] = allrecord
  return(tablist)

}
