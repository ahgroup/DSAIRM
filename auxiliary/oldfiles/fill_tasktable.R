

# This function fills in what to record
# Prevents records being stored in the wrong index

fill_tasktable <- function(allrecords,tid,rc,nrec,reclist)
{
  for (n in 1:nrec)
  {
    allrecord[rc,"TaskID"] = tid
    allrecord[rc,"RecordID"] = n
    allrecord[rc,"Record"] = reclist$rectext[n]
    allrecord[rc,"Type"] = reclist$rectype[n]
    allrecord[rc,"Note"] = reclist$recnote[n]
    allrecord[rc,"Fuzzy"] = reclist$recfuzzy[n]
    rc = rc+1
  }
  return(allrecord)
}
