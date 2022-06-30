

#
# context("test-DSAIRMsolutions.R")
#
#
# test_that("individual simulation results match DSAIRMsolutions",
#           {

            DSAIRMsolutionsdir <- "../DSAIRMsolutions"

            DSAIRMsolutionshelperfunctions <- list.files(path = file.path(DSAIRMsolutionsdir,
                                                                          "helperfunctions"),
                                                         pattern = "[.][r|R]$",
                                                         full.names = TRUE)

            source(DSAIRMsolutionshelperfunctions[which(grepl("record_answers", DSAIRMsolutionshelperfunctions))])



            DSAIRMsolutions <- list.files(path = file.path(DSAIRMsolutionsdir, "solutions_source"),
                                          pattern = "[.][r|R][m|M][d|D]$",
                                          full.names = TRUE)

            DSAIRMsolutionstasktables <- list.files(path = file.path(DSAIRMsolutionsdir,
                                                                     "tasktable_files"),
                                                    pattern = "_tasktable[.]tsv$",
                                                    full.names = TRUE)

            DSAIRMsolutionscompletesolutionsheets <- list.files(path = file.path(DSAIRMsolutionsdir,
                                                                                 "complete_solution_sheets"),
                                                                pattern = "_complete[.]xlsx$",
                                                                full.names = TRUE)

            library(dplyr)
            library(readxl)



            DSAIRMsolutions_df <- full_join(data.frame(app = sub("^.+/(.+)_.*[.].{3}$", "\\1", DSAIRMsolutions),
                                                       RMD = DSAIRMsolutions),
                                            data.frame(app = sub("^.+/(.+)_.*[.].{3}$", "\\1", DSAIRMsolutionstasktables),
                                                       tasktable = DSAIRMsolutionstasktables),
                                            by = "app")%>%
              full_join(.,
                        data.frame(app = sub("^.+/DSAIRM_(.+)_.*[.].{4}$", "\\1", DSAIRMsolutionscompletesolutionsheets),
                                   completesolutionsheets = DSAIRMsolutionscompletesolutionsheets),
                        by = "app")





            thecomparisons <- list()

            pb <- txtProgressBar(min = 0, max = nrow(DSAIRMsolutions_df)+1, style = 3)



            for(k in 1:nrow(DSAIRMsolutions_df)){

              setTxtProgressBar(pb, k)

              thesolutionrmd <- readLines(DSAIRMsolutions_df$RMD[k])
              # paste(, collapse = "\n")
              # cat(thesolutionrmd)

              tasktable <- read.delim(DSAIRMsolutions_df$tasktable[k], sep = "\t")

              solutionsheet <- read_xlsx(DSAIRMsolutions_df$completesolutionsheets[k])



              codechunkindexstart <- which(grepl("^```\\{[r|R].*\\}$", thesolutionrmd))
              codechunkindexstop <- which(grepl("^```$", thesolutionrmd))

              codechunks <- lapply(1:length(codechunkindexstart),
                                   function(ii){
                                     return(thesolutionrmd[{codechunkindexstart[ii]+1}:{codechunkindexstop[ii]-1}])
                                   })

              codechunks <- codechunks[c(-1,-2,-length(codechunks))]

              codechunksexpressions <- lapply(codechunks,
                                              function(thiscodechunk){
                                                parse(text = thiscodechunk)
                                              })


              pb2 <- txtProgressBar(min = 0, max = length(codechunksexpressions)+1, style = 3)

              sapply(codechunksexpressions,
                     function(thisexpression){
                       try(setTxtProgressBar(pb2, which(codechunksexpressions==thisexpression)))
                       try(eval(thisexpression, envir = parent.frame(n=3)))
                     })

              tasktable <- tasktable %>%
                select(Answertest = Answer, everything())
              # names(tasktable)[which(names(tasktable)%in%c("Answer"))] <- "Answertest"



              comparison_df <- full_join(tasktable,
                                         solutionsheet)%>%
                filter(Answertest!=""&!is.na(Answer))

              comparison <- comparison_df%>%
                summarise(n_agree = sum(Answertest==Answer))%>%
                select(n_agree)%>%
                unlist()

              theexpected <- c(n_agree=nrow(comparison_df))

              thecomparisons[[k]] <- list(theexpected=theexpected,
                                          comparison=comparison)
              # testthat::expect_equal(theexpected,  comparison)
            }

          # })

