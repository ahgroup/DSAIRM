#' Influenza virus load data
#'
#' Daily average virus load of volunteers infected with influenza.
#'
#' Data is from Hayden et al 1996 JAMA:
#' doi:10.1001/jama.1996.03530280047035
#'
#' Specifically, data was extracted from Figure 2.
#' See this article and citations therein for more details on the data.
#'
#' @docType data
#'
#' @usage data(hayden96flu)
#'
#' @format A data frame with these variables:
#' \describe{
#' \item{HoursPI}{Hours post infection - measurements were taken daily.}
#' \item{txtime}{Hours post infection when treatment started. The value of 29 is the average of the 2 reported early treatment times. A value of 200, which is later than the last recorded virus load, means no treatment.}
#' \item{LogVirusLoad}{Average virus load for volunteers in a given treatment condition, in log10 units.}
#' \item{LOD}{Limit of detection for virus load, in log10 units.}
#' }
#'
"hayden96flu"


#' Streptococcus pneumoniae infection data
#'
#' Time series data from Streptococcus pneumoniae infection in mice
#'
#'
#' Data is from Schirm et al 2020 PLoS One:
#' https://doi.org/10.1371/journal.pone.0243147
#'
#' Specifically, data used here are time-series of bacteria and neutrophils
#' in BAL, as shown in panels a) and c) of Figure 4.
#' The original data uses P for the bacteria, and N for neutrophils.
#' For consistency with other DSAIRM models,
#' we use B for bacteria and I for neutrophils (immune response).
#' See the original article and citations therein for more details on the data.
#'
#' @docType data
#'
#' @usage data(schirm20strep)
#'
#' @format A data frame with these variables:
#' \describe{
#' \item{time}{Hours post infection.}
#' \item{value}{Amount of bacteria and neutrophils. See original paper for units.}
#' \item{quantity}{B = Streptococcus pneumoniae bacteria, I = neutrophils.}
#' }
#'
"schirm20strep"


