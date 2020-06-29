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
