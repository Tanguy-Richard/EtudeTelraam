#' Test d'appartenance d'une date a une liste d'intervals lubridate
#'
#' @param date Date au format lubridate
#' @param liste_interval Liste d'intervals lubridate
#'
#' @return Un booléen indiquant si la date appartient ou pas à la liste d'interval
#' @export
#'
#' @importFrom lubridate ymd_hms interval %within%
#'
#' @examples
#' \dontrun{
#' date <- ymd_hms("2021/01/01 00:00:00")
#' interval1 <- interval("2020/11/02 00:00:00","2021/03/01 00:00:00")
#' interval2 <- interval("2022/02/01 00:00:00","2021/04/01 00:00:00")
#' Liste_interval = c(interval1, interval2)
#' test_date(date,Liste_interval)
#' }

test_date <- function(date, liste_interval){
  return(sum(date %within% liste_interval) > 0)
}
