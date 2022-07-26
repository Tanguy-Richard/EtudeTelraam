#' Découper une période en "tranches" de 3 mois
#'
#' @param date1 Caractère. Date de début au format "aaaa-mm-jj hh:mm:ss"
#' @param date2 Caractère. Date de fin au format "aaaa-mm-jj hh:mm:ss"
#'
#' @return Dataframe avec une ligne par période de 3 mois et deux colonnes "debut" et "fin"
#' @export
#'
#' @importFrom lubridate ymd_hms
#'
#' @examples
#' \dontrun{
#' date1 <- "2021-03-01 12:35:21"
#' date2 <- "2022-07-01 03:15:33"
#'
#' test_df <- decouper_periode(date1, date2)
#' }
#'
decouper_periode <- function(date1, date2){

  debut <- seq(from = ymd_hms(date1),
               to = ymd_hms(date2),
               by = "3 month")

  fin <- debut + months(3)

  return(data.frame(debut,
             fin))

}
