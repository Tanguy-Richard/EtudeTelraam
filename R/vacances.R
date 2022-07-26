#' Import des vacances de l'académie de Rennes
#'
#' @return un data frame à 4 colonnes: description (nom des vacances), start_date (date de début au format lubridate)
#' end_date (idem mais pour la date de fin) et interval (période de vacances au format interval de lubridate)
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd_hms interval
#' @importFrom dplyr select mutate
#'
#'
#' @examples
vacances <- function(){
  url = "https://data.education.gouv.fr/api/v2/catalog/datasets/fr-en-calendrier-scolaire/exports/json"
  Vacances <- GET(
    url = url,
    query = list(refine = "location:Rennes",
                 exclude = "population:Enseignants")
  ) %>%
    .$content %>%
    rawToChar() %>%
    fromJSON() %>%
    select(description,
           start_date,
           end_date) %>%
    mutate(
      start_date = ymd_hms(start_date),
      end_date = ymd_hms(end_date),
      interval = interval(start_date, end_date)
    )
  return(Vacances)
}
