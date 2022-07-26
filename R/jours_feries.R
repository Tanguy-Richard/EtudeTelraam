#' Import des jours fériés sur 25 ans
#'
#' @return un vecteur de date au format lubridate
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' data(chateaubourg)
#' jours <- jours_feries()
#'
#' donnees_filtrees <- selection_date(chateaubourg,vacance) %>%
#'                         .$donnees_correspondantes
#'
#' summary(donnees_filtrees)
#'
#' }
jours_feries <- function(){
  # Récupération des jours au format: "YYYY-MM-DD"
  jours <- "https://calendrier.api.gouv.fr/jours-feries/metropole.json" %>%
    GET() %>%
    .$content %>%
    rawToChar() %>%
    fromJSON() %>%
    names() %>%
    ymd() # Mise au format date de lubridate

  return(jours)
  }


