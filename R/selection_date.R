#' Filtrage sur l'appartenance à une liste de périodes d'un dataframe
#'
#' @param donnees un dataframe avec une colonne "date" (format lubridate)
#' @param liste_interval une liste d'intervals lubridate
#'
#' @return Le data frame des lignes correspondants aux intervals et un dataframe complementaire
#' sous la forme d'une liste a 2 elements: donnees_correspondantes et donnees_complementaires
#' @export
#'
selection_date <- function(donnees, liste_interval){
  dates_correspondantes = unlist(lapply(donnees$date, FUN = function(x){test_date(x,liste_interval)}))
  donnees_correspondantes = donnees[dates_correspondantes,]
  donnees_complementaires = donnees[!dates_correspondantes,]
  return(list(donnees_correspondantes = donnees_correspondantes,
              donnees_complementaires = donnees_complementaires))
}
