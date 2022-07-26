#' Filtrage sur l'appartenance à une liste de dates d'un dataframe
#'
#' @param donnees un dataframe avec une colonne "date" (format lubridate)
#' @param liste_date une liste de dates lubridate
#'
#' @importFrom lubridate date
#'
#' @return Le data frame des lignes correspondants aux dates et un dataframe complementaire
#' sous la forme d'une liste a 2 elements: donnees_correspondantes et donnees_complementaires
#' @export
#'
selection_date2=function(donnees,liste_date){
  dates_correspondantes = unlist(lapply(donnees$date, FUN = function(x){
    date(x) %in% liste_date}))
  donnees_correspondantes = donnees[dates_correspondantes,]
  donnees_complementaires = donnees[!dates_correspondantes,]
  return(list(donnees_correspondantes = donnees_correspondantes ,
              donnees_complementaires = donnees_complementaires))
}
