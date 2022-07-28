#' Données des capteurs telraam de chateaubourg
#'
#' Import des données au 28 juillet 2022
#' Données avec au moins une mesure non nulle et un uptime
#' strictement supérieur à 0.5
#'
#' @docType data
#'
#' @usage data(chateaubourg)
#'
#' @format Un dataframe issu d'un import de données terlraam à 34609 lignes et 18 variables:
#' segment_id : nom des capteurs
#' date : jour et heure de la mesure
#' uptime : uptime de la mesure (part de l'heure ayant servi à la mesure)
#' heavy / heavy_rgt / heavy_lft : nombre de poids lourds totaux / selon un sens de circulation
#' (rgt correspond à B vers A sur le site de Telraam / lft correspond à A vers B)
#' car / car_rgt / car_lft : idem pour les véhicules légers
#' bike / bike_rgt / bike_lft : idem pour les vélos
#' pedestrian / pedestrian_rgt / pedestrian_lft : idem pour les piétons
#' car_speed_hist_0to70plus : vecteur de la part d'usagers allant respectivement entre 0 et 10km/h,
#' 10 et 20km/h, ... , 60 et 70 km/h et plus de 70km/h
#' car_speed_hist_0to120plus :  idem mais jusqu'à 120km/h
#' v85: v85 (vitesse supérieure à celle de 85% des véhicules)
#'
#' @keywords datasets
#'
#' @source \href{https://telraam.net}{Telraam}
#'
#' @examples
#' \dontrun{
#' data(chateaubourg)
#' }
"chateaubourg"
