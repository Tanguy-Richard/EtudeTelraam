#' Import des données Telraam
#'
#' @param liste_capteurs  vecteur des identifiants des capteurs
#' @param liste_noms  vecteur des noms à donner aux identifiants
#' @param clef  Votre clef d'accès à l'API
#' @param date_debut première date d'interet format YYYY-MM-DD (de base "2020-01-01")
#' @param date_fin dernière date d'interet format YYYY-MM-DD (de base Sys.Date())
#'
#' @return une liste de 2 objets: geomet donnant accès aux information géographique sur le capteur
#'  et donnee donnant accès au données horaire pour chaques capteurs
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom httr GET POST add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd_hms interval
#' @importFrom dplyr mutate filter
#'
#' @examples
#' \dontrun{
#' liste_capteurs <- c("9000002156", "9000001844")
#'
#' liste_noms <- c("Burel","RteVitré")
#'
#' data("clef")
#'
#' Donnee <- import(liste_capteurs, liste_noms, clef)$donnee
#' head(Donnee)
#'
#' Donnee_2 <- import(liste_capteurs, liste_noms, clef,date_debut = "2021-06-06",
#'                    date_fin = "2022-08-01")$donnee
#' head(Donnee_2)
#' }
import <- function(liste_capteurs, liste_noms, clef, date_debut = "2020-01-01", date_fin = Sys.Date()) {

  # initialisation sous la forme de dataframes vides
  # pour les donnees totales
  dfglob = data.frame()
  # pour les information geographiqe + donnees des dernieres 24h d'enregistrement
  dfgeo = data.frame()
  # iteration sur l'ensemble des capteurs renseignes
  for (segs in liste_capteurs) {
    #recuperation des donnees sur le capteurs: localisation et date d'emission
    df <-
      paste0("https://telraam-api.net/v1/segments/id/", segs) %>%
      GET(add_headers("X-Api-Key" = clef)) %>%
      .$content %>%
      rawToChar() %>%
      fromJSON() %>%
      .$features

    # Recuperation des coordonnees (geometry) des routes isolees
    lat = df$geometry$coordinates[[1]][, , 2]
    lon = df$geometry$coordinates[[1]][, , 1]
    id <- cbind(lat, lon, df$properties)
    # Propriete correspondant au mesure sur les dernieres 24h
    df <- df$properties
    # Recuperation des dates d'emission de la premiere et derniere donnee


    dateInf = df$first_data_package
    dateSup = df$last_data_package

    dateInf <- max(ymd_hms(dateInf),ymd_hms(paste(date_debut," 00:01:00")))
    dateSup <- min(ymd_hms(dateSup),ymd_hms(paste(date_fin," 23:00:00")))

    dfgeo = rbind(dfgeo, id)

    #recuperation des listes de dates separees par maximum 3 mois (pour iterer lors de l'import)
    dates = decouper_periode(dateInf, dateSup)

    #recuperation des donnees mesurees par le capteur
    for (i in 1:length(dates$debut)) {
      #recuperation des donnees
      resTraffic <-
        POST(
          "https://telraam-api.net/v1/reports/traffic",
          add_headers("X-Api-Key" = clef)
          ,
          body = paste0(
            '{
    "level": "segments",
    "format": "per-hour",
    "id": "',
            segs,
            '",
    "time_start": "',
            dates$debut[i],
            '",
    "time_end": "',
            dates$fin[i],
            '"
    }'
          )
        )
      # On rend le fichier exploitable par R
      dataTraffic <-  resTraffic %>%
        .$content %>%
        rawToChar() %>%
        fromJSON()
      df <- dataTraffic$report
      # On change la classe de date (caractère) en Date avec un decalage horaire de 2
      df$date <- ymd_hms(df$date, tz = df$timezone[1])
      dfglob = rbind(dfglob, df)
    }
    # Incrémentation de la barre de progression

  }

  #gestion du typage
  dfglob$segment_id <- dfglob$segment_id %>%
    as.character()
  dfgeo$lat <- dfgeo$lat %>%
    as.character() %>%
    as.numeric()
  dfgeo$lon <- dfgeo$lon %>%
    as.character() %>%
    as.numeric()

  #renomage des capteurs
  for (i in 1:length(liste_capteurs)) {
    dfglob <- dfglob %>%
      mutate(segment_id = replace(segment_id, segment_id == liste_capteurs[i], liste_noms[i]))
  }

  # Selection des donnees non nulles
  donnees_non_nulles <- dfglob %>% filter(
    uptime > 0.5,
    heavy_lft + car_lft + pedestrian_lft + bike_lft +
      heavy_rgt + car_rgt + pedestrian_rgt + bike_rgt >
      0
  )
  #Stockage des resultats

  #liste des deux objets
  return(list(geomet = dfgeo, donnee = donnees_non_nulles))

}
