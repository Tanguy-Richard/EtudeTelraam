##########################
# Courbe non saisonalise #
##########################
# Object : Visualisation des données de deux capteurs sur la période commune (non normalisé)
# Entree : donnees d'import Telraam, le noms des capteurs, leur sens, l'heure et le jour
# Sortie : Une liste composé du graph et du coefficient de corrélation

#' Title
#'
#' @param donnees donnees issues d'un import de données Telraam
#' @param segment Premier capteur
#' @param sens Choix du sens du premier capteur ("Rgt"/"Lft"/"Toute")
#' @param segment2 Second capteur
#' @param sens2 Choix du sens du second capteur ("Rgt"/"Lft"/"Toute")
#' @param heure heure choisie pour l'analyse
#' @param jour jour de la semaine, entre 1 et 7 (lundi à dimanche)
#'
#' @return Une liste composé du graph et du coefficient de corrélation
#' @export
#'
#' @importFrom lubridate wday
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom ggplot2 ggplot aes geom_line
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#'
#' liste_capteurs <- c("9000002156", "9000001844")
#'
#' liste_noms <- c("Burel","RteVitré")
#'
#' data("clef")
#'
#' Donnees <- import(liste_capteurs, liste_noms, clef)$donnee
#'
#' graph <- creation_courbe(Donnees,"Burel","Rgt","RteVitré","Rgt",9,5)$graph
#' graph
#'
#' creation_courbe(Donnees,"Burel","Rgt","RteVitré","Rgt",9,5)$correlation
#'
#'}
#'
creation_courbe=function(donnees , segment , sens, segment2, sens2 ,heure,jour){
  # Import des vacances
  Vacances <- vacances()
  #Import des jours fériés
  JF <- jours_feries()
  # Filtre pour enlever les vacances
  donnees_temp <- selection_date(donnees,Vacances$interval)$data2
  # Filtre pour enlever les jours fériés
  donnees_temp <- selection_date2(donnees,JF)$data2
  # Filtrage pour garder les dates communes
  tab_temp <- donnees_horaire(donnees_temp,c(segment,segment2),heure,sens,sens2)
  # Filtrage sur le jour de la semaine (et pour ne garder que les colonnes nous concernant)
  segA_trait <- tab_temp[wday(tab_temp$date)==jour,c("date",segment)]
  segB_trait <- tab_temp[wday(tab_temp$date)==jour,segment2]
  # création du donnees pour le graphique (normalisation)
  trait1 <- bind_cols(segA_trait[,"date"],
                      scale(segA_trait[,segment]),
                      rep(segment,length(segA_trait$date)))
  colnames(trait1) <- c("date","valeur","segment")
  trait2 <- bind_cols(segA_trait[,"date"],
                      scale(segB_trait),
                      rep(segment2,length(segA_trait$date)))
  colnames(trait2) <- c("date","valeur","segment")
  trait <- bind_rows(trait1,trait2)
  # Graphique
  graph <- ggplot(trait)+aes(x = date , y = valeur, color =segment)+geom_line()

  # retour la liste de résultats
  return(list(graph = graph, correlation = cor(segA_trait[,segment],segB_trait),use = "na.or.complete"))
}
