#' Sélectionne les périodes commune à 2 capteurs
#'
#' A partir de choix de capteur, de sens et d'une heure, isoler les
#' donnees sur des dates communes pour deux capteurs (car+heavy)
#' Nécessaire pour la plus part des fonction de comparaisons de 2 capteurs
#'
#' @param donnees tableau issu de l'import de données Telraam
#' @param segments vecteur des 2 capteurs considérés
#' @param heure l'heure choisie pour l'analyse
#' @param dirA Sens choisi pour le capteur 1 ("Rgt"/"Lft"/"Toute")
#' @param dirB Sens choisi pour le capteur 2 ("Rgt"/"Lft"/"Toute")
#'
#' @return  Un tableau à 3 colonnes
#'  - "date" : date des mesures
#'  - non du premier capteur : VL + PL dans la direction choisie
#'  correspondant aux dates communes (pour le premier capteur)
#'   - non du second capteur : VL + PL dans la direction choisie
#'  correspondant aux dates communes (pour le second capteur)
#' @export
#'
#' @importFrom lubridate hour
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join filter bind_cols
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
#' tableau_horaire(Donnees,c("Burel","RteVitré"),9,"Lft","Lft")
#'
#' }
#'
donnees_horaire=function(donnees,segments,heure,dirA,dirB){
  # Sélection des capteurs et de l'heure
  Seg1 <- donnees %>% filter(segment_id==segments[1],
                             hour(date)==heure)
  Seg2 <- donnees %>% filter(segment_id==segments[2],
                             hour(date)==heure)
  # Jointure des donneesx de chacun des donneess par raaport à la date
  # (les lignes conservées correspondent à des dates communes)
  tabjoin <- inner_join(Seg1,Seg2,by="date",suffix=c("1","2"))

  # Sélection du sens de circulation des capteurs
  if(dirA=="Toute"){
    totA <- tabjoin$car1 + tabjoin$heavy1
  }
  if(dirA=="Rgt"){
    totA <- tabjoin$car_rgt1 + tabjoin$heavy_rgt1
  }
  if(dirA=="Lft"){
    totA <- tabjoin$car_lft1 + tabjoin$heavy_lft1
  }
  if(dirB=="Toute"){
    totB <- tabjoin$car2 + tabjoin$heavy2
  }
  if(dirB=="Rgt"){
    totB <- tabjoin$car_rgt2 + tabjoin$heavy_rgt2
  }
  if(dirB=="Lft"){
    totB <- tabjoin$car_lft2 + tabjoin$heavy_lft2
  }

  # Filtrage sur la présence de date commune (rend NULL si pas de données)
  if(length(tabjoin$date)==0){
    return(NULL)
  }else{
    res <- bind_cols(tabjoin$date,totA,totB)
    colnames(res) <- c("date",segments)
    return(res)
  }
}
