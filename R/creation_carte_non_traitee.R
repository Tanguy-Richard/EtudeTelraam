#' Visualisation des corrélations avec un capteur de référence sur une carte
#' sans traitement de saisonalité donc à heure et jour fixe
#'
#' @param donnees donnees issues d'un import de données Telraam
#' @param dfgeo infos geographiques issues d'un import de données Telraam
#' @param segment non du capteur de référence
#' @param sens sens du capteur de référence
#' @param liste_seg liste de capteur à comparer
#' @param liste_sens liste des sens correspondant
#' @param heure heure choisie pour l'analyse
#' @param jour jour choisi pour l'analyse (nombre entre 1 et 7)
#'
#' @return une carte ou les segments apparaisse sous la forme de point dont la taille est
#' proportionnelle à la valeur de la corrélation.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate wday
#' @importFrom dplyr filter
#' @importFrom stats cor
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' data(chateaubourg)
#' data(capteurs_chateaubourg)
#'
#' creation_carte_non_traitee(chateaubourg,capteurs_chateaubourg,"Burel","Rgt",
#'                            c("RteVitré","ParisArcEnCiel","RueGdDomaine","ParisMarché"),
#'                            c("Rgt","Lft","Rgt","Lft"),9,1)
#'
#' }
creation_carte_non_traitee=function(donnees, dfgeo , segment , sens, liste_seg, liste_sens ,heure,jour){
  # On récupère les coordonnées géographique du capteur de référence
  geometr <- dfgeo %>% filter(oidn==segment)
  #On prend les coordonnées moyennes
  capteur_lat <- mean(geometr$lat)
  capteur_lon <- mean(geometr$lon)
  # Pour stocker l'affichage dans la carte
  capteur_content <- "Capteur de reference"
  #On sélectionne les données hors varcances et jours fériés
  tableau_temp <- selection_date(donnees,vacances())$data2
  tableau_temp <- selection_date2(tableau_temp,jours_feries())$data2
  #On itère sur les capteurs
  Donnee_temp <- NULL
  for(i in 1:length(liste_seg)){
    #On conserve les dates communes
    tab_temp <- donnees_horaire(tableau_temp,c(segment,liste_seg[i]),heure,sens,liste_sens[i])
    # On ne prend pas en compte s'il n'y a pas assez de données
    n=length(tab_temp$date)
    if(is.null(tab_temp)|n<=35){

    }else{
      # On extrait la partie correspondant au jour de la semaine et à l'heure sélectionnée
      segA_trait <- tab_temp[wday(tab_temp$date)==jour,segment]
      segB_trait <- tab_temp[wday(tab_temp$date)==jour,liste_seg[i]]
      # On calcule la corrélation entre les deux signaux
      coef <- cor(segA_trait,segB_trait,use = "na.or.complete")
      # On récupère les coordonnées du capteur
      geometr <- dfgeo %>% filter(oidn==liste_seg[i])
      lat <- mean(geometr$lat)
      lon <- mean(geometr$lon)
      # On stocke le contenu à afficher sur la carte
      content <- paste(sep = "", "Coefficient de correlation : ", as.character(round(coef,3)))
      # On ajoute au tableau servant à afficher la carte
      Donnee_temp <- rbind(Donnee_temp,c(lat,lon,coef,content))
    }
  }
  # On finalise le tableau (type et nom de colonnes)
  Donnee_temp <- as_tibble(Donnee_temp,.name_repair = 'unique')
  colnames(Donnee_temp) <- c("lat","lon","coef","content")

  # On affiche la carte, la taille des capteur est proportionnelle à la valeur de la corrélation.
  leaflet(data = Donnee_temp) %>% addTiles() %>%
    addCircleMarkers(~as.double(lon),~as.double(lat), popup=~content,
                     radius = ~as.double(coef)*10,
                     fillOpacity = 0.8,color="red")%>%
    addCircleMarkers(capteur_lon,capteur_lat, popup=capteur_content,
                     radius = 8,
                     fillOpacity = 0.8,color="green")


}
