#' création de carte permettant de visualiser la correlation
#'
#' @param donnees donnees issues d'un import de données Telraam
#' @param dfgeo infos geographiques issues d'un import de données Telraam
#' @param segment non du capteur de référence
#' @param heure heure choisie pour l'analyse
#'
#' @return une carte leaflet ou apparait le capteur de référence et les autres capteurs avec les correlations
#' compare le trafic total entre chaque capteur
#'
#' @importFrom magrittr %>%
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
#' creation_carte(chateaubourg,capteurs_chateaubourg,"Burel",9)
#'
#' }
creation_carte=function(donnees,dfgeo,segment,heure){
  # Récupération des capteurs disponible dans le tableau de données
  capteur <- as.vector(levels(as.factor(donnees$segment_id[donnees$segment_id!=segment])))
  # Récupérartion des données géographiques (capteur de ref)
  geometr <- dfgeo %>% filter(oidn==segment)
  # On prend la latitude et la longitude moyenne pour le capteur
  capteur_lat <- mean(geometr$lat)
  capteur_lon <- mean(geometr$lon)
  # Affichage de text epour le capteur de référence
  capteur_content <- "Capteur de reference"

  # Boucle sur les capteurs du tableau
  Donnee_temp <- NULL
  for(i in capteur){
    # Filtrage sur les dates communes
    tab_temp <- donnees_horaire(donnees,c(segment,i),heure,"Toute","Toute")
    # S'il il y a trop peu de dates commune on ne l'inclu pas
    n=length(tab_temp$date)
    if(is.null(tab_temp)|n<=35){
      capteur <- capteur[capteur!=i]
    }else{
      # Extraction du bruit
      segA_trait <- desaisonnalite(tab_temp,segment,"add")$bruit
      segB_trait <- desaisonnalite(tab_temp,i,"add")$bruit
      # Calcul de la correlation
      coef <- cor(segA_trait,segB_trait,use = "na.or.complete")
      # Récupération des données géographiques
      geometr <- dfgeo %>% filter(oidn==i)
      lat <- mean(geometr$lat)
      lon <- mean(geometr$lon)
      # Création de l'affichage pour le capteur
      content <- paste(sep = "", "Coefficient de correlation : ", as.character(round(coef,3)))
      # Ajout dans le tableau
      Donnee_temp <- rbind(Donnee_temp,c(lat,lon,coef,content))
    }
  }
  # Finalisation du tableau pour la carte
  Donnee_temp <- as_tibble(Donnee_temp)
  colnames(Donnee_temp) <- c("lat","lon","coef","content")

  # Carte (taille des capteurs propotionnelle à la corrélation )
  leaflet(data = Donnee_temp) %>% addTiles() %>%
    addCircleMarkers(~as.double(lon),~as.double(lat), popup=~content,
                     radius = ~as.double(coef)*10,
                     fillOpacity = 0.8,color="red")%>%
    addCircleMarkers(capteur_lon,capteur_lat, popup=capteur_content,
                     radius = 8,
                     fillOpacity = 0.8,color="green")


}
