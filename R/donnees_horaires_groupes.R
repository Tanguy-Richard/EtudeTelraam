 #' Jointure sur les dates sur listes de segments
#'
#' A partir de choix de capteur, de sens et d'une heure, isoler les
#' donnees sur des dates communes pour une liste de capteurs (car+heavy)
#'
#' @param donnees tableau issu de l'import de données Telraam
#' @param segments  vecteur des 2 capteurs
#' @param heure l'heure choisie pour l'analyse
#' @param dir vecteurs des directions des capteurs choisis
#'
#' @return Un tableau à "nombre de capteur+1" colonnes
#'          - "date" : date des mesures
#'          - non de chaque capteur_sens du capteur : VL + PL dans la direction choisie
#'          correspondant aux dates communes avec les autres capteurs
#' @export
#'
#' @importFrom lubridate hour
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join filter bind_cols
#'
#' @examples
#' \dontrun{
#'
#' segs <- c("Burel","ParisMarché","ParisArcEnCiel","RteVitré","RueGdDomaine")
#' dir <- c("Rgt","Lft","Lft","Rgt","Rgt")
#'
#' donnees_synchrone <- donnees_horaires_groupes(donnees_nn_plus,segs,9,dir)
#'
#' donnees_a_analyser <- desaisonnalite_tab(donnees_synchrone)
#'
#' mod <- lm(Burel_Rgt~., data = donnees_a_analyser[,-1])
#' summary(mod)
#'
#' }
donnees_horaires_groupes=function(donnees,segments,heure,dir){
  # On initie notre boucle sur les segments avec le premier
  # On conserve le capteur et l'heure choisie
  Segs <- donnees %>% filter(segment_id==segments[1],
                             hour(date)==heure)
  # On conserve la date
  res <- tibble(Segs$date)
  # choix de la direction
  if(dir[1]=="Toute"){
    tot <- Segs$car + Segs$heavy
  }
  if(dir[1]=="Rgt"){
    tot <- Segs$car_rgt + Segs$heavy_rgt
  }
  if(dir[1]=="Lft"){
    tot <- Segs$car_lft + Segs$heavy_lft
  }
  # On crée le tableau avec la date et les mesures du premier capteur
  res <- bind_cols(res,tot)
  colnames(res) <- c("date",paste(segments[1],dir[1],sep="_"))
  # Pour chaque capteur restant:
  n=length(segments)
  for(i in 2:n){
    # On conserve l'heure choisie et le capteur
    Segs <- donnees %>% filter(segment_id==segments[i],
                               hour(date)==heure)
    # On extrait la date
    res_temp <- tibble(Segs$date)
    #On choisie le sens
    if(dir[i]=="Toute"){
      tot <- Segs$car + Segs$heavy
    }
    if(dir[i]=="Rgt"){
      tot <- Segs$car_rgt + Segs$heavy_rgt
    }
    if(dir[i]=="Lft"){
      tot <- Segs$car_lft + Segs$heavy_lft
    }
    # On crée le tableau avec la date et les mesures du capteur
    res_temp <- bind_cols(res_temp,tot)
    colnames(res_temp) <- c("date",paste(segments[i],dir[i],sep="_"))
    # On fait une jointure interieur sur les dates conservées à l'étape précédente pour ne garder
    # que les dates communes
    res <- inner_join(res,res_temp,by="date")
  }
  # On test si le tableau est vide (pas de dates communes)
  if(length(res$date)==0){
    return(NULL)
  }else{
    return(res)
  }
}
