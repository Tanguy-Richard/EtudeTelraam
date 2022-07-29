#' Séparation d'un signal en 3 part ( tendance, cycle hebdomadaire et bruit statistique)
#'
#' @param donnees Un dataframe avec une colonne "date" (format lubridate) et une colonne cible
#' @param col Nom de la colonne cible
#' @param model un modèle (multiplicatif ou additif) pour la séparation
#'
#' @return une liste de 3 vecteurs: la tendance, le cycle hebdomadaire et le bruit statistique
#' @export
#'
#' @importFrom lubridate hour wday
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter group_by
#' @importFrom forecast ma
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#'
#'
#'
#' liste_capteurs <- c("9000002156", "9000001844")
#'
#' liste_noms <- c("Burel","RteVitré")
#'
#' data("clef")
#'
#' Donnees <- import(liste_capteurs, liste_noms, clef)$donnee
#'
#' Burel_Vitre <- donnees_horaire(Donnees,c("Burel","RteVitré"),9,"Lft","Lft")
#'
#' Burel_traitement <- desaisonnalite(Burel_Vitre,"Burel","add")$bruit
#' Vitre_traitement <- desaisonnalite(Burel_Vitre,"RteVitré","add")$bruit
#'
#' plot(Burel_Vitre$date,Burel_traitement,type="l")
#' lines(Burel_Vitre$date,Vitre_traitement,col="red")
#'}
#'
desaisonnalite=function(donnees,col,model){
  # Calcul de la tendance à l'aide d'une moyenne glissante
  tendance <- as.vector(ma(donnees[,col],order = 14))
  # Extraction de la tendance selon le choix de modèle
  if(model=="mult"){
    sanstendance <- donnees[,col] / tendance
  }
  if(model=="add"){
    sanstendance <- donnees[,col] - tendance
  }
  # Donnees sans tendance
  tab_temp <- as_tibble(cbind(donnees$date,sanstendance))
  colnames(tab_temp) <- c("date","ma")
  # Determination de l'impact du jour de la semaine
  jours_semaines <- tab_temp %>%
    group_by(wday(date)) %>%
    mutate(moyday=mean(ma,na.rm = TRUE)) %>%
    filter (! duplicated(wday(date))) %>%
    arrange(wday(date))
  colnames(jours_semaines) <- c("date","ma","Jsem","moyday")
  # Séparation des données sans tendance en un cycle hebdomadaire et un signal restant (bruit)
  decycle <- NULL
  cycle <- NULL
  for(i in 1:length(tab_temp$date)){
    # Récupération du jour de la semaine
    jour <- wday(tab_temp$date[i])
    # Récupération de la valeur du jour de la semaine
    val_jour <- jours_semaines %>% filter(Jsem==jour) %>%
      .$moyday
    # Séparaison selon le modèle choisie
    cycle <- c(cycle,val_jour)
    if(model=="mult"){
      val_restante <- tab_temp$ma[i]/val_jour
    }
    if(model=="add"){
      val_restante <- tab_temp$ma[i]-val_jour
    }
    decycle <- c(decycle,val_restante)
  }

  return(list(tendance=tendance,cycle=cycle,bruit=decycle))
}
