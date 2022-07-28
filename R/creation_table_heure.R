#' Tableau de correlation entre un capteur de référence et d'autres capteurs
#'
#' @param donnees donnees issues d'un import de données Telraam
#' @param segment capteur de référence
#' @param sens sens du capteur de référence ()
#' @param liste_seg vecteur de capteurs à comparer
#' @param liste_sens vecteur des sens de capteurs à comparer
#' @param liste_heure une liste d'heure pour lesquels on veut la comparaison
#' @param jour un jour (nombre entre 1 et 7)
#'
#' @return Un tableau donc les lignes sont les capteurs à comparés et les colonnes les heures choisies,
#' les valeurs correspondent aux coefficient de correlations de Pearson entre le capteur de reference
#' et le capteur de la ligne pour le jour entré, à l'heure de la colonne
#' @export
#'
#' @importFrom lubridate wday
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#'
#' data(chateaubourg)
#'
#' creation_table_heure(chateaubourg,"Burel","Rgt",
#'                      c("RteVitré","ParisArcEnCiel","RueGdDomaine", "ParisMarché"),
#'                      c("Rgt","Lft","Rgt","Lft"),8:20,2) %>% View()
#'
#' }
creation_table_heure=function(donnees , segment , sens, liste_seg, liste_sens, liste_heure,jour){
  # Selection des dates hors vacances et jours fériés
  tableau_temp <- selection_date(donnees,vacances())$data2
  tableau_temp <- selection_date2(tableau_temp,jours_feries())$data2
  # Tableau vide pour stocker les résultats
  Donnee_res <- NULL
  # Pour chaque capteur de la liste:
  for(i in 1:length(liste_seg)){
    # On va stoker nos données pour chaque heure dans ce vecteur vide
    ligne_temp <- NULL
    # On itère sur l'ensemble des heures selectionnée
    for(j in liste_heure){
      # On filtre sur les dates communes
      tab_temp <- donnees_horaire(tableau_temp,c(segment,liste_seg[i]),j,sens,liste_sens[i])
      n=length(tab_temp$date)
      # On ne stocke rien si on a pas assez de données
      if(is.null(tab_temp)|n<=10){
        ligne_temp <- c(ligne_temp,NA)
      }else{
        # On isole les parties nous interessants (correspondant aux capteurs)
        segA_trait <- tab_temp[wday(tab_temp$date)==jour,segment]
        segB_trait <- tab_temp[wday(tab_temp$date)==jour,liste_seg[i]]
        # On ajoute la correlation entre les deux capteur à notre ligne
        ligne_temp <- c(ligne_temp,cor(segA_trait,segB_trait))
      }

    }
    # On rajoute la ligne correspondant au capteur au tableau de données
    Donnee_res <- rbind(Donnee_res,c(ligne_temp,mean(ligne_temp)))

  }
  # On nomme les colonnes et les lignes selon les sélections
  colnames(Donnee_res) <- c(liste_heure,"moyenne")
  row.names(Donnee_res) <- paste(liste_seg,liste_sens)

  return(Donnee_res)
}
