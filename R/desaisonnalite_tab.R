#' Extraction du bruit a partir d'un tableau issu de "donnees_horaires_groupes"
#'
#'
#' @param donnees Un tableau issu de "donnees_horaires_groupes"
#'
#' @return Un tableau à "nombre de capteur+1" colonnes
#'          - "date" : date des mesures
#'          - non de chaque capteur_sens du capteur : bruit statistique correspondant
#'            à chaque colonne du tableau entré
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#'
#' @examples
#' \dontrun{
#' data(chateaubourg)
#'
#' segs <- c("Burel","ParisMarché","ParisArcEnCiel","RteVitré","RueGdDomaine")
#' dir <- c("Rgt","Lft","Lft","Rgt","Rgt")
#'
#' donnees_synchrone <- donnees_horaires_groupes(chateaubourg,segs,9,dir)
#'
#' donnees_a_analyser <- desaisonnalite_tab(donnees_synchrone)
#'
#' mod <- lm(Burel_Rgt~., data = donnees_a_analyser[,-1])
#' summary(mod)
#'
#' }
#'
desaisonnalite_tab=function(donnees){
  # On extrait la date
  res <- tibble(donnees$date)
  # Pour chaque colonne (hors date) on extrait le bruit statistique et on rajoute une colonne aux resultats
  for(i in colnames(donnees)[colnames(donnees)!="date"]){
    bruit <- desaisonnalite(donnees,i,"add")$bruit
    res <- bind_cols(res,bruit)
  }
  # On reprend le nom des colonnes
  colnames(res) <- colnames(donnees)
  return(res)
}
