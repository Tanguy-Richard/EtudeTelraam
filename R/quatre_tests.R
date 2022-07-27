#' Calculs de seuils à partir de la détection dans la cassure de courbes à partir de 4 tests
#'
#' Cette fonction s'appuit lourdement sur le package CPAT:
#' Curtis Miller (2018). CPAT: Change Point Analysis Tests. R package version 0.1.0.
#' https://CRAN.R-project.org/package=CPAT
#'
#' @param Donnees Tableau issue de l'import de donnée Telraam
#' @param Id Nom du cateur d'interet
#' @param orientation Choix de la direction du capteur ("Toute"/"Rgt"/"Lft")
#'
#' @return Un dataframe à 5 colonnes correspondant à l'absisse et aux ordonnées des courbes
#' @export
#'
#' @importFrom CPAT CUSUM.test DE.test HR.test HS.test
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
#' quatre_tests(Donnees, "Burel","Toute")
#'
#' }
quatre_tests=function(Donnees,Id,orientation){ # En utilisant que les p value inf a 0.05
  # Récupération des données de lissages
  Donnee <- recup_lissage(Donnees,Id,orientation)
  # Réalisation de différents tests de cassure de courbes
  res_cusum <- NULL
  res_de <- NULL
  res_hr <- NULL
  res_hs <- NULL
  for(i in 2:5){ # Itération sur les courbes des différentes vitesses
    # CUSUM TEST
    test_cusum <- CUSUM.test(Donnee[,i])
    x <- Donnee[test_cusum$estimate,1]
    if(test_cusum$p.value<0.05){res_cusum <- c(res_cusum,x)}else{res_cusum <- c(res_cusum,NA)}
    # Darling-Erdos
    test_de <- DE.test(Donnee[,i])
    x <- Donnee[test_de$estimate,1]
    if(test_de$p.value<0.05){res_de <- c(res_de,x)}else{res_de <- c(res_de,NA)}
    # (univariate) Renyi-type test
    test_hr <- HR.test(Donnee[,i])
    x <- Donnee[test_hr$estimate,1]
    if(test_hr$p.value<0.05){res_hr <- c(res_hr,x)}else{res_hr <- c(res_hr,NA)}
    # (univariate) Hidalgo-Seo
    test_hs <- HS.test(Donnee[,i])
    x <- Donnee[test_hs$estimate,1]
    if(test_hs$p.value<0.05){res_hs <- c(res_hs,x)}else{res_hs <- c(res_hs,NA)}
  }
  # Calcul du seuil moyen
  CUSUM <- c(res_cusum, mean(res_cusum,na.rm = TRUE))
  D.E. <- c(res_de, mean(res_de,na.rm = TRUE))
  Renyi <- c(res_hr, mean(res_hr,na.rm = TRUE))
  H.S. <- c(res_hs, mean(res_hs,na.rm = TRUE))
  # Création du tableau des resultats
  res <- rbind(CUSUM,D.E.,Renyi,H.S.)
  colnames(res) <- c("Plus_40","Plus_30","Plus_20","Plus_10","Moyenne")
  return(res)
}
