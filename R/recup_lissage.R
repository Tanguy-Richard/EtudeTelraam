#' Récupération des coordonnées des courbes de lissages des courbes de vitesses
#'
#' @param Donnees Tableau issue de l'import de donnée Telraam
#' @param Id Nom du cateur d'interet
#' @param orientation Choix de la direction du capteur ("Toute"/"Rgt"/"Lft")
#'
#' @return Un dataframe à 5 colonnes correspondant à l'absisse et aux ordonnées des courbes
#' @export
#'
#' @importFrom stats embed
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select arrange bind_cols
#' @importFrom ggplot2 ggplot aes stat_smooth geom_line ggtitle labs ggplot_build
#' @importFrom tibble tibble
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
#' recup_lissage(Donnees, "Burel","Toute") %>% summary()
#'
#' }
recup_lissage= function(Donnees,Id,orientation){
  # Filtrage des données selon le capteur choisi
  tableau_temp <- Donnees %>% filter(segment_id==Id)

  # Création des vecteurs pour stocker les vitesses
  Vit_moins10=NULL
  Vit_moins20=NULL
  Vit_moins30=NULL
  Vit_moins40=NULL
  for(i in tableau_temp$car_speed_hist_0to70plus){# On parcours les répartitions de vitesse
    vitesse <- unlist(i)
    # Somme progressive sur les parts d'usagers selon la vitesse
    vitesse10 <- vitesse[1]
    vitesse20 <- sum(vitesse[1:2])
    vitesse30 <- sum(vitesse[1:3])
    vitesse40 <- sum(vitesse[1:4])
    # Rajout des parts calculés aux vecteurs
    Vit_moins10 <- c(Vit_moins10,vitesse10)
    Vit_moins20 <- c(Vit_moins20,vitesse20)
    Vit_moins30 <- c(Vit_moins30,vitesse30)
    Vit_moins40 <- c(Vit_moins40,vitesse40)
  }
  tableau_temp$vit_moins10 <- Vit_moins10
  tableau_temp$vit_moins20 <- Vit_moins20
  tableau_temp$vit_moins30 <- Vit_moins30
  tableau_temp$vit_moins40 <- Vit_moins40

  # Création du tableau selon la direction choisie
  if(orientation=="Toute"){
    tableau_temp <- tableau_temp %>%
      select(car,heavy,vit_moins10,vit_moins20,vit_moins30,vit_moins40) %>%
      mutate(vehic = car + heavy) %>%
      arrange(vehic)
  }
  if(orientation=="Rgt"){
    tableau_temp <- tableau_temp %>%
      select(car_rgt,heavy_rgt,vit_moins10,vit_moins20,vit_moins30,vit_moins40) %>%
      mutate(vehic = car_rgt + heavy_rgt) %>%
      arrange(vehic)
  }
  if(orientation=="Lft"){
    tableau_temp <- tableau_temp %>%
      select(car_lft,heavy_lft,vit_moins10,vit_moins20,vit_moins30,vit_moins40)  %>%
      mutate(vehic = car_lft + heavy_lft) %>%
      arrange(vehic)
  }
  # Calcul de moyenne glissante chaque vitesses
  vitesse10 <- embed(tableau_temp$vit_moins10,50)
  vitesse10 <- apply(vitesse10,1,mean)
  vitesse10 <- 100-vitesse10
  vitesse20 <- embed(tableau_temp$vit_moins20,50)
  vitesse20 <- apply(vitesse20,1,mean)
  vitesse20 <- 100-vitesse20
  vitesse30 <- embed(tableau_temp$vit_moins30,50)
  vitesse30 <- apply(vitesse30,1,mean)
  vitesse30 <- 100-vitesse30
  vitesse40 <- embed(tableau_temp$vit_moins40,50)
  vitesse40 <- apply(vitesse40,1,mean)
  vitesse40 <- 100-vitesse40
  # Réalisation des abcisses
  vehicule <- embed(tableau_temp$vehic,50)
  vehicule <- apply(vehicule,1,mean)

  # Préparation des données pour le graphique
  VehG <- rep(vehicule,4)
  Vitesse <- c(vitesse10,vitesse20,vitesse30,vitesse40)
  k=length(vehicule)
  Legende <- c(rep("Plus de 10km/h",k),rep("Plus de 20km/h",k),rep("Plus de 30km/h",k),rep("Plus de 40km/h",k))
  donnee_graph <- tibble(VehG,Vitesse,Legende)

  # Récupération des données des courbes de lissages
  p <- ggplot(donnee_graph)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+stat_smooth()
  y <- ggplot_build(p)$data[[1]][,1:3]

  # Création du tableau pour stocké les données
  Donnee <- NULL
  for(i in levels(as.factor(y$colour))){
    Donnee <- bind_cols(Donnee,y[y$colour==i,-1])
  }
  # Absisse (nombre de véhicule par heure)
  X=Donnee[,1]
  # Ordonnée des courbes de lissages
  Y=Donnee[,c(2,4,6,8)]
  # Rangement des colonnes par vitesse:
  # Il y a un moins grand pourcentage d'usagers dépassant les 40km/h que ceux dépassant 30km/h (etc)
  Y <- t(t(Y)[order(t(Y)[,1]),])
  Donnee <- as.data.frame(bind_cols(X,Y))
  colnames(Donnee) <- c("Nombre de vehicules","plus de 40km/h","plus de 30km/h","plus de 20km/h","plus de 10km/h") #Renomage des colonnes

  return(Donnee)
}
