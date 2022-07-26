#' Graphique de la part d'usagers atteignant certaines vitesses selon la circulation avec un seuil
#'
#' Trace sur un graphique le pourcentages d'usagers arrivant a dépasser respectivement
#' 10km/h, 20km/h, 30km/h et 40km/h en fonction du  nombres d'usagers par heure (voiture plus poids lourds)
#' Affiche un seuil calculé à partir d'un test de Darling Erdos
#'
#' Cette fonction s'appuit sur le test de Darling-Erdos du package CPAT:
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
#' @importFrom CPAT DE.test
#' @importFrom stats embed
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter select arrange
#' @importFrom ggplot2 ggplot aes geom_smooth geom_line ggtitle labs geom_smooth geom_vline geom_text scale_x_continuous
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
#' graphe_seuil(Donnees, "Burel","Toute")
#'
#' }
graphe_seuil=function(Donnees,Id,orientation){
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

  VehG <- rep(vehicule,4)
  Vitesse <- c(vitesse10,vitesse20,vitesse30,vitesse40)
  #Création du tableau pour l'import
  TablRes <- as.data.frame(cbind(vehicule,vitesse10,vitesse20,vitesse30,vitesse40))
  colnames(TablRes) <- c("Nombre de vehicules","plus de 40km/h","plus de 30km/h","plus de 20km/h","plus de 10km/h")

  # Creation du tibble pour commencer le graphique
  k=length(vehicule)
  Legende <- c(rep("Plus de 10km/h",k),rep("Plus de 20km/h",k),rep("Plus de 30km/h",k),rep("Plus de 40km/h",k))
  donnee <- tibble(VehG,Vitesse,Legende)


  # Récupération des courbes lissées à partir de la méthode smooth de R
  p <- ggplot(donnee)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+stat_smooth()
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
  # Calcul des seuils pour chaque courbes à partir d'un test de Darling Erdos
  res2 <- NULL
  for(i in c(2:5)){ #Itération sur les courbes de vitesses
    tt <- DE.test(Donnee[,i]) # Darling-Erdos
    x <- Donnee[tt$estimate,1]
    res2 <- c(res2,x)
  }
  moyenne <- mean(res2)
  # Préparation de l'indexation de l'abscisse
  mmV <- max(donnee$VehG)
  if(mmV<100){
    absi <- seq(0,100,10)
  }else{
    if(mmV<500){
      absi <- seq(0,500,50)

    }else{
      maxi <- floor(mmV/100)*100
      absi <- seq(0,maxi,100)

    }
  }

  # ordonnée de l'afichage de la valeur du seuil
  miny <- min(vitesse40)
  ordMoy <- (100+miny)/2

  # Graphique du seuil
  ggplot(donnee)+aes(x=VehG, y=Vitesse, color = Legende, group=Legende)+geom_line(color="black")+
    geom_smooth()+labs(x="Nombre de vehicules sur une tranche horaire", y = "Pourcentage de vehicule depassant la vitesse donnees")+
    ggtitle("Evolution de la vitesse de conduite selon le nombre d'usagers")+
    scale_x_continuous(breaks=c(absi), labels=c(absi))+ labs(fill = "")+ geom_vline(xintercept=moyenne,color="red", size = 1.5)+
    geom_text(aes(x=moyenne, y=ordMoy,label=round(moyenne)),size=5,angle=-90, vjust=-0.5,color="red")


}
