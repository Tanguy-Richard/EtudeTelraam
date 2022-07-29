#' Comparaison de deux périodes pour le même capteur
#'
#' Trace les courbes de fréquentation horaire pour les deux périodes et teste
#' l'égalité des répartition entre les périodes
#'
#' @param donnees donnees issues d'un import de données Telraam
#' @param capteur capteur d'interet
#' @param sens sens du capteur ("Rgt"/"Lft"/"Toute")
#' @param mobilite vecteur des type de mobilité d'interet (parmis "car","heavy","bike","pedestrian")
#' @param periode1 interval lubridate pour la période 1
#' @param periode2 interval lubridate pour la période 2
#' @param vacance1 choix de garder ou non les vacances pour la période 1 ("oui","non","exclusivement")
#' @param vacance2 choix de garder ou non les vacances pour la période 2 ("oui","non","exclusivement")
#' @param JF1 choix de garder ou non les jours fériés pour la période 1 ("oui","non","exclusivement")
#' @param JF2 choix de garder ou non les jours fériés pour la période 2 ("oui","non","exclusivement")
#' @param SM1 choix des jours de la semaine pour la période 1 (vecteur composé de nombre entre 1 et 7)
#' @param SM2 choix des jours de la semaine pour la période 2 (vecteur composé de nombre entre 1 et 7)
#'
#' @return un graphique composé des courbes des deux périodes et des resultats d'un test de wilcoxon
#' heure par heure
#' Ou un message explicitant l'absence de données
#' @export
#'
#' @importFrom lubridate wday hour
#' @importFrom stats wilcox.test qt var
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by filter select mutate arrange summarise rename n
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon ggtitle expand_limits scale_x_continuous scale_color_manual
#' @importFrom ggplot2 geom_histogram theme element_text element_blank scale_fill_manual labs
#' @importFrom cowplot plot_grid
#'
#' @examples
#' \dontrun{
#'
#' data(chateaubourg)
#'
#' periode1 <- interval(ymd_hms("2021/01/01 00:00:00"),ymd_hms("2022/01/01 00:00:00"))
#' periode2 <- interval(ymd_hms("2022/01/01 00:00:00"),ymd_hms("2023/01/01 00:00:00"))
#'
#' courbe_periode(chateaubourg,"Burel","Lft",c("car","heavy"),periode1,periode2,
#'                "oui","non","oui","non",1:5,1:5)
#' }
courbe_periode <- function(donnees,capteur,sens,mobilite,periode1,periode2,vacance1,vacance2,JF1,JF2,SM1,SM2){

  # Filtrage sur le capteur sélectionnée
  donnees <- donnees %>% filter(segment_id==capteur)

  # Filtrage sur le sens choisie
  if(sens=="Toute"){
    donnees <- donnees[,c("date","uptime","heavy","car","bike","pedestrian")]
  }
  if(sens=="Rgt"){
    donnees <- donnees %>% select(c(date,uptime,heavy_rgt,car_rgt,bike_rgt,pedestrian_rgt)) %>%
      rename(c(heavy = heavy_rgt, car = car_rgt, bike = bike_rgt, pedestrian = pedestrian_rgt))
  }
  if(sens=="Lft"){
    donnees <- donnees %>% select(c(date,uptime,heavy_lft,car_lft,bike_lft,pedestrian_lft)) %>%
      rename(c(heavy = heavy_lft, car = car_lft, bike = bike_lft, pedestrian = pedestrian_lft))
  }

  # Filtrage sur la sélection de mobilités
  if(length(mobilite)>1){
    interet <- apply(donnees[,mobilite], MARGIN = 1 ,FUN = sum)
    donnees$total <- interet
  }else{
    donnees$total <- donnees[,mobilite]
  }

  # Test pour savoir si la sélection est vide
  if(length(donnees$date)==0){
    return("Pas de donnes pour cette selection")}else{

      donnees_1 <- selection_date(donnees,periode1) %>%
        .$donnees_correspondantes

      # Sélection des jours de toute la semaine
      donnees_1 <- donnees_1 %>% filter(wday(date) %in% SM1)

      if(length(donnees_1$date)==0){ # Test pour savoir si la sélection est vide
        return("Pas de donnees pour la selection de la periode 1")
      }else{ # Sélection de vacances
        if(vacance1=="non"){
          donnees_1 <- selection_date(donnees_1,vacances()) %>%
            .$donnees_complementaires
        }
        if(vacance1=="exclusivement"){
          donnees_1 <- selection_date(donnees_1,vacances()) %>%
            .$donnees_correspondantes
        }
        if(length(donnees_1$date)==0){  # Test pour savoir si la sélection est vide
          return("Pas de donnees pour la selection de la periode 1")
        }else{ # Sélection de jours fériés
          if(JF1=="non"){
            donnees_1 <- selection_date2(donnees_1,jours_feries()) %>%
              .$donnees_complementaires
          }
          if(JF1=="exclusivement"){
            donnees_1 <- selection_date2(donnees_1,jours_feries()) %>%
              .$donnees_correspondantes
          }
          if(length(donnees_1$date)==0){  # Test pour savoir si la sélection est vide
            return("Pas de donnees pour la selection de la periode 1")
          }
        }
      }}

  # Test pour savoir si la sélection est vide
  if(length(donnees$date)==0){
    return("Pas de donnes pour cette selection")}else{



      donnees_2 <- selection_date(donnees,periode2) %>%
        .$donnees_correspondantes

      # Sélection des jours de toute la semaine
      donnees_2 <- donnees_2 %>% filter(wday(date) %in% SM2)

      if(length(donnees_2$date)==0){ # Test pour savoir si la selection est vide
        return("Pas de donnees pour la selection de la periode 2")
      }else{ # Sélection de vacances
        if(vacance2=="non"){
          donnees_2 <- selection_date(donnees_2,vacances()) %>%
            .$donnees_complementaires
        }
        if(vacance2=="exclusivement"){
          donnees_2 <- selection_date(donnees_2,vacances()) %>%
            .$donnees_correspondantes
        }
        if(length(donnees_2$date)==0){  # Test pour savoir si la sélection est vide
          return("Pas de donnees pour la selection de la periode 2")
        }else{ # Sélection de jours fériés
          if(JF2=="non"){
            donnees_2 <- selection_date2(donnees_2,jours_feries()) %>%
              .$donnees_complementaires
          }
          if(JF2=="exclusivement"){
            donnees_2 <- selection_date2(donnees_2,jours_feries()) %>%
              .$donnees_correspondantes
          }
          if(length(donnees_2$date)==0){  # Test pour savoir si la sélection est vide
            return("Pas de donnees pour la selection de la periode 2")
          }
        }
      }}

  # Moyenne par tranche horaire
  n_1 <- donnees_1 %>% group_by(hour(date)) %>% summarise(n = n())
  Donnee_1 <- donnees_1 %>%
    group_by(hour(date)) %>%
    mutate(Moyenne=mean(total), Var=var(total)) %>%
    filter (!duplicated(hour(date))) %>%
    arrange(hour(date))
  # Sélection de la colonne heure et celle colonne horaire
  Donnee_1 <- bind_cols(Donnee_1[,8:10],n_1[,2])
  # Rajout d'une colonne répétant "Periode Ref"
  Donnee_1 <- cbind(Donnee_1,rep("Periode_Ref",length(Donnee_1[,1])))

  # Moyenne par tranche horaire
  n_2  <- donnees_2 %>% group_by(hour(date)) %>% summarise(n = n())
  Donnee_2 <- donnees_2 %>%
    group_by(hour(date)) %>%
    mutate(Moyenne=mean(total), Var=var(total)) %>%
    filter (!duplicated(hour(date))) %>%
    arrange(hour(date))
  # Sélection de la colonne heure et celle colonne horaire
  Donnee_2 <- bind_cols(Donnee_2[,8:10],n_2[,2])
  # Rajout d'une colonne répétant "Periode 1"
  Donnee_2 <- cbind(Donnee_2,rep("periode1",length(Donnee_2[,1])))

  # Concaténation à la suite des tableaux
  Donnee <- rbind(Donnee_1,Donnee_2)
  # Renomage des colonnes
  colnames(Donnee) <- c("Heure","Nombre_usagers","Variance","Effectif","Periode")

  # Selection des colonnes sur lesquelles on a une variance (2 valeurs au moins)
  Donnee <- Donnee %>% filter(Effectif>1)

  # Calcul des bornes (loi de student)
  Donnee <- Donnee%>% mutate(q=qt(.975,df=Effectif-1))
  Donnee <- Donnee %>% mutate(Born1=Nombre_usagers-q*sqrt(Variance/Effectif),
                              Born2=Nombre_usagers+q*sqrt(Variance/Effectif))
  # Sélection des heures communes
  heure1 <- as.numeric(levels(as.factor(hour(donnees_1$date))))
  heure2 <- as.numeric(levels(as.factor(hour(donnees_2$date))))
  heure <- intersect(heure1,heure2)
  # Nombre d'heure
  k=length(heure)

  # Récupération des termes en français
  fr <-  c("Vehicules Legers","Poids Lourds","Pietons","Velos")
  eng <-  c("car","heavy","pedestrian","bike")
  mobfr <- fr[eng %in% mobilite]
  # Concaténation des mobilités séparées par "+"
  titre_fr <- paste(mobfr,collapse = " + ")


  # Création du graphique faisant apparaitre les courbes des deux périodes
  l <- ggplot(Donnee)+aes(x = Heure, y=Nombre_usagers, group = Periode, color = Periode)+geom_line(aes(linetype=Periode),size=1.5)+
    labs(x="Heure", y = "Nombre moyen d'usagers")+
    geom_ribbon(aes(ymin=Born1, ymax=Born2, fill = Periode), linetype = "blank",alpha = 1/4)+
    ggtitle(titre_fr)+scale_x_continuous(breaks=heure,limits = c(heure[1]-0.5,heure[k]+0.5))+
    expand_limits(y = 0)+ scale_color_manual(values=c("#ff5900","#006bb6" ))+
    scale_fill_manual(values=c("#ff5900","#006bb6" ))

  # Création du vecteur qui va servir à stocker les valeurs des test de Wilcoxon
  Stat_wilcox <- NULL
  Couleur <- NULL
  # Boucle sur chaque heure
  for(i in heure){
    # Test de wilcoxon pour une heure donnée
    stata <- wilcox.test(donnees_1 %>%
                           filter(hour(date)==i) %>%
                           .$total,
                         donnees_2 %>%
                           filter(hour(donnees_2$date)==i) %>%
                           .$total
    )$p.value
    Stat_wilcox <- c(Stat_wilcox, stata)
    # Choix de la couleur en fonction de la p-valeur du test
    if(stata<0.05){
      Couleur <- c(Couleur,"Significatif")
    }
    if(stata>=0.1){
      Couleur <- c(Couleur,"Non-significatif")
    }
    if(stata>=0.05 & stata<0.1){
      Couleur <- c(Couleur,"Entre deux")
    }

  }

  # Création du tableau pour la barre indiquant la significativité des tests
  Don2 <- as_tibble(cbind(heure,Couleur))
  colnames(Don2) <- c("heure","couleur")

  # Graphique: histogramme d'une unité de hauteur, indiquant la valeur de la significativité
  h <- ggplot(Don2)+aes(x = as.double(heure) , color = couleur, fill = couleur) +
    geom_histogram(bins = k+1)+scale_x_continuous(breaks=as.double(heure),limits = c(as.double(heure[1])-0.5,as.double(heure[k])+0.5))+
    scale_color_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
    scale_fill_manual(values = c("Significatif"="#D55E00","Non-significatif"="#56B4E9","Entre deux"="#D4D4D4"))+
    theme(
      title = element_text(hjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())+
    labs(title="Significativite d'une difference de comportement (*)",
         x="Heure",
         y="")

  # Alignement des deux graphiques
  plot_grid(l, h, align = "v", ncol = 1, rel_heights = c(0.75, 0.25))

}
