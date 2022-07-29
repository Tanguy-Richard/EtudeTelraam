#' Courbe de la fréquentation horaire selon les sens et de la vitesse v85
#'
#' @param donnees donnees issues d'un import de données Telraam
#' @param capteur capteur d'interet
#' @param periode interval lubridate pour la période
#' @param vacances choix de garder ou non les vacances ("oui","non","exclusivement")
#' @param JF choix de garder ou non les jours fériés ("oui","non","exclusivement")
#' @param SM choix des jours de la semaine pour la période 1 (vecteur composé de nombre entre 1 et 7)
#'
#' @return Un graphique plotly de la fréquentation et la vitesse V85 en fonction des heures de la journée
#' @export
#'
#' @importFrom lubridate wday hour
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by filter summarise full_join
#' @importFrom plotly plot_ly add_trace layout
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' data(chateaubourg)
#'
#' periode <- interval(ymd_hms("2021/01/01 00:00:00"),ymd_hms("2022/01/01 00:00:00"))
#'
#' courbe_par_heure(donnees_non_nulles,"Burel",periode,"oui","oui",1:5)
#' }
courbe_par_heure <- function(donnees,capteur,periode,vacances,JF,SM){

  #Séléction du capteur sélectionnée
  donnees_filtrees <- donnees %>%
    filter(segment_id==capteur)

  #Séléction de la période
  donnees_filtrees <- selection_date(donnees_filtrees,periode) %>%
    .$donnees_correspondantes

  if(length(donnees_filtrees$date)==0){  # Test pour savoir si la sélection est vide
    return("Pas de donnees pour la selection")
  }else{
    if(vacances=="Non"){
      donnees_filtrees <- selection_date(donnees_filtrees,vacances()) %>%
        .$donnees_complementaires
    }
    if(vacances=="exclusivement"){
      donnees_filtrees <- selection_date(donnees_filtrees,vacances()) %>%
        .$donnees_correspondantes
    }
    if(length(donnees_filtrees$date)==0){  # Test pour savoir si la sélection est vide
      return("Pas de donnees pour la selection")
    }else{ # Sélection de jours fériés
      if(JF=="Non"){
        donnees_filtrees <- selection_date2(donnees_filtrees,jours_feries()) %>%
          .$donnees_complementaires
      }
      if(JF=="exclusivement"){
        donnees_filtrees <- selection_date2(donnees_filtrees,jours_feries()) %>%
          .$donnees_correspondantes
      }
      if(length(donnees_filtrees$date)==0){  # Test pour savoir si la sélection est vide
        return("Pas de donnees pour la selection")
      }else{
        # Séléction des jours de la semaine
        jours <- SM
        # Calcul des moyenne par créneau horaire
        # Vitesse
        vitesse=donnees_filtrees %>%
          filter(wday(date) %in% jours) %>%
          group_by(hour(date)) %>%
          summarise(moy_vit=mean(v85,na.rm=TRUE))
        colnames(vitesse)=c("Heure", "Vitesse")
        # Trafic B -> A
        trafic_rgt=donnees_filtrees %>%
          filter(wday(date) %in% jours) %>%
          group_by(hour(date)) %>%
          summarise(moy_rgt=mean(car_rgt+heavy_rgt,na.rm=TRUE))
        colnames(trafic_rgt)=c("Heure", "Voiture_BversA")
        # Trafic A -> B
        trafic_lft=donnees_filtrees %>%
          filter(wday(date) %in% jours) %>%
          group_by(hour(date)) %>%
          summarise(moy_lft=mean(car_lft+heavy_lft,na.rm=TRUE))
        colnames(trafic_lft)=c("Heure", "Voiture_AversB")
        # Tableau final pour le graphique
        trafic_horaire = full_join(trafic_lft, trafic_rgt, by = "Heure")
        trafic_horaire = full_join(trafic_horaire, vitesse, by = "Heure")

        if(length(trafic_horaire$Heure)==0){ # Test pour savoir si la sélection est vide
          return("Pas de donnees pour la selection")
        }else{
          # Création du graphique
          # Placement des courbes des véhicules
          fig <- plot_ly(trafic_horaire, x = ~Heure)
          fig <- fig %>% add_trace(y= ~Voiture_BversA, mode = "lines+markers", name = "B vers A",
                                   line=list(color="blue", dash = "dot"),
                                   marker=list(color="blue"))
          fig <- fig %>% add_trace(y= ~Voiture_AversB, mode = "lines+markers", name = "A vers B",
                                   line=list(color="blue", dash = "dash"),
                                   marker=list(color="blue"))
          # Création du second axe des ordonnées
          ay <- list(
            tickfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Vitesse v85 moyenne (km/h)")
          # Placement de la courbe de vitesse
          fig <- fig %>% add_trace(y= ~Vitesse,  yaxis = "y2", mode = "lines+markers", name = "Vitesse v85 moyenne",
                                   line=list(color="red"),
                                   marker=list(color="red"))
          # Mise en place du titre et des axes
          fig <- fig %>% layout(
            title = "", yaxis2 = ay,
            xaxis = list(title="Heure"),
            yaxis = list(title="Nombre de vehicules moyen")
          )%>%
            layout(plot_bgcolor='#e5ecf6',
                   xaxis = list(
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff',
                     dtick=1),
                   yaxis = list(
                     tickfont= list(color="blue"),
                     zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff')
            )

          fig

        }

      }

    }
  }
}
