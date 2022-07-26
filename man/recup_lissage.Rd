% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recup_lissage.R
\name{recup_lissage}
\alias{recup_lissage}
\title{Récupération des coordonnées des courbes de lissages des courbes de vitesses}
\usage{
recup_lissage(Donnees, Id, orientation)
}
\arguments{
\item{Donnees}{Tableau issue de l'import de donnée Telraam}

\item{Id}{Nom du cateur d'interet}

\item{orientation}{Choix de la direction du capteur ("Toute"/"Rgt"/"Lft")}
}
\value{
Un dataframe à 5 colonnes correspondant à l'absisse et aux ordonnées des courbes
}
\description{
Récupération des coordonnées des courbes de lissages des courbes de vitesses
}
\examples{
\dontrun{

liste_capteurs <- c("9000002156", "9000001844")

liste_noms <- c("Burel","RteVitré")

data("clef")

Donnees <- import(liste_capteurs, liste_noms, clef)$donnee

recup_lissage(Donnees, "Burel","Toute") \%>\% summary()

}
}
