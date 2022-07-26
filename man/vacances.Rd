% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vacances.R
\name{vacances}
\alias{vacances}
\title{Import des vacances de l'académie de Rennes}
\usage{
vacances()
}
\value{
un data frame à 4 colonnes: description (nom des vacances), start_date (date de début au format lubridate)
end_date (idem mais pour la date de fin) et interval (période de vacances au format interval de lubridate)
}
\description{
Import des vacances de l'académie de Rennes
}
\examples{
\dontrun{
data(chateaubourg)
vacance <- vacances()

donnees_filtrees <- selection_date(chateaubourg,vacance) \%>\%
                        .$donnees_correspondantes

summary(donnees_filtrees)

}
}
