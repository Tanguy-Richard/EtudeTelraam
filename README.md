
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EtudeTelraam

<!-- badges: start -->
<!-- badges: end -->

L’objectif du package est de permettre l’import et l’analyse de données
issues de capteurs Telraam. Il a été développé pour l’association [Agis
Ta Terre](https://www.agistaterre.org/), dans le but d’analyser des
capteurs situés sur la commune de Chateaubourg.

## Installation

Vous pouvez installer la version de developpement du package depuis
[GitHub](https://github.com/) avec:

``` r
# install.packages("devtools")
devtools::install_github("Tanguy-Richard/EtudeTelraam")
```

## Example

## Example

Cette exemple simple vous montre une analyse possible du package:

``` r
library(EtudeTelraam)

# Chargement du tableau de données inclu dans le package
data(chateaubourg)

# Import des périodes de vacances
vacance <- vacances()

# Filtrage pour ne garder que les vacances
donnees_filtrees <- selection_date(chateaubourg,vacance) %>%
.$donnees_correspondantes

# Courbe des vitesses atteintes par les usager selon la circulation en période de vacances
courbe_vitesse(donnees_filtrees, "Burel","Toute")
```
