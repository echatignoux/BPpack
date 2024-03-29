##======================================================================
### --- build_pack.R ---
##======================================================================
## Auteur: Edouard Chatignoux
## Cr�� le mer.  1 oct. 2014
## Derni�re mise � jour le 2022-04-04
##======================================================================
##  Description:
##--------------
##' Suite des �tapes pour construire un pacquage avec devtools.
##======================================================================

##'Cr�ation des r�pertoires
devtools::create()

##'Vignette
usethis::use_vignette("use_BPpack")

##'Liste des packages dont on a besoin
##' (use_package ajoute le paquet � DOCUMENT)
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("purrr")
usethis::use_package("magrittr")
usethis::use_package("rlang")
usethis::use_package("progress")
usethis::use_package("splines")
usethis::use_package("survey")
usethis::use_package("formula.tools")

## Fichier/dossiers � ignorer
usethis::use_build_ignore("working_example")
usethis::use_build_ignore("build_pack.R")

##'On charge tout (et on regarde si �a marche)
devtools::load_all()

##' Cr�ation des .Rd � partir des commentaires roxygen
devtools::document()

##' On regarde si tous va bien
##' dans les exemples...
devtools::run_examples()
##' et dans la structure
devtools::check()

## Manuel en pdf
devtools::build_manual()

## Construction du package (.zip)
devtools::build(binary = TRUE)

## Installation dans mon R
devtools::install()

