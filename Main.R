library(jsonlite)

questions<-fromJSON("Questions_gouvernement_XIV.json")
acteurs<-fromJSON("AMO10_deputes_actifs_mandats_actifs_organes_XIV.json")

acteursDF<-acteurs$export$acteurs$acteur$etatCivil

index<-questions$questionsGvt$question$indexationAN
auteur<-questions$questionsGvt$question$auteur
textes<-questions$questionsGvt$question$textesReponse


table(index$rubrique)
