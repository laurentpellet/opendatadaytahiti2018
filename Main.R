library(jsonlite)
questions<-fromJSON("Questions_gouvernement_XIV.json")
index<-questions$questionsGvt$question$indexationAN
auteur<-questions$questionsGvt$question$auteur
textes<-questions$questionsGvt$question$textesReponse


table(index$rubrique)
