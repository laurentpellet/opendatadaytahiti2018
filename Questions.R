library(jsonlite)
library(data.table)

questions<-fromJSON("Questions_gouvernement_XIV.json")
q<-questions$questionsGvt$question

q1<-cbind(
  q$uid,
  q$identifiant,
  q$indexationAN$rubrique,
  q$indexationAN$teteAnalyse)
  



auteur<-questions$questionsGvt$question$auteur
textes<-questions$questionsGvt$question$textesReponse


table(index$rubrique)
