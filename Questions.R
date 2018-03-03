library(jsonlite)
library(data.table)
library(XML)
library(RCurl)

q<-fromJSON("Questions_gouvernement_XIV.json")
q<-q$questionsGvt$question
pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"

q1<-cbind(
  q$uid,
  q$identifiant,
  q$indexationAN$rubrique,
  q$indexationAN$teteAnalyse,
  unlist(lapply(q$indexationAN$analyses$analyse, FUN=function(x)paste0(x,collapse = ","))),
  q$auteur$identite,
  q$auteur$groupe,
  q$minInt,
  gsub(pattern, "\\1", q$textesReponse$texteReponse$texte))

colnames(q1)[c(1,5,6,7,13,14,15)]<-c("uid", "rubrique", "teteAnalyse", "motsCles","minIntAbrege","minIndDeveloppe","question")
