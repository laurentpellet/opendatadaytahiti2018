library(jsonlite)
library(data.table)

q<-fromJSON("Questions_gouvernement_XIV.json")
q<-q$questionsGvt$question
acteurs<-fromJSON("AMO30_tous_acteurs_tous_mandats_tous_organes_historique.json")
acteurs <- acteurs$export$acteurs$acteur
pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"

q1<-data.table(
  uid=q$uid,
  q$identifiant,
  rubrique=q$indexationAN$rubrique,
  teteAnalyse=q$indexationAN$teteAnalyse,
  motsCles=unlist(lapply(q$indexationAN$analyses$analyse, FUN=function(x)paste0(x,collapse = ","))),
  q$auteur$identite,
  groupe=q$auteur$groupe,
  ministre=q$minInt$abrege,
  #gsub(pattern, "\\1", q$textesReponse$texteReponse$texte),
  dateQuestion=q$cloture$dateCloture)

acteurs1<-data.table(acteurRef=acteurs$uid$`#text`, 
                     acteurs$etatCivil$ident,
                     acteurs$profession$socProcINSEE,
                     acteurs$profession$libelleCourant)


data<-merge(q1, acteurs1, by="acteurRef")

rm(list=c("pattern", "acteurs", "acteurs1", "q", "q1"))
