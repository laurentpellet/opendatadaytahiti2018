acteur <- acteurs$export$acteurs$acteur
acteursDF<-cbind(ActeurRef=acteur$uid$`#text`, 
                 acteur$etatCivil$ident,
                 acteur$profession$socProcINSEE,
                 acteur$profession$libelleCourant)


table(acteur$profession$socProcINSEE)
head(acteurs$export$acteurs$acteur$profession)
