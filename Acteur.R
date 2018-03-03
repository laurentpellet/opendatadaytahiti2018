acteur <- acteurs$export$acteurs$acteur
acteursDF<-cbind(acteur$etatCivil$ident,
                 acteur$profession$libelleCourant,
                 acteur$mandats$mandat)
names(acteur$mandats$mandat[[1]])
names(acteurs$export$organes$organe)
head(acteurs$export$organes$organe$positionPolitique)
head(acteurs$export$acteurs$acteur$profession)
