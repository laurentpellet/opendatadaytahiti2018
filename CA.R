# L'analyse des correspondances est plus adapté pour extraire l'information
# pertinente de ce tableau
data_ministreCA<-data2[,table(groupe.developpe,ministre)]
CA(data_ministreCA)
