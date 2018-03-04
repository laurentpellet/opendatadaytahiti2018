library(FactoMineR)
library(plyr)
library(scales)
library(ggplot2)
library(ggrepel)


graphMCA <- function(MCA,axes=c(1,2),HC=NULL,classeName=NULL,cos2min=0.05,cos2min.ind=0.05,
                     jit=NULL, alpha=0.2,
                     titre="Analyse des Correspondances Multiples (ACM)",
                     sousTitre=sprintf("La qualité de représentation doit être supérieure à %s pour que les modalités figurent le graphique.",percent(cos2min))){
  # correction de Benzecri 1979
  vp.corrige<-sapply(MCA$eig[MCA$eig[,1]>1/length(MCA$call$quali),1],FUN=function(x) (length(MCA$call$quali)/(length(MCA$call$quali)-1))^2*(x-1/length(MCA$call$quali))^2)
  eig_ajuste<-percent(vp.corrige/sum(vp.corrige))
  
  DT.quali   <- data.table(variable="Active",label=rownames(MCA$var$coord),Coord=MCA$var$coord[,axes],
                           Contrib=MCA$var$contrib[,axes],Cos2=MCA$var$cos2[,axes],v.test=MCA$var$v.test[,axes])
  if(!is.null(MCA$quali.sup)){
    DT.quali.sup.var   <- data.table(variable="Inactive",label=rownames(MCA$quali.sup$coord),Coord=MCA$quali.sup$coord[,axes],
                                     Cos2=MCA$quali.sup$cos2[,axes],v.test=MCA$quali.sup$v.test[,axes])
    DT.quali <- rbind(DT.quali,DT.quali.sup.var,fill=T)
  }
  
  setnames(DT.quali,names(DT.quali),c("variable","label","coord.1","coord.2",
                                      "contrib.1","contrib.2","cos2.1","cos2.2","v.test.1","v.test.2"))
  
  DT.ind <- data.table(Id=rownames(MCA$ind$coord),Coord=MCA$ind$coord[,axes],
                       Contrib=MCA$ind$contrib[,axes],Cos2=MCA$ind$cos2[,axes])
  setnames(DT.ind,names(DT.ind),c("Id","coord.1","coord.2","contrib.1","contrib.2","cos2.1","cos2.2"))
  DT.ind[,label:=Id]
  DT.ind[cos2.1+cos2.2<cos2min.ind,label:=""]
  
  DT.quali[variable=="Active" & cos2.1+cos2.2<cos2min,label:=""]
  DT.quali[,contrib:=""]
  DT.quali[abs(coord.1)>sqrt(MCA$eig[axes[1],1]), contrib:=paste("axe",axes[1])]
  DT.quali[abs(coord.2)>sqrt(MCA$eig[axes[2],1]), contrib:=paste("axe",axes[2])]
  DT.quali[abs(coord.1)>sqrt(MCA$eig[axes[1],1]) & abs(coord.2)>sqrt(MCA$eig[axes[2],1]), contrib:=paste("axes",axes[1],"et",axes[2])]
  DT.quali[variable=="Inactive", contrib:="Ne contribue pas"]
  
  if (!is.null(HC)) {
    DT.HC <- data.table(HC$data.clust,keep.rownames = T)
    DT.ind[DT.HC, classe:=i.clust,on=.(Id==rn)]
    DT.ind[,pct:=.N/DT.ind[,.N],by=classe]
    DT.ind[,coord.classe.1:=mean(coord.1),by=classe]
    DT.ind[,coord.classe.2:=mean(coord.2),by=classe]
    if(!is.null(classeName)) {DT.ind[,cName:=factor(classe,labels = classeName)]
    } else {DT.ind[,cName:=factor(classe)]}
    DT.ind[,cName:=paste(cName,":",percent(pct))]
  }
  g <- ggplot(data=DT.ind,aes(x = coord.1, y= coord.2))
  if(is.null(HC)) g <- g + geom_jitter(size=1,shape=21, alpha = alpha,width = jit, height = jit)
  else g <- g + geom_polygon(data = DT.ind[, .SD[chull(coord.1,coord.2)], by = classe],aes(fill=cName),
                             alpha = alpha/2) +
    geom_jitter(aes(fill=cName),size=1,shape=21, alpha = alpha,show.legend=FALSE,width = jit, height = jit)
  # else g <- g + geom_point(data=unique(DT.ind[,.(coord.classe.1,coord.classe.2,cName,pct)]),
  #                          aes(x=coord.classe.1,y=coord.classe.2,fill=cName,size=pct*100),
  #                          shape=21, alpha = 0.3,show.legend=FALSE)
  g <- g + geom_text_repel(data=DT.quali,
                           aes(x = coord.1, y= coord.2,label=label,
                               size=cos2.1+cos2.2,
                               # size=ifelse(variable=="Active",cos2.1+cos2.2,max(cos2.1+cos2.2)/2),
                               color=factor(contrib)),inherit.aes = F) +
    geom_text_repel(aes(label=label,size=cos2.1+cos2.2),color="blue") +
    scale_color_manual(breaks=c(paste("axe",axes[1]),paste("axe",axes[2]),
                                paste("axes",axes[1],"et",axes[2]),"Ne contribue pas"),
                       values = c("black","darkgreen","darkred","darkblue","blue")) +
    guides(fill=guide_legend(override.aes = list(alpha=0.2),
                             title = "Classification\ndes individus",
                             order=3, ncol = 1, title.position = "top",keyheight=0.8),
           color=guide_legend(title = "Contribution\nsignificative pour",
                              order=2,ncol=1, title.position = "top",keyheight=0.5),
           size=FALSE) +
    theme_light() +
    theme(legend.position=c(0,1),
          legend.justification = c(0,1),
          legend.box = "horizontal",
          legend.background=element_blank(),
          legend.text=element_text(size=8),
          legend.title=element_text(size=8),
          legend.margin = margin(b=0.1,l=0.15,r=0,unit = 'cm')) +
    geom_hline(yintercept = 0,linetype=3) +
    geom_vline(xintercept = 0,linetype=3) +
    xlab(paste("Axe",axes[1],"(",eig_ajuste[axes[1]],")")) + 
    ylab(paste("Axe",axes[2],"(",eig_ajuste[axes[2]],")")) +
    ggtitle(titre,subtitle=sousTitre)
  return(g)
} 


data_rubrique<-data[,.N,by=rubrique][order(N,decreasing = T)][,.(rubrique,cumsum(N))]
data2<-data[rubrique %in% data_rubrique[1:40,rubrique]]
data_ministre<-data2[,.N,by=.(groupe.developpe,ministre)]
data_ministre[,q_quintile:=cut(N,breaks = c(1,2,6,max(N)),
                               include.lowest = T,ordered_result = T,
                               dig.lab = 5,right = F)]
data_ministre2<-dcast(data_ministre,ministre~groupe.developpe,value.var = "q_quintile")

data_ministre2[,(names(data_ministre2)[-1]):=lapply(.SD,function(x) ifelse(is.na(x),0,as.character(x))),
               .SDcols=names(data_ministre2)[-1]]

## Analyse multidimensionnelle (ACM)----------------
data_ministreDF<-data.frame(data_ministre2[,lapply(.SD,factor),.SDcols=names(data_ministre2)[-1]],
                            row.names = data_ministre2[,ministre])
data_MCA <- MCA(data_ministreDF,graph = T)

summary(data_MCA)
barplot(data_MCA$eig[,1],main="Eigenvalues",names.arg=1:nrow(data_MCA$eig))

#Etude des 2 premiers axes (règle du coude)  formule de Benzecri 1979 (taux_u=(p/p-1)^2*(u-1/p)^2)
vp.corrige<-sapply(data_MCA$eig[data_MCA$eig[,1]>1/length(data_MCA$call$quali),1],FUN=function(x) (length(data_MCA$call$quali)/(length(data_MCA$call$quali)-1))^2*(x-1/length(data_MCA$call$quali))^2)
print(percent(vp.corrige/sum(vp.corrige)))

## Sorties graphiques----------------

graphMCA(data_MCA,axes=c(1,2),cos2min = 0.3,cos2min.ind = 0.3,jit=0.05,alpha=0.5,
                 titre = "",sousTitre = "")
