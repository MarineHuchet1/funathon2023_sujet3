# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}


library(aws.s3)
library(dplyr)
library(readr)


bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"


description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)


# Import des librairies
library(ggplot2)
library(ggcorrplot)
library(sf)

# Option d'affichage
options(dplyr.width = Inf)
options(repr.plot.width=20, repr.plot.height=10)



ponderations<-description_indiv %>%
  select(NOIND, pond_indiv_adu_pop2, pond_indiv_enf_pop1)

actphys_sedent<-actphys_sedent %>%
  left_join(ponderations)



actphys_enfants<-actphys_sedent[actphys_sedent$POPULATION=="Pop1 Individu",] 
actphys_adultes<-actphys_sedent[actphys_sedent$POPULATION=="Pop2 Individu",] 


prop.table(table(actphys_sedent$transport_personnel))
prop.table(table(actphys_sedent$transport_ecole))
#moyens de tranport différents

ggplot(data=actphys_sedent,aes(x=tv_duree))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")


mean(actphys_sedent$tv_duree,na.rm=TRUE)




prop.table(table(actphys_sedent$transport_personnel))
prop.table(table(actphys_sedent$transport_ecole))


ggplot(data=actphys_sedent,aes(x=tv_duree, fill=factor(POPULATION)))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")


ggplot(data=actphys_sedent,aes(x=jvideo_duree))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")



ggplot(data=actphys_sedent,aes(x=club_nbjours, fill= factor(POPULATION)))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")
prop.table(table(actphys_sedent$club_nbjours))
table(actphys_sedent$club_nbjours,actphys_sedent$POPULATION)


mean(actphys_sedent$tv_duree,na.rm = TRUE)
mean(actphys_sedent$tv_duree[actphys_sedent$POPULATION=="Pop1 Individu"],na.rm = TRUE)
mean(actphys_sedent$tv_duree[actphys_sedent$POPULATION=="Pop2 Individu"],na.rm = TRUE)
#les adultes regardent en moyenne plus la télé
 
 
 median(actphys_sedent$tv_duree[actphys_sedent$POPULATION=="Pop1 Individu"],na.rm = TRUE)
 median(actphys_sedent$tv_duree[actphys_sedent$POPULATION=="Pop2 Individu"],na.rm = TRUE)

 
ggplot(data=actphys_sedent,aes(x=activite_moderee_nbjours, fill= factor(POPULATION)))+
   geom_histogram(binwidth=1,color="grey",fill="lightblue")
 
 
actphys_enfants<-actphys_sedent[actphys_sedent$POPULATION=="Pop1 Individu",] 
actphys_adultes<-actphys_sedent[actphys_sedent$POPULATION=="Pop2 Individu",] 


ggplot(data=actphys_enfants,aes(x=ordi_duree))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")

ggplot(data=actphys_enfants,aes(x=activite_musculation_nbjours))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")
prop.table(table(actphys_enfants$enfant_actif))


ggplot(data=actphys_sedent,aes(x=sedentarite_duree))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")



actphys_sedent$nap<-as.factor(actphys_sedent$nap)

ggplot(actphys_sedent)+aes(x=nap)+geom_bar(aes(fill=nap))

ggplot(actphys_adultes)+aes(x=nap)+geom_bar(aes(fill=nap))
ggplot(actphys_enfants)+aes(x=nap)+geom_bar(aes(fill=nap))


prop.table(table(actphys_sedent$nap, actphys_sedent$POPULATION))



ggplot(actphys_sedent, aes(x = POPULATION, y= tv_duree, color=POPULATION))+ geom_boxplot()


ggplot(data=actphys_adultes,aes(x=activite_total_score))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")

ggplot(actphys_enfants)+aes(x=profil_activite)+geom_bar(aes(fill=profil_activite))


ggplot(data=actphys_adultes,aes(x=activite_total_score,fill=profil_activite))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")



ggplot(data=actphys_adultes,aes(x=profil_activite, fill=nap))+geom_bar(stat="identity")

barplot <- function(variable){
  ggplot(actphys_sedent) +
    aes(x = factor(eval(parse(text = variable))), fill = factor(POPULATION)) +
    geom_bar(position = "fill") +
    xlab(variable) +
    ylab("Proportion") +
    labs(fill = "Population") +
    scale_y_continuous(labels = scales::percent) +
    theme_classic() +
    scale_fill_brewer()
} 

barplot("transport_personnel")


ggplot(actphys_sedent) +
    aes(x = factor(eval(parse(text = variable))), fill = factor(POPULATION)) +
    geom_bar(position = "fill") +
    xlab(variable) +
    ylab("Proportion") +
    labs(fill = "Population") +
    scale_y_continuous(labels = scales::percent) +
    theme_classic() +
    scale_fill_brewer()
} 


summary(actphys_adultes)
colnames(actphys_adultes)
actphys_adultes_clust<-actphys_adultes %>%
  select(3,6,8,10,24,84,85,86,87,88,89,90)




library(FactoMineR)
library(factoextra)
library(cluster)

# Méthode du coude
elbow_method <- fviz_nbclust(actphys_adultes_clust, kmeans, method = "wss", k.max = 10)
elbow_method


res_acp <- PCA(actphys_adultes_clust)
hc<-HCPC(res_acp,nb.clust = 3)

actphys_adultes_clust<-cbind(actphys_adultes_clust,hc$data.clust$clust)


coord<-res_acp$ind$coord
k_moyennes <- kmeans(coord, centers = 3)

actphys_adultes_clust<- cbind(actphys_adultes_clust,k_moyennes$cluster)

table(actphys_adultes_clust$`k_moyennes$cluster`)


hc$data.clust

mean(actphys_adultes_clust$activite_total_score[actphys_adultes_clust$`k_moyennes$cluster`==1],na.rm = TRUE)
mean(actphys_adultes_clust$activite_total_score[actphys_adultes_clust$`k_moyennes$cluster`==2],na.rm = TRUE)
mean(actphys_adultes_clust$activite_total_score[actphys_adultes_clust$`k_moyennes$cluster`==3],na.rm = TRUE)



prop.table(table(actphys_adultes_clust$nap,actphys_adultes_clust$`k_moyennes$cluster`),margin=2)

#le groupe 2 semble plus sédentaire, et moins d'activité physique, le groupe 1 est le plus actif



