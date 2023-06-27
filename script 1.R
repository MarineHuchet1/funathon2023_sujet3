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





prop.table(table(actphys_sedent$transport_personnel))
prop.table(table(actphys_sedent$transport_ecole))
#moyens de tranport différents

ggplot(data=actphys_sedent,aes(x=tv_duree))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")


mean(actphys_sedent$tv_duree,na.rm=TRUE)


ponderations<-description_indiv %>%
  select(NOIND, pond_indiv_adu_pop2, pond_indiv_enf_pop1)

actphys_sedent<-actphys_sedent %>%
  left_join(ponderations)




prop.table(table(actphys_sedent$transport_personnel))
prop.table(table(actphys_sedent$transport_ecole))
#moyens de tranport différents

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

ggplot(data=actphys_adultes,aes(x=ordi_duree))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")





