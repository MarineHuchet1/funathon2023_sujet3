
library(tidyr)
ponderations2<-description_indiv %>%
  select(NOIND, pond_indiv_adu_pop2) %>%
drop_na()


ponderations3<-description_indiv %>%
  select(NOIND, pond_indiv_enf_pop2) %>%
  drop_na()


habitudes_indiv_adultes<-right_join(habitudes_indiv,ponderations2)
habitudes_indiv_enfants<-right_join(habitudes_indiv,ponderations3)








table(habitudes_indiv_adultes$etiquette_ingredients,habitudes_indiv_adultes$etiquette_contenu_nutri)
chisq.test(table(habitudes_indiv_adultes$etiquette_ingredients,habitudes_indiv_adultes$etiquette_contenu_nutri))


prop.table(table(habitudes_indiv_adultes$lieu_repas_midi))


table(habitudes_indiv_enfants$conso_plats_faits_maison,habitudes_indiv_enfants$jardin_potager)




#conso viande et bio

chi_test<-function(variable1, variable2){
  table(variable1,variable2)%>%
    chisq.test()
}

chi_test(habitudes_indiv$table_ketchup, habitudes_indiv$conso_barbecue_printps_ete_freq)

table(habitudes_indiv$table_ketchup, habitudes_indiv$conso_barbecue_printps_ete_freq)



#nombre de cigarettes fumées par jour

ggplot(data=description_indiv, aes(x=imc))+
  geom_histogram(binwidth=1, color="grey",fill="lightblue")+
  labs(title="Histogramme des IMC",
       x="IMC",
       y="nombre d'individus")

#fréquence des PCS

#recodage des PCS
description_indiv<-description_indiv %>%
  mutate(PCS=case_when(PCS_8cl_PR==1 ~ "Employé",
                       PCS_8cl_PR==2 ~ "Ouvrier",
                       PCS_8cl_PR==3 ~ "Agriculteur",
                       PCS_8cl_PR==4 ~ "Artisan, commerçant, chef d'entreprise",
                       PCS_8cl_PR==5 ~ "Profession intermédiaire",
                       PCS_8cl_PR==6 ~ "Cadre, profession libérale",
                       PCS_8cl_PR==7 ~ "Retraité, ancien actif",
                       PCS_8cl_PR==8 ~ "Autre inactif",
                       PCS_8cl_PR==9 ~ "Refus",
                       PCS_8cl_PR==10 ~ "Ne sait pas"))

# Tableau des fréquences de chaque PCS
counts_PCS<-description_indiv %>%
  group_by(PCS) %>%
  summarise(n=n())
#graphique en barres horizontales
ggplot(data = counts_PCS, aes(x=PCS, y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+
  labs(title="Histogramme des PCS",
       x="PCS",
       y="nombre d'individus")





