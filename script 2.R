
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


#revenus mensuel du foyer par uc et niveau diplôme personne référente

# Recodage des niveaux de diplôme

description_indiv <- description_indiv %>% mutate(categorie_diplome=case_when(diplome_interv==1 ~ "Aucun diplôme, n'a jamais été scolarisé",
                                                                              diplome_interv==2 ~ "Aucun diplôme, scolarité s'est arrêtée à l'école primaire",
                                                                              diplome_interv==3 ~ "Aucun diplôme, scolarité s'est arrêtée au collège",
                                                                              diplome_interv==4 ~ "Aucun diplôme, scolarité s'est arrêtée au delà du collège",
                                                                              diplome_interv==5 ~ "Aucun diplôme, sans précision",
                                                                              diplome_interv==6 ~ "CEP",
                                                                              diplome_interv==7 ~ "CAP, BEP, BEPC, brevet élémentaire, brevet de compagnon",
                                                                              diplome_interv==8 ~ "Baccalauréat technologique ou professionnel,\nBrevet professionnel ou de technicien,\nBEA, BEC, BEI, BEH, capacité en droit",
                                                                              diplome_interv==9 ~ "Baccalauréat général",
                                                                              diplome_interv==10 ~ "Diplôme de 1er cycle universitaire (Bac +3, licence),\nBTS, DUT, DEST, DEUG, diplôme des professions\nsociales ou de la santé, d'infirmier",
                                                                              diplome_interv==11 ~ "Diplôme de 2ème cycle universitaire (Bac+4, Bac+5),\nMaster, Maîtrise, diplôme d'ingénieur,\nd'une grande école",
                                                                              diplome_interv==12 ~ "Diplôme de 3ème cycle universitaire (>Bac+5, doctorat),\ndiplôme de vétérinaire, médecin, pharmacien",
                                                                              diplome_interv==13 ~ "Refus",
                                                                              diplome_interv==14 ~ "Ne sait pas"))

revenu_par_diplome<-description_indiv %>%
  group_by(categorie_diplome) %>%
  summarise(revenu_moyen=mean(RUC_4cl,na.rm=TRUE))


ggplot(revenu_par_diplome, aes(x=categorie_diplome, y=revenu_moyen))+
         geom_histogram(stat="identity")+
         coord_flip()+
         labs(title = "Histograme du revenu par UC moyen selon le niveau de diplome",
              x= "niveau de diplome",
              y= "revenu par uc moyen")


#score d'insécurité alimentaire par revenu
description_indiv<-description_indiv %>%
  mutate(categorie_ruc=case_when(RUC_4cl==1 ~ "<900 €/mois/UC",
         RUC_4cl==2 ~ "[900-1 340[ €/mois/UC",
         RUC_4cl==3 ~ "[1 340-1 850[ €/mois/U",
         RUC_4cl==4 ~ ">=1 850 €/mois/UC"))

insecurite_par_revenu<-description_indiv %>%
  group_by(categorie_ruc) %>%
  summarise(score_moyen=mean(IA_score,na.rm = TRUE))

ggplot(insecurite_par_revenu, aes(x=categorie_ruc, y=score_moyen)) +
  geom_histogram(stat="identity")+
  coord_flip()+
  labs(title="score d'insécutité alimentaire moyen par catégorie de revenu",
       x="revenu",
       y= "score d'insécurité alimentaire ")


description_x_habitudes <- description_indiv %>% left_join(habitudes_indiv,by="NOIND")
df_num <- description_x_habitudes %>% select(where(is.numeric))

df_num<-df_num %>%
  select(c("revenu","imc","agglo_5cl", "tage_PS", "IA_score", "regime_duree_sem", "poidsmax", "poidsmin"))

matrice_correlation <- model.matrix(~0+., data=df_num) %>% 
  cor(use="pairwise.complete.obs")
       

matrice_correlation %>%   
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=7)



df_num2 <- description_x_habitudes %>% select(where(is.numeric))

df_num2<-df_num2 %>%
  select(c("collation_freq","revenu","imc", "tage_PS", "IA_score", "regime_duree_sem", "poidsmax", "poidsmin"))

matrice_correlation2 <- model.matrix(~0+., data=df_num2) %>% 
  cor(use="pairwise.complete.obs")


matrice_correlation2 %>%   
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=5)




n_rows <- nrow(habitudes_indiv)
n_cols <- ncol(habitudes_indiv)
print(paste("Nombre de lignes :",n_rows))
print(paste("Nombre de colonnes :",n_cols))
head(habitudes_indiv, 3)



sum(!is.na(habitudes_indiv))/(sum(is.na(habitudes_indiv)) + sum(!is.na(habitudes_indiv)))   


summary(habitudes_indiv)

habitudes_indiv_clustering<-habitudes_indiv %>%
  select_if(is.numeric)%>%
  select(-c("NOIND","periode_reference"))%>%
  select_if(function(col) any(!is.na(col))) %>%
  select_if(function(col) length(unique(col)) > 1)

Mode <- function(x) {
  non_missing_values <- x[!is.na(x)]
  ux <- unique(non_missing_values)
  ux[which.max(tabulate(match(non_missing_values, ux)))]
}

habitudes_indiv_clustering_2<-habitudes_indiv_clustering %>%
  ifelse()
  
  
  
  
  
