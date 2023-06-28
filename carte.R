url <- "https://minio.lab.sspcloud.fr/projet-cartiflette/diffusion/shapefiles-test1/year%3D2022/administrative_level%3DREGION/crs%3D4326/FRANCE_ENTIERE%3Dmetropole/vectorfile_format%3D%27geojson%27/provider%3D%27IGN%27/source%3D%27EXPRESS-COG-CARTO-TERRITOIRE%27/raw.geojson"
region <- sf::st_read(url)

# Passons le fonds de carte dans le système de coordonnées de référence utilisé pour la FRance, Lambert 93 (code : 2154) au lieu de WGS 84
region <- region %>% st_transform(2154)

# Représentons les contours de notre fond de carte
plot(st_geometry(region))
region$NOM_M
region <- region %>% mutate(NOM_M=ifelse(NOM_M=="CORSE", "PROVENCE-ALPES-COTE D'AZUR", NOM_M))


description_x_fpq = left_join(description_indiv, fpq, by="NOIND")
# Recodage de la variable région pour avoir les mêmes noms que dans notre fond de carte

description_x_fpq <- description_x_fpq %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
                                                                          region_adm_12cl==2 ~ "NORMANDIE",
                                                                          region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
                                                                          region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
                                                                          region_adm_12cl==5 ~ "BRETAGNE",
                                                                          region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
                                                                          region_adm_12cl==7 ~ "GRAND EST",
                                                                          region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
                                                                          region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
                                                                          region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
                                                                          region_adm_12cl==11 ~ "OCCITANIE",
#fréquence de conso d'apéritifs                                                                          region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",))

aperitif <- description_x_fpq %>% 
  group_by(region_recode) %>% 
  summarise(freq_conso_aperitif_moyenne=mean(BA_aperitif_freq_M,na.rm=TRUE))
aperitif

region_inca <- left_join(region,aperitif,by=c("NOM_M"="region_recode"))

ggplot(data=region_inca) +
  geom_sf(aes(fill=freq_conso_aperitif_moyenne)) +
  scale_fill_continuous(low="yellow", high="red",name="Fréquence de consommation d'apéritifs ou alcools forts en jours par mois en moyenne") +
  labs(title="Fréquence de consomation d'apéritifs ou alcools forts (jours/mois) moyenne par région")




#proportion de végérariens                                                                          region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",))

végétariens <- description_x_fpq %>% 
  group_by(region_recode) %>% 
  summarise(prop_vegetariens=mean(veget_viande,na.rm=TRUE))
végétariens

region_inca <- left_join(region,végétariens,by=c("NOM_M"="region_recode"))

ggplot(data=region_inca) +
  geom_sf(aes(fill=prop_vegetariens)) +
  scale_fill_continuous(low="yellow", high="red",name="Part de végétariens ") +
  labs(title="Part de végérariens par région")





