
####### CHARGEMENT ET MISE EN FORME DU JDD #####
library(readxl)
PE <- read_excel("C:/git/Grand_lieu/DATA/PE_new.xlsx",col_names = TRUE)
library(data.table)
setnames(PE,"ESPECE","CODE")
PE$CODE <- as.factor(PE$CODE)#code CRBPO des esp
PE$SITE <- as.factor(PE$SITE)# Point d'ecoute
PE$ANNEE_txt <- as.factor(PE$ANNEE)#annee en facteur 
PE$annee_sc = scale(PE$ANNEE)#annee en numerique et centre reduite
summary(PE)

####### PRE TRAITEMENT DU JDD ######
#### a) Especes rares #### 
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x >0)) ; occ_esp #occurence de chaque esp
rare_species_50 <- names(occ_esp[occ_esp < 50]) ; rare_species_50# liste des esp rares
###### Filtrer le jeu de données pour exclure les espèces rares :
PE2 <- PE[!(PE$CODE %in% rare_species_50),]
head(PE2) ; summary(PE2)


####### autres jdd
library(readxl)
info_esp <- read_excel("C:/git/Grand_lieu/DATA/info_especes.xlsx",
                       col_names = TRUE)
pod_site <- read_excel("C:/git/Grand_lieu/DATA/pod_site3.xlsx",
                       col_names = TRUE)#mettre les donnees habitats 
n_eau <- read_excel("C:/git/Grand_lieu/DATA/table_niveau_eau.xlsx",
                    col_names = TRUE)
meteo <- read_excel("C:/git/Grand_lieu/DATA/meteo_gl_final.xlsx",
                    col_names = TRUE)

##### FAIRE UNE ACP POUR EVALUER LA REPARTITION DES POINTS D'ECOUTE #############

pod_afc <- subset(pod_site, select = -c(derangement,LAT, LON))
summary(pod_afc)
pod_afc2 <- pod_afc ; summary(pod_afc2)
pod_afc2$ZI <- as.numeric(pod_afc2$ZI)
pod_afc2$arbres <- as.numeric(pod_afc2$arbres)
pod_afc2$buissons <- as.numeric(pod_afc2$buissons)
pod_afc2$aqua <- as.numeric(pod_afc2$aqua)
pod_afc2$urbain <- as.numeric(pod_afc2$urbain)

#changement de la variable protec en numerique a 3 niveaux de protection 
# 0 = aucune / 1 = faible / 2 = fort
pod_afc2$protec <- ifelse(pod_afc2$protec == "RNN", "2",
                         ifelse(pod_afc2$protec == "aucune", "0",
                                ifelse(pod_afc2$protec == "RNR", "2",
                                       ifelse(pod_afc2$protec == "site inscrit", "1",
                                              ifelse(pod_afc2$protec == "site classe", "1", pod_afc2$protec)))))
pod_afc2$protec <- as.numeric(pod_afc2$protec)

pod_afc3 <- subset(pod_afc2, select = -c(Site, ZSC))


#ANALYSES
#1ere version
library(FactoMineR)
afc_result <- PCA(pod_afc3)
plotellipses(afc_result)
#2eme version 
library(factoextra)
fviz_pca_biplot(afc_result, geom = "point", 
                addEllipses = TRUE, label = "none")

