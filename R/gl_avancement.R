######################
#
#
#             PARTIE 1 - CREATION DU JDD 
#
#
#####################
############## 1 - CHARGEMENT DU JDD BRUT : pod ########


#setwd("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA")
library(readxl)
pod <- read_excel("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/ANALYSES/DATA/dataPE.xlsx",
                  col_names = TRUE)
pod$Site <- ifelse(pod$Site == "Pointe ou friche de l'Arsangle", "Pointe de l'Arsangle", pod$Site)

#VISUALISER ET RESUME DU JDD
#View(pod) #voir
summary(pod) #resume
dim(pod)# dimensions : nbre de lignes puis de colonnes



############## 2 - CREATION DU JDD EXPLOITABLE SUR R - une ligne par obs #######
####### a - Nettoyer lignes et colonnes et renommer variables : pod2 #####


pod2<-subset(pod, Site != "TOTAL")#subset pour tout garder sauf les lignes total (avec !=)
pod2 <- pod2[,-c(3:4)]#enlever lat et long en colonne 3 et 4 
pod2[is.na(pod2)] <- 0#remplacer les na par des 0 
colnames(pod2)[1:2] <- c("ANNEE","SITE")#renommer les colonnes
dim(pod2)#dimensions : on a 20 lignes en moins et deux colonnes en moins? alors c est ok
#View(pod2)


####### b - Basculer le jdd en 1 ligne = 1 observation : PE #####


#Fonction reshape pour restructurer le jdd
#melt permet de passer des colonnes en lignes, en creant une nouvelle variable a partir de ces lignes 
PE <- reshape2::melt(pod2,id=c("ANNEE","SITE"),value.name = "ABONDANCE")
colnames(PE)[3] <- "ESPECE"


############## 3 - CREATION DES JDD -----------------
####### a - Points d ecoute, GPS et caracteristiques : pod_site #####

#Mise en forme du jdd
pod_site <- unique(pod[,2:4])
pod_site<-subset(pod_site, Site != "TOTAL")#garder tout sauf ligne total 
#View(pod_site) 
dim(pod_site)#119 lignes = 119 points d ecoute ---> c'est ok
#Suppression des caracteres speciaux dans le nom des lieux : 
pod_site$Site <- gsub("[����]", "e", pod_site$Site, ignore.case = TRUE)
pod_site$Site <- gsub("[��]", "a", pod_site$Site, ignore.case = TRUE)
#ignore.case permet d'ignorer MAJ/min sur les lettres 

#STATUT DE PROTECTION 
#chargement
library(readr)
PE_RNN <- read_csv("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/CARTOGRAPHIE/WORK/table attributaire/ta_PE_RNN.csv")
PE_RNR <- read_csv("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/CARTOGRAPHIE/WORK/table attributaire/ta_PE_RNR.csv")
PE_ZPS <- read_csv("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/CARTOGRAPHIE/WORK/table attributaire/ta_PE_ZPS.csv")
PE_SITE_CLASS <- read_csv("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/CARTOGRAPHIE/WORK/table attributaire/ta_PE_SITE_CLASSE.csv")
PE_SITE_INS <- read_csv("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/CARTOGRAPHIE/WORK/table attributaire/ta_PE_SITE_INSCRIT.csv")
#ils ont tous ete crees sur QGIS 
#Attention, n est pas reproductible si la personne n a pas ces fichiers ! 
#suppression les colonnes inutiles : 
PE_RNN <- subset(PE_RNN, select = c(Site, Nom))#lieu et statut seulemt
PE_RNR <- subset(PE_RNR, select = c(Site,ID_LOCAL)) #RNR191
PE_ZPS <- subset(PE_ZPS, select = c(Site, SITECODE))# FR5210008
PE_SITE_CLASS <- subset(PE_SITE_CLASS, select = c(Site, id_regiona))#4449
PE_SITE_INS <- subset(PE_SITE_INS, select = c(Site, id_entite))#b
dim(pod_site)

#integrer chaque protection dans pod_site :
pod_site <- merge(pod_site,PE_RNN, all.x = TRUE, by = "Site")
pod_site <- merge(pod_site,PE_RNR, all.x = TRUE, by = "Site")
pod_site <- merge(pod_site,PE_ZPS, all.x = TRUE, by = "Site")
pod_site <- merge(pod_site,PE_SITE_CLASS, all.x = TRUE, by = "Site")
pod_site <- merge(pod_site,PE_SITE_INS, all.x = TRUE, by = "Site")

#Fusionner les colonnes ensemble 
pod_site$protec <- paste0(ifelse(is.na(pod_site$ID_LOCAL), "", "RNR"),
                          ifelse(is.na(pod_site$SITECODE), "", "ZPS"),
                          ifelse(is.na(pod_site$Nom), "", "RNN"), 
                          ifelse(is.na(pod_site$id_regiona), "", "site classe"),
                          ifelse(is.na(pod_site$id_entite), "", "site inscrit"))
pod_site$protec[pod_site$protec == ""] <- "aucune"
pod_site$protec <-as.factor(pod_site$protec)
pod_site <- subset(pod_site, select = -c(Nom, ID_LOCAL, SITECODE,id_regiona, id_entite))
table(pod_site$protec)
barplot(table(pod_site$protec),
        main = "repartition des statut de protection des points d ecoute")
#Jai fait le choix de ne faire qu'une variable avec le statut de protection 
#le plus fort, ex: ceux en RNN sont aussi en ZPS mais ZPS moins fort que RNN 

rm(pod, pod2, PE_RNN, PE_RNR, PE_ZPS, PE_SITE_CLASS, PE_SITE_INS)#suppr les inutiles

### Attention il faut refaire la classification et enlever ZPS qui n'a pas grand interet dans la protection des oiseaux 

####### b - Caracteristique de l'habitat : ######

#Chargement jdd 
library(readxl)
hab <- read_excel("C:/git/Grand_lieu/DATA/habitats.xlsx",
                  col_names = TRUE)
hab$Site <- gsub("[����]", "e", hab$Site, ignore.case = TRUE)
hab$Site <- gsub("[��]", "a", hab$Site, ignore.case = TRUE)
pod_site <- merge(pod_site, hab, all.x = TRUE, by = "Site")


####### c - Les noms vernaculaires des oiseaux : code_crbpo #####

library(readxl)
code_crbpo <- read_excel("C:/git/Grand_lieu/DATA/noms_vernaculaires.xlsx", col_names = FALSE)#chargement du jdd 
colnames(code_crbpo) <- c("ESPECE", "NOM_FR_BIRD")#nom des variables dans la matrice
#View(code_crbpo)#visualisation du jdd
dim(PE)
#jonction du jdd principal avec code_crbpo pour avoir les noms vernaculaires dans le jdd 
PE <- merge(PE,code_crbpo, all.x = TRUE, by = "ESPECE")
dim(PE)#verif que merge fonctionne / 1 colonne en + = c'est ok 
#Pour reprendre l ordre des colonnes : 
library(dplyr)
PE <-select(PE, ANNEE, SITE, ESPECE, NOM_FR_BIRD,ABONDANCE)#data puis ordre des colonnes
#View(PE)

####### d - La liste des especes en comptage exaustif : liste #######
#La liste des comptages exaustifs a ete faite a la main 
#Pourquoi il n'y a pas les autres rapaces dans ce mode de comptage ? 
library(readr)
liste <- read_csv("C:/git/Grand_lieu/DATA/liste_comptage_exaustif.csv"); colnames(liste)<- "code"
summary(liste)

#Inclure le type de comptage dans PE : 
liste$type <- 1 
code_crbpo2 <-merge(code_crbpo, liste, by.x = "ESPECE", by.y = "code", all.x = TRUE )
code_crbpo2[is.na(code_crbpo2)] <- 0 ; code_crbpo2 <- code_crbpo2[-2]
PE <-merge(PE,code_crbpo2, by = "ESPECE", all.x = TRUE )
#le bricolage fonctionne 

#1 = le comptage est exaustif, tous les individus sont comptes 
#0 = le comptage n'est pas exaustif et se fait par le nbre de males chanteurs 
#les rapaces ne sont pas en exaustif ?
summary(code_crbpo2)
code_crbpo2$ESPECE <-as.factor(code_crbpo2$ESPECE)
sum(code_crbpo2$type)#le nombre d'especes avec un protocole exaustif : 
(sum(code_crbpo2$type)/nrow(code_crbpo2))*100# pourcentage dans le jdd 
rm(code_crbpo2)




####### e - La famille et l ordre de chaque espece : info_esp et PE_info #####


library(readr)
info_esp_complet <- read_csv("C:/git/Grand_lieu/DATA/espece.csv")
#View(info_esp_complet)
summary(info_esp_complet)
#Faire la correction du code crbpo diff entre les jdd :
info_esp_complet$code_crbpo <- ifelse(info_esp_complet$pk_species == "LANSEN" , "LANSER" , info_esp_complet$pk_species)
#fonction ifelse = ptite boucle avec Si ... alors ... sinon ...) 


info_esp <- merge(code_crbpo, info_esp_complet, by.x = "ESPECE", by.y = "code_crbpo")
#niquel, il faut maintenant que je supprime pk_species qui a le mauvais code crbpo

rm(info_esp_complet)#supprimer info_esp_complet qui ne sert plus a rien 


####### f - Le poids des esp, leur regime alimentaire et autre : geb #####

#chargement du jdd avec les poids moyen des esp 
#attention triche : je l ai converti en csv 

geb <- read.csv2("C:/git/Grand_lieu/DATA/geb12127-sup-0002-ap.csv",skip = 6)# skip pour sauter les premieres lignes 
geb$code <- casefold(geb$code, upper=T)#permet de tout mettre en majuscule (pour merge apres)
#/!\ CODE CRBPO A CORRIGER /// meme oiseau == autre nom 
geb$code_crbpo <- ifelse(geb$code == "LANSEN" , "LANSER" , geb$code) # maj des codes crbpo
#expl de la fontion : SI (...condition... , ALORS ... , SINON ...)
#geb <- geb[,c(2:3)]#enlever les colonnes non desirees
summary(geb)
#View(geb)
#geb$POIDS <- as.numeric(geb$POIDS) #ne pas faire pour l'instant car introduit des NA

####### g - Gradient de specialisation : ind_fonction  #######

library(readr)
ind_fonction<- read_csv("C:/git/Grand_lieu/DATA/espece_indicateur_fonctionel.csv")
#View(ind_fonction)
ind_fonction$pk_species<- ifelse(ind_fonction$pk_species == "LANSEN" , "LANSER" ,
                                 ind_fonction$pk_species)
#j ai remplace directement dans la variable pk_species 



####### h - La migration des oiseaux : HWI ##### 

setwd("C:/git/Grand_lieu/DATA")
library(readxl)
HWI_complet <- read_excel("Dataset_HWI_2020-04-10_shread_2020_naturecom.xlsx")
colnames(HWI_complet)[11:13] <- c("migration_1","migration_2","migration_3")
colnames(HWI_complet)[3] <- "species_name"
summary(HWI_complet)
HWI_complet$species_name <- ifelse(HWI_complet$species_name == "Coloeus monedula", 
                                   "Corvus monedula", HWI_complet$species_name) # nom diff pour choucas des tours 

#Extraire les especes de notre protocole:  
#On utilise les noms scienti car pas de code crbpo dans HWI_complet
scien_name <- info_esp[,1:5]
scien_name <- scien_name[,-c(2:3)]
#View(scien_name)
HWI <- merge(scien_name,HWI_complet, all.x = TRUE, by.x = "scientific_name",
             by.y = "species_name")# nbre de lignes = nbre d especes ? c ok 
summary(HWI)
HWI$Diet <- as.factor(HWI$Diet)
HWI$Territoriality <- as.factor(HWI$Territoriality)
barplot(table(HWI$Diet), main = "Répartition des régimes alimentaires des oiseaux communs de Grand Lieu", 
        xlab = "régime alimentaire", ylab = "Nombre d'espèces")
barplot(table(HWI$Territoriality), main = "Classement des especes territoriales de Grand Lieu", 
        xlab = "type de territorialité", ylab = "Nombre d'espèces")
table(HWI$migration_1)
table(HWI$migration_2) 
#1 = sédentaire 
#2 = partiellement migrateur 
#3 = totalement migrateur 
table(HWI$migration_3)

barplot(table(HWI$migration_2), main = "repartition des especes migratrices sur gl")

rm(scien_name, HWI_complet)#supprimer HWI_complet qui ne sert plus a rien 


####### i - La meteo de grand lieu : meteo_gl #######

library(readr) 
meteo_gl<- read_csv2("C:/git/Grand_lieu/DATA/AnnualData19602021.csv")
#View(meteo_gl)
summary(meteo_gl)
dim(meteo_gl)
meteo_gl$RR <- as.numeric(meteo_gl$RR) #mettre en numerique ce dont on a besoin 
meteo_gl$Date_m <- as.numeric(meteo_gl$Date_m)
#Supprimer la valeur de NA ? Ici on ne va pas le faire (je garde le code au cas ou)
#meteo_gl <- meteo_gl[!is.na(meteo_gl$RR),]#drop_na(nom_de_la_colonne) #autre facon  


#Mise en forme du jdd meteo: --> valeur annuelle 

#Temperature moyenne par an :
tm_y <- aggregate(TM ~ Date_y, data = meteo_gl, mean) ; tm_y
#permet de faire la moyenne des TM en fonction de l'annee
#autre methode possible avec fonction ave()


#Precipitation moyenne par jour et par an : #est-ce-que c'est pertinent ?
rr_y <- aggregate(RR ~ Date_y, data = meteo_gl, mean, na.rm = TRUE) ; rr_y
# na.rm = TRUE pour ne pas prendre en compte les NA
 

#Somme des precipitations de l'annee :
rr_y_sum <- aggregate(RR ~ Date_y, data = meteo_gl, sum, na.rm = TRUE) ; rr_y_sum
colnames(rr_y_sum)[2] <-'RR_sum'


meteo_y <- merge(tm_y,rr_y, all.x = TRUE, by = "Date_y")
meteo_y <-merge(meteo_y,rr_y_sum,all.x = T, by = "Date_y")


library(data.table)
DT_meteo <- meteo_gl#faire une copie du jdd
setDT(DT_meteo)#a quoi ca sert deja ?

dt_y_printemps <- DT_meteo[Date_m %in% c(4,5,6),.(RR_sum_spring = sum(RR,na.rm = TRUE), TM_spring = mean(TM,na.rm=TRUE)),by = Date_y]
#ecriture particuliere de data.table qui permet de creer des moyennes avec des conditions 

meteo_y <-merge(meteo_y,dt_y_printemps, all.x = T, by = "Date_y")


###Creation de la variable "nombre de jours de gel dans l'hiver precedent" : 

#Nouvelle variable qui corrige l'annee pour les mois de octo, nov, dec : 
meteo_gl$fin_hiver <- ifelse(meteo_gl$Date_m %in% c(1:3,10:12),
                             ifelse(meteo_gl$Date_m %in% c(1:4),
                                    meteo_gl$Date_y,meteo_gl$Date_y + 1), NA)
#attention on prend avril en + !! 
#Creation nouvel objet qui contient le nbre de jours de gel :
gel <- aggregate(TM ~ fin_hiver,data = meteo_gl,
                 FUN = function(X) sum(as.numeric(X < 0)));colnames(gel) <-c("Date_y", "j_rude")
#on lui dit de compter le nombre de fois ou les valeurs sont inf à 0 

#Inclure cette nouvelle variable dans le jdd : 
meteo_y <-merge(meteo_y,gel, all.x = T, by = "Date_y")

#Sous jeu de donnees seulement pour les annees de l'etude : 
meteo_y_etude <- subset(meteo_y, Date_y >=2000)


#Langage data.table : Autre methode pour faire la meme chose : 
#DT_meteo[,fin_hiver := ifelse(Date_m %in% c(1:3,10:12),
#                              ifelse(Date_m %in% c(1:3),Date_y,Date_y + 1),
#                             NA)] 
#DTgel <- DT_meteo[,.(nb_gel = sum(as.numeric(TM < 0))),fin_hiver]


###GRAPHIQUES###
library(ggplot2)
library(dplyr)
library(scales)



#Graphique comparaison des temperatures annuelles et printanières 

ggplot(meteo_y_etude, aes(x = Date_y)) +
  geom_point(aes(y = TM, color = "Température annuelle")) +
  geom_line(aes(y = TM, color = "Température annuelle")) +
  geom_point(aes(y = TM_spring, color = "Temperature du printemps")) +
  geom_line(aes(y = TM_spring, color = "Temperature du printemps")) +
  scale_color_manual(values = c("red", "Darkred")) +
  #scale_y_continuous(name = "TM",limits = c(min(meteo_y_etude$TM), max(meteo_y_etude$TM)))+
  labs(x = "Année",y = "temperature (°C)", title = "Température moyenne annuelles et printanières du lac de Grand lieu entre 2000 et 2021")
  #On a une sacre correlation entre les deux variables (logique)

# Graphique comparaison des variables de precipitations 

ggplot(meteo_y_etude, aes(x = Date_y)) +
  geom_point(aes(y = RR_sum_spring, color = "RR print")) +
  geom_line(aes(y = RR_sum_spring, color = "RR print")) +
  geom_point(aes(y = RR_sum, color = "somme")) +
  geom_line(aes(y = RR_sum, color = "somme")) +
  scale_color_manual(values = c("blue", "Darkblue")) +
  #scale_y_continuous(name = "TM",limits = c(min(meteo_y_etude$TM), max(meteo_y_etude$TM)))+
  labs(x = "Année",y = "RR", title = "Precipitations moyenne annuelles et printanières du lac de Grand lieu entre 2000 et 2021")


ggplot(meteo_y_etude, aes(x = Date_y)) +
  geom_point(aes(y = RR, color = "RR jour")) +
  geom_line(aes(y = RR, color = "RR jour")) +
  labs(x = "Année",y = "Précipitations (mm)",title = "Precipitations moyenne par jour du lac de Grand lieu entre 2000 et 2021")
#pas tres pertinent je pense cette variable... 
#j'aimerai que les courbes de variations se superposent
#( chacune dans leur gamme de variation respective) afin de pouvoir les comparer.
#Une variable est en nombre de jours de gel ( entre 0 et 20)
#et l'autre variable est une moyenne des températures qui tourne autour de 15 degres. comment faire ?


#Graphique :
#Des ptits graphiques pour visualiser un peu de tout... 
plot(dt_y_printemps$RR_sum_spring ~ dt_y_printemps$Date_y, type = "b",
     main = "Variation des précipitations du printemps en fonction des ans",
     xlab = "Annees", ylab = "Precipitations(mm)")

plot(dt_y_printemps$TM_spring ~ dt_y_printemps$Date_y, type = "b",
     main = "Variation des temperatures du printemps en fonction des ans",
     xlab = "Annees", ylab = "Temperature (°C)")
#flagrant l'augmentation des temperatures...

plot(meteo_y_etude$j_rude~ meteo_y_etude$Date_y,
     type = "b", main = "Nombre de journées rudes par hiver",
     xlab = "Annee", ylab = "Nb de jours")

#pour supprimer un objet : rm(nom_objet) #pour remove
rm(DT_meteo, dt_y_printemps, gel, rr_y, rr_y_sum, tm_y, meteo_gl, meteo_y)




####### j - Niveaux d'eaux : n_eau  #######



#chargement du jdd 
library(readxl)
niv_eau <- read_excel("C:/git/Grand_lieu/DATA/Cote Lac GL_1958_2022.xlsx", col_names = T)#chargement du jdd 
summary(niv_eau) ; colnames(niv_eau)[1] <- "date"

#mise en forme du jdd
n_eau_prov <- reshape2::melt(niv_eau,id=c("date"),value.name = "hauteur (m)")
#passage colonnes en lignes
library(stringr)
Date <-str_split(n_eau_prov$date, "-", simplify = TRUE) ; colnames(Date) <- c("annee","mois","jour")
#permet de separer la date complete par 3 variables differentes : annee/mois/jour
n_eau_complet<- data.frame(n_eau_prov$date, Date[,2:3] , annee = n_eau_prov$variable, hauteur = n_eau_prov$hauteur)
#la fonction data.frame me permet de "refaire" un jeu de donnees avec les colonnes que je souhaite 
colnames(n_eau_complet)[1] <- "date"
library(dplyr)
n_eau_complet <-select(n_eau_complet, date, annee, mois, jour, hauteur)
#remettre les colonnes dans l'ordre (data puis nom des colonnes dans l'ordre)
#View(PE)
summary(n_eau_complet)#variables sous un mauvais format : 
n_eau_complet$mois <- as.numeric(n_eau_complet$mois)
n_eau_complet$jour <- as.numeric(n_eau_complet$jour)
n_eau_complet$annee <- as.numeric(as.character(n_eau_complet$annee))#passer en character puis en numeric 
#creation du jdd des niveaux d'eaux final --> printemps 2000/2022
n_eau<- subset(n_eau_complet, annee >=2000)# garder les annees sup ou egal à 2000 
n_eau<- subset(n_eau, mois == "4" | mois == "5" | mois == "6" | mois == "7")#garder les mois de spring
summary(n_eau) # ; View(n_eau)

n_eau$date <- as.Date(n_eau$date)#mettre au format date 
library(lubridate)#package pour date
n_eau$jj <- yday(n_eau$date)#transformer en jour julien 
#n_eau <- subset(n_eau, select = -c(date)) #enlever la colonne date qui ne sert plus
head(n_eau)

#On a le jdd n_eau propre, avec la hauteur pour chaque jour
#Sauf que nous, on a besoin d'une seule valeur par an dans notre PE_info 
#On crée donc un indice de crue, 1 annee = 1 valeur

###Creation de l'indice de crue : 

## mauvaise idée de standardisation :-(
# require(data.table)
# setDT(n_eau)
# n_eau[,hauteur_y_med := median(hauteur),by=annee]
# n_eau[,hauteur_sc := hauteur / hauteur_y_med]
# n_eau[,hauteur_jj_med := median(hauteur_sc),by = jj]
# n_eau[,hauteur_anomalie := hauteur_sc - hauteur_jj_med]
# n_eau[,anomalie_y_mean := mean(hauteur_anomalie),by = annee]
# n_eau[,hauteur_anomalie_sc := hauteur_anomalie/anomalie_y_mean]

require(data.table)
#setDT(n_eau)
n_eau2 <- n_eau#copie jdd
n_eau_decal <- n_eau #deuxieme copie jdd
#utilisation data.table
setDT(n_eau_decal)

#Objectif : visualiser de facon quantitative lorsqu'il y a une forte 
#augmentation du niveau d'eau durant le printemps 
#--> Faire une variable "difference" niveau d'eau = ne(n) - ne(n-1) 

#Création des nouvelles variables : 

#Variation sur 24h : diff_jj1 = hauteur(jj) - hauteur(jj_1) 
n_eau_decal[,jj_1:= jj]#jj_1 prend la valeur de jj
n_eau_decal[,jj := jj + 1]#jj prend 1 jour de +
n_eau_decal <- n_eau_decal[,.(jj,jj_1,annee,hauteur)]#garder colonnes importantes
setnames(n_eau_decal,"hauteur","hauteur_j1")#renommer colonnes

n_eau2 <- merge(n_eau2,n_eau_decal,by = c("jj","annee"),all.x = TRUE)
#fusionner les niveaux d'eaux + les niveaux d'eaux 2 jours avant 
n_eau2

#Variation sur 48h : diff_j2 = hauteur(jj) - hauteur(jj_2) 
#meme mecanisme que pour jj_1
n_eau_decal[,jj_2 := jj] 
n_eau_decal[,jj := jj + 2]
n_eau_decal <- n_eau_decal[,.(jj,jj_2,annee,hauteur_j1)]
setnames(n_eau_decal,"hauteur_j1","hauteur_j2")
n_eau2 <- merge(n_eau2,n_eau_decal,by = c("jj","annee"),all.x = TRUE)
n_eau2

#Variation sur 72h : diff_j3 = hauteur(jj) - hauteur(jj_3) 
#meme mecanisme que pour jj_1
n_eau_decal[,jj_3:= jj]
n_eau_decal[,jj := jj + 3]
n_eau_decal <- n_eau_decal[,.(jj,jj_3,annee,hauteur_j2)]
setnames(n_eau_decal,"hauteur_j2","hauteur_j3")
n_eau2 <- merge(n_eau2,n_eau_decal,by = c("jj","annee"),all.x = TRUE)
n_eau2

#Création de la variable diff_jn, la difference de hauteur : 
n_eau2[,`:=`(diff_j1 = hauteur - hauteur_j1,#Rappel jj_2 est 2 jours avant jj
             diff_j2 = hauteur - hauteur_j2,
             diff_j3 = hauteur - hauteur_j3)]
#Ne garder seulement les differences supperieures a 0, donc les augmentations 
n_eau2[,`:=`(diff_j1 = ifelse(diff_j1 <0 ,0, diff_j1),
             diff_j2 = ifelse(diff_j2 <0 ,0, diff_j2),
             diff_j3 = ifelse(diff_j3 <0 ,0, diff_j3))]
#Creation de la variable seuil_jn, TRUE/FALSE, est-ce-que l'augmentation du niveau 
#d'eau a franchi la valeur de 1.90, valeur d'innondation des prés-marais ? 
n_eau2[,`:=`(seuil_j1 = hauteur > 1.85 & hauteur_j1 < 1.95 & diff_j1 > 0,
             seuil_j2 = hauteur > 1.85 & hauteur_j2 < 1.95 & diff_j2 > 0,
             seuil_j3 = hauteur > 1.85 & hauteur_j3< 1.95 & diff_j3 > 0)]

# Faire le tableau recap :
med_spring <- aggregate(n_eau$hauteur ~ n_eau$annee, data = n_eau, median) #Hauteur mediane par printemps 
colnames(med_spring)[2] <- "mediane"
mean_spring <- aggregate(n_eau$hauteur ~ n_eau$annee, data = n_eau, mean)#Moyenne par printemps
colnames(mean_spring)[2] <- "moyenne"
sd_spring <- aggregate(n_eau$hauteur ~ n_eau$annee, data = n_eau, sd)#ecart type associé
colnames(sd_spring)[2] <- "sd"
min_spring <- aggregate(n_eau$hauteur ~ n_eau$annee, data = n_eau, min)# hauteur min du printemps
colnames(min_spring)[2] <- "min_hauteur"
max_spring <- aggregate(n_eau$hauteur ~ n_eau$annee, data = n_eau, max)#hauteur max du printemps 
colnames(max_spring)[2] <- "max_hauteur"
diff_j3_max <- aggregate(n_eau2$diff_j3 ~ n_eau2$annee, data = n_eau2, max)
colnames(diff_j3_max)[2] <- "diff_max_j3"
crue_seuil <- aggregate(n_eau2$seuil_j3 ~ n_eau2$annee, data = n_eau2, any)
colnames(crue_seuil)[2] <- "crue_seuil"
somme_seuil <- aggregate(n_eau2$seuil_j3 ~ n_eau2$annee, data = n_eau2, sum)
colnames(somme_seuil)[2] <- "somme_seuil"
haut_max_true <- aggregate(n_eau2$hauteur ~ n_eau2$annee + seuil_j3, data = n_eau2, max) #garder seulement ceux qui sont TRUE 
haut_max_true <- subset(haut_max_true, seuil_j3 == "TRUE") 
haut_max_true <- subset(haut_max_true, select = -c(seuil_j3)) 
colnames(haut_max_true)[2] <- "haut_max_true"
diff_max_true <- aggregate(n_eau2$diff_j3 ~ n_eau2$annee + seuil_j3, data = n_eau2, max)
diff_max_true <- subset(diff_max_true, seuil_j3 == "TRUE")
diff_max_true <- subset(diff_max_true, select = -c(seuil_j3)) 
colnames(diff_max_true)[2] <- "diff_max_true"
#On fusionne tout ça maintenant : 
table_niveau_eau <- merge(mean_spring,med_spring, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau$annee")
table_niveau_eau <- merge(table_niveau_eau,sd_spring, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau$annee")
table_niveau_eau <- merge(table_niveau_eau,min_spring, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau$annee")
table_niveau_eau <- merge(table_niveau_eau,max_spring, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau$annee")
table_niveau_eau <- merge(table_niveau_eau,diff_j3_max, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau2$annee")
table_niveau_eau <- merge(table_niveau_eau,crue_seuil, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau2$annee")
table_niveau_eau <- merge(table_niveau_eau,somme_seuil, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau2$annee")
table_niveau_eau <- merge(table_niveau_eau,haut_max_true, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau2$annee")
table_niveau_eau <- merge(table_niveau_eau,diff_max_true, all.x = TRUE, by.x = "n_eau$annee", by.y = "n_eau2$annee")


# creer table hauteur/annee : 
# année / hauteur mediane de l'annee / ecart type de l'annee / min(niveau d'eau) / max(niveau d'eau) / diff max
# la colonne crue seuil (TRUE/FALSE) / Nbre de jours avec TRUE / Hauteur max pour les TRUE / Diff max pour les TRUE  / hauteur d'eau au diff max 
# pour crue seuil, faire any de la colonne seuil pour savoir s'il y a un true dans l'année 

#documentation avec structuration du jeu de données 
#d'ou vient les données, explication etc 


#Graphiques : 


#Exemple de crue 2001 -2002: 
n_eau_2001 <- subset(n_eau, annee == 2001)
n_eau_2002<- subset(n_eau, annee == 2002)

ggplot() +
  geom_line(data = n_eau_2001,aes(x = date, y = hauteur, color = "2001")) +
  geom_line(data = n_eau_2002,aes(x = date, y = hauteur,color = "2002" ) ) +
  #scale_color_manual(values = c("blue", "red")) +
  #scale_y_continuous(name = "TM",limits = c(min(n_eau$TM), max(n_eau$TM)))+
  labs(x = "Date",y = "Hauteur(m)",
       title = "Hauteur du niveau d'eau de Grand lieu", color = "Légende :") +
  theme_bw() 
#la fonction theme() permet de changer l'apparence du graphique, avec size pour la police en encore family 


#Autre exemple de crue 2014-2015: 
n_eau_2014 <- subset(n_eau, annee == 2014)
n_eau_2015<- subset(n_eau, annee == 2015)

ggplot() +
  geom_line(data = n_eau_2014,aes(x = date, y = hauteur, color = "2014")) +
  geom_line(data = n_eau_2015,aes(x = date, y = hauteur,color = "2015" ) ) +
  scale_color_manual(values = c("blue", "red")) +
  #scale_y_continuous(name = "TM",limits = c(min(n_eau$TM), max(n_eau$TM)))+
  labs(x = "Date",y = "Hauteur(m)",
       title = "Hauteur du niveau d'eau de Grand lieu en 2014 et 2015", color = "Légende :") +
  theme_bw() 


#Superposition de toutes les courbes de niveau d'eau sur 20 ans 
ggplot(n_eau, aes(x = date, y = hauteur, group = as.factor(annee), color = as.factor(annee))) +
  geom_line() +# facet_wrap(.~annee,scales = "free_y")
  labs(x = "Date", y = "Hauteur (m)", title = "Hauteur du niveau d'eau sur la période du protocole", color = "Légende :") +
  scale_color_manual(values = rev(rainbow(length(unique(n_eau$annee))))) +
  theme_bw()


#Courbes avec seuil de 1.90  
ggplot(n_eau2, aes(x = date, y = hauteur, group = as.factor(annee),colour = diff_j1)) +
  geom_line() +geom_point(data=subset(n_eau2,diff_j1 > 0),size=0.5)+geom_point(data=subset(n_eau2,seuil_j1),size=1.1,colour = "red")+facet_wrap(.~annee,scales = "free_y")
  labs(x = "Date", y = "Hauteur (m)", title = "Hauteur du niveau d'eau sur la période du protocole", color = "Légende :") +
  scale_color_manual(values = rev(rainbow(length(unique(n_eau$annee))))) +
  theme_bw()
  

library(gridExtra)#package graphique, de meche avec ggplot
# Creer une liste pour stocker les graphiques : 
plots_list <- list()

# Boucle pour creer un graphique pour chaque annee
for (i in 2016:2022) { #pour tous les ans : max(n_eau$annee)
  data_year <- n_eau[n_eau$annee == i,]# filtrer les données pour l'année i
  # Créer le graphique pour l'année i
  graph <- ggplot(data_year, aes(x = date, y = hauteur)) +
    geom_line(color = "blue") +
    labs(x = "Date", y = "Hauteur(m)", title = paste("Niveau d'eau en", i))
  # Ajouter le graphique à la liste
  plots_list[[i-2015]] <- graph
}

# Afficher les graphiques : grid.arrange()
grid.arrange(grobs = plots_list, ncol = 4)#grobs pour afficher une liste
#ncol pour le nbre de colonnes 
#on voit des crues en 2001, 2007, 2008?, 2012, 2013, 2015++, 

#rm(n_eau_prov, Date, niv_eau)#supprimer les jdd intermediaires



####### k - Creation info_especes #############

info_especes <- merge(code_crbpo,info_esp, all.x = TRUE, by = "ESPECE")
 
#Remplir PE_info avec tous les jdd 
info_especes <- merge(info_especes,ind_fonction, all.x = TRUE, by.x = "ESPECE", by.y = "pk_species")
prov <- merge(code_crbpo,geb, all.x = TRUE,  by.x = "ESPECE", by.y = "code")#fusion des deux jdd
#ici, le merge me fait 2 lignes en double et je ne comprends pas pourquoi
#il s'agit des lignes 29 et 81 (corneille noire et marouette de baillon )
prov <- prov[-c(29,81),]# je sais ce nest pas bien, mais je ne comprends pas... 
info_especes<- merge(info_especes,prov, all.x = TRUE, by = "ESPECE")#fusion des deux jdd
info_especes <- merge(info_especes,HWI, all.x = TRUE, by = "ESPECE")#fusion des deux jdd

info_especes <- subset(info_especes, select = - c(NOM_FR_BIRD.y, pk_species, niveau_taxo,class_tax,
                                                  scientific_name.y, scientific_name_2.y, Synonym, Notes))

                                                  
#voir pourquoi NA sur le verdier d'europe ?                                                   
                                                  
#Selectionner les colonnes que l'on veut :
# # info_especes <- subset(info_especes, select = c(, order_tax, family_tax, e.bodymass.g.,
#                                       e.seeds.nuts.grain, e.fruits.frugivory,
#                                       e.vegitative, e.invert, e.fish, ssi, sti_europe,
#                                       stri,Territoriality, Diet, migration_1,
#                                       migration_2, migration_3))





# PE_info <- merge(PE,info_esp, all.x = TRUE, by = "ESPECE")
# #PE_info <- PE_info[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
# #/!\ Attention /!\ 
# #saisi des codes crbpo diff entre les jdd 
# #il faut verifier que tous les codes soient les memes :
# unique(subset(PE_info, is.na(family_tax), select = "ESPECE"))#recherche des mauvais code espece 
# # si egal a 0 alors c est ok 
# 
# #Remplir PE_info avec tous les jdd 
# PE_info <- merge(PE_info,ind_fonction, all.x = TRUE, by.x = "ESPECE", by.y = "pk_species")
# PE_info <- merge(PE_info,geb, all.x = TRUE, by.x = "ESPECE", by.y = "code")#fusion des deux jdd 
# PE_info <- merge(PE_info,HWI, all.x = TRUE, by = "ESPECE")#fusion des deux jdd 
# PE_info <- merge(PE_info,meteo_y_etude, all.x = TRUE, by.x = "ANNEE", by.y = "Date_y")#fusion des deux jdd 
# 
# #Selectionner les colonnes que l'on veut : 
# PE_info <- subset(PE_info, select = c(ESPECE, NOM_FR_BIRD.x, ANNEE, SITE, ABONDANCE,
#                                       type, order_tax, family_tax, e.bodymass.g.,
#                                       e.seeds.nuts.grain, e.fruits.frugivory,
#                                       e.vegitative, e.invert, e.fish, ssi, sti_europe,
#                                       stri,Territoriality, Diet, migration_1, 
#                                       migration_2, migration_3))
# PE_info$SITE <- gsub("[����]", "e", PE_info$SITE, ignore.case = TRUE)#Mettre entre crochets tous les caracteres speciaux 
# PE_info$SITE <- gsub("[��]", "a", PE_info$SITE, ignore.case = TRUE)
# colnames(PE_info)[1] <- "CODE"
# colnames(PE_info[2]) <- "NOM_FR_BIRD"
# setnames(PE_info,"type","TYPE")
# setna

#changer le nom des colonnes 

############## 4 - Analyses descriptives du jdd #######

#Indices de diversit� ?

#POUR LA DESCRIPTION JE CREE UN JDD SANS LES 0 
PE_obs<-subset(PE, ABONDANCE != 0 )
PE_obs

PE_obs_info <-subset(PE_info, ABONDANCE != 0 )
View(PE_obs_info)

####### a - Variable reponse : ABONDANCE ####### 
#VARIABLE REPONSE == ABONDANCE
plot(PE_obs$ABONDANCE)
hist(PE_obs$ABONDANCE) #bcp bcp de petites valeurs#att valeurs extremes
summary(PE$ABONDANCE)
summary(PE_obs$ABONDANCE)#+ de 50% des valeurs sont des 1 ; max a 700 indiv
table(PE_obs$ABONDANCE)
table(PE$ABONDANCE)
table(PE$ABONDANCE >= 1)
table(PE$ABONDANCE >=1)[2]/length(PE$ABONDANCE)*100 #pour differencier le 0 des autres 

mean(PE_obs$ABONDANCE); sd((PE_obs$ABONDANCE)) ; var(PE_obs$ABONDANCE)
#variance +++ a cause des quelques valeurs tres hautes ? 






####### b - Variable SITE : les points d ecoutes #######

#exploration de base : 
length(unique(PE$SITE)) #le nombre de points d ecoute (120) 
summary(PE$SITE)#pas grand interet ? 
table(PE$SITE)# pas grand interet ?
table(PE_obs$SITE)#nombre d observation par site 
hist(table(PE_obs$SITE))

#Nombre d'oiseaux (en abondance (donc pas tres pertinent?)) par site :
AB_site <- aggregate(ABONDANCE ~ SITE, data = PE_obs, FUN = sum)#fonction aggregate pour fusionner des infos ensemble 
colnames(AB_site)[2] <- "nb_bird"#renomme variable ABONDANCE
#Graphique : 
barplot(AB_site$nb_bird, names.arg = AB_site$SITE, xlab = "Site", ylab = "Nombre d'oiseaux", main = "Nombre d'oiseaux par site")

#Richesse specifique par site, toute année confondu : 
RS_site<- aggregate(ESPECE ~ SITE, data = PE_obs, FUN = function(x) length(unique(x)))
colnames(RS_site)[2] <- "RS"
#representation graphique 
par(las=2)#fonction qui permet d'orienter les noms des axes
barplot(RS_site$RS, names.arg = RS_site$SITE, xlab = "Site",
        ylab = "Nombre d'espèces", main = "Richesse spécifique par site", cex.names = 0.5)






####### c - Variable ANNEE #######
summary(PE$ANNEE)
max(PE$ANNEE)-min(PE$ANNEE)#Le nombre d annees de suivi est de
table(PE_obs$ANNEE) #le nombre total d'obs par annee est de 
plot(table(PE_obs$ANNEE), main = "Nombre d'observations d'oiseaux par an ",
     xlab = "Année", ylab = "obs d'oiseaux")
#faire un truc + beau apres 

#Quantité d'oiseaux au cours du temps 
AB_year <- aggregate(ABONDANCE ~ ANNEE, data = PE_obs, FUN = function(x) length(unique(x)))
colnames(AB_year)[2] <- "nb_bird"
AB_lm <- lm(nb_bird~ANNEE, data = AB_year)#la quantite d oiseaux n a pas l air de changer 
#resultat a prendre avec des pincettes car comptage particulier 
summary(AB_lm)
#graphique 
par(las = 2) #las = 2 permet d'incliner a 90 les axes
barplot(AB_year$nb_bird, names.arg = AB_year$ANNEE, xlab = "Année", 
        ylab = "Quantité d'oiseaux", main = "Nombre d'oiseaux par année", cex.names = 0.8)


# Richesse spécifique par année, tout site confondu : 
RS_year <- aggregate(ESPECE ~ ANNEE, data = PE_obs, FUN = function(x) length(unique(x)))
colnames(RS_year)[2] <- "RS"
RS_lm <- lm(RS~ANNEE, data = RS_year)
summary(RS_lm)# on tend vers une baisse de la RS, quasi-significatif
# Graphique :
par(las = 2) #las = 2 permet d'incliner a 90 les axes
barplot(RS_year$RS, names.arg = RS_year$ANNEE, xlab = "Site et année", 
        ylab = "Nombre d'espèces", main = "Nombre d'espèces d'oiseaux par année", cex.names = 0.8)

# Ajout de la ligne de tendance 
#abline(RS_lm, col = "red")
#lines(loess(RS_year$RS ~ RS_year$ANNEE), col = "red")
#mean_by_year <- tapply(RS_year$RS, RS_year$ANNEE, mean)
#lines(names(mean_by_year), mean_by_year, type = "l", col = "red")
#rien ne fonctionne ?! 
#faire sur un plot, pas de barplot
#tenter ggplot 




####### d - Variable ESPECE ######
#Stats de base :
length(unique(PE$ESPECE)) #est le nombre d espece vu dans ce protocole, toutes annees confondues 
table(PE_obs$NOM_FR_BIRD)#le nombre de fois ou chaque esp a ete vu, toutes annees confondue 

unefois <- data.frame(table(PE_obs$NOM_FR_BIRD) == 1)#extraire ceux present qu'une fois 
table(unefois) #Dans ce jeu de données, 9 especes ont ete vu 1 fois, et 90 plusieurs fois
centfois <- data.frame(table(PE_obs$NOM_FR_BIRD) <= 100)#extraire ceux present + de cent fois 
table(centfois) # 42 especes ont ete vu - de cent fois 

barplot(table(PE_obs$NOM_FR_BIRD))#visualisation de la ligne d'au dessus 
barplot(tail(sort(table(PE_obs$NOM_FR_BIRD)),length(unique(PE$ESPECE)) )) #meme chose avec les espèce dans l'ordre d'obs
pie(table(PE_obs$NOM_FR_BIRD))#le camembert pas très lisible mais permet tout de meme de visualiser quelques especes tres presente
#Quelques explorations par curiosite : 
#LES 10 + VU
tail(sort(table(PE_obs$NOM_FR_BIRD)),10)#me donne les 10 + grandes valeurs 
pie(tail(sort(table(PE_obs$NOM_FR_BIRD)),10))#camembert des 10 + presents
barplot(tail(sort(table(PE_obs$NOM_FR_BIRD)),10))
#Pour l'année 2002, nous avons : 
table(PE_obs$NOM_FR_BIRD[PE_obs$ANNEE == "2002"])
length(unique(PE_obs$ESPECE[PE_obs$ANNEE==2002])) #RS en 2002 est :

# Sur combien de point d ecoute sont present chaque espece, toute annee confondu : 
ES_PE <- aggregate(SITE~ ESPECE, data = PE_obs, FUN = function(x) length(unique(x)))
colnames(ES_PE)[2] <- "nb_PE"
ES_PE <- ES_PE[order(ES_PE$nb_PE),]#fonction order pour trier dans l'ordre croissant 

mean(ES_PE$nb_PE) ; sd(ES_PE$nb_PE) ; var(ES_PE$nb_PE)
summary(ES_PE) #50% des esp st presentes sur 44 sites #cela varie au cours du temps ?

# Graphique :
par(las = 2) #las = 2 permet d'incliner a 90 les axes
barplot(ES_PE$nb_PE, names.arg = ES_PE$ESPECE, xlab = "Nom des espèces", 
        ylab = "Nombre de points d'écoutes", main = "Nombre de sites fréquenté par espèce", 
        cex.names = 0.3)#graphique en baton #cex.names pour la taille # names.arg pour def les noms  pour chaque barre 
abline(h = median(ES_PE$nb_PE), col = "darkred", lty = 2)# Ajout de la ligne de tendance 
#h pour horizontale, lty = 2 pour les pointilles 


  


####### e - Les groupes taxonomiques #######

#ORDRE TAXONOMIQUE 
GT <- aggregate(ESPECE ~ order_tax, data = PE_obs_info, FUN = function(x) length(unique(x)))
colnames(GT) <- c("GPTAX", "NB_ESP")
GT <-GT[order(GT$NB_ESP),]
par(las = 2) #las = 2 permet d'incliner a 90 les axes
barplot(GT$NB_ESP, names.arg = GT$GPTAX, xlab = "groupe taxo", 
        ylab = "nombre d'especes", main = "Nombre d'espèces d'oiseaux par groupe taxonomique", cex.names = 0.8)
#le groupe des passereaux est largement represente dans le jdd 

#FAMILLE TAXONOMIQUE 
GT2 <- aggregate(ESPECE ~ family_tax, data = PE_obs_info, FUN = function(x) length(unique(x)))
colnames(GT2) <- c("FMTAX", "NB_ESP")
GT2 <-GT2[order(GT2$NB_ESP),]
par(las = 2) #las = 2 permet d'incliner a 90 les axes
barplot(GT2$NB_ESP, names.arg = GT2$FMTAX, xlab = "FAMILLE taxo", 
        ylab = "nombre d'especes", main = "Nombre d'espèces d'oiseaux par famille", cex.names = 0.8)

####### f - Les régimes alimentaires ####### 
#les infos des regimes alimentaires sont stockees dans le jdd "geb" 
#bien que je ne comprends pas tous les noms de variables, 
#on va regarder tout ca : 

geb_ss <- merge(code_crbpo, geb , by.y = "code", by.x = "ESPECE"  )

table(geb_ss$e.seeds.nuts.grain)
fruit <- table(geb_ss$e.fruits.frugivory)
View(fruit)
table(geb_ss$e.vegitative)
invert <-table(geb_ss$e.invert)
fish <-table(geb_ss$e.fish)
table(geb_ss$e.v.sm.mammals)
table(geb_ss$e.lg.mammals)
#il semblerait que la grosse proportions d'oiseaux du jdd soient des mangeurs d'invertebres 

#Graphique : 

#Proportion de frugivores :
barplot(fruit, main="mangeurs de fruits",
        ylab="nombre despeces mangeuses de fruits",
        col=c("blue", "red"), cex.names = 0.5)


#Proportion de mangeurs d'invertebres :
barplot(invert, main="mangeurs d invertebre",
        ylab="nombre despeces mangeuses d 'invert",
        col=c("blue", "red"), cex.names = 0.5)


#Proportion de mangeurs de poissons :
barplot(fish, main="mangeurs de poissons",
        ylab="nombre despeces mangeuses de poissons",
        col=c("blue", "red"), cex.names = 0.5)



####### g - Le poids des oiseaux #######


#Poids des espèces (pas tres pertinent)
geb_ss<-geb_ss[order(geb_ss$e.bodymass.g.),]
par(las = 2)
barplot(geb_ss$e.bodymass.g., main="Poids des espèces d'oiseaux de grand lieu",
        xlab="espece", ylab="masse",names.arg = geb_ss$code,
        col=c("blue", "red"), cex.names = 0.5)


##### PE_info : la création 




########### 5 - EXPORTER LE JDD FINAL 

#write.csv


#####################
#
#
#           PARTIE 2 : Les Analyses 
#
#
####################


####### 6 - Variations d'abondances #######
summary(PE)
PE$type <-as.factor(PE$type)
#test et bidouillage pour voir ce qui sort : 


#ABONDANCE EN FONCTION DES ANNEES EN GAUSSIEN : 
md <- glm(family = gaussian, ABONDANCE ~ ANNEE, data =PE)
summary(md)

#ABONDANCE EN FONCTION DES ANNEES EN POISSON : 
md <- glm(family = poisson, ABONDANCE ~ ANNEE, data =PE)
summary(md)

#ABONDANCE EN FONCTION DES ANNEES EN quasiPOISSON : 
md <- glm(family = quasipoisson, ABONDANCE ~ ANNEE, data =PE)
summary(md)

PEtest <- subset(PE, type == "0")
PErousserolleeffar <- subset(PE, ESPECE == "ACRSCI") 

md <- glm(family = poisson, ABONDANCE ~ SITE , data =PEtest)
summary(md)


mdrouss <- glm(family = poisson , ABONDANCE ~ ANNEE, data = PErousserolleeffar)
summary(mdrouss)#on perd -0,07 rousserolle par an ? très significatif 
mdrouss2 <- glm(family = poisson , ABONDANCE ~ SITE, data = PErousserolleeffar)
summary(mdrouss2)

#PAS DE DONNEES METEO POUR 2022 !!! 
md_info <-  glm(family = poisson , ABONDANCE ~ TM + RR + j_gel, data = PE_info)
summary(md_info)#bizarre pour le nombre de jours de gel 

md_infot1 <-  glm(family = poisson , ABONDANCE ~ RR + RR_sum, TM_spring, RR_sum_spring,j_gel, data = PE_infot1)
summary(md_infot1)#bizarre pour le nombre de jours de gel 



#Extraire jdd 
library(openxlsx)
write.csv(PE_info, file = "PE_info.csv", row.names = TRUE)
write.xlsx(PE_info, file = "PE_info.xlsx", sheetName = TRUE)
write.xlsx(info_especes, file = "info_especes.xlsx", sheetName = TRUE)
write.xlsx(pod_site, file = "pod_site.xlsx", sheetName = TRUE)
write.xlsx(table_niveau_eau, file = "table_niveau_eau.xlsx", sheetName = TRUE)
write.xlsx(meteo_y_etude, file = "meteo_gl_final.xlsx", sheetName = TRUE)


