###################### PARTIE 1 - CREATION DU JDD -----------------------------
############## 1 - CHARGEMENT DU JDD BRUT : pod ########


#setwd("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA")
library(readxl)
pod <- read_excel("DATA/dataPE.xlsx", 
                  col_names = TRUE)
#VISUALISER ET RESUME DU JDD
View(pod) #voir
summary(pod) #resume
dim(pod)# dimensions : nbre de lignes puis de colonnes



############## 2 - CREATION DU JDD EXPLOITABLE SUR R - une ligne par obs ##############


####### a - Nettoyer lignes et colonnes et renommer variables : pod2 #####


pod2<-subset(pod, Site != "TOTAL")#subset pour tout garder sauf les lignes total (avec !=)
pod2 <- pod2[,-c(3:4)]#enlever lat et long en colonne 3 et 4 
pod2[is.na(pod2)] <- 0#remplacer les na par des 0 
colnames(pod2)[1:2] <- c("ANNEE","SITE")#renommer les colonnes
dim(pod2)#dimensions : on a 20 lignes en moins et deux colonnes en moins? alors c est ok
View(pod2)


####### b - Basculer le jdd en 1 ligne = 1 observation : PE #####


#Fonction reshape pour restructurer le jdd
#melt permet de passer des colonnes en lignes, en creant une nouvelle variable a partir de ces lignes 
PE <- reshape2::melt(pod2,id=c("ANNEE","SITE"),value.name = "ABONDANCE")
colnames(PE)[3] <- "ESPECE"


############## 3 - CREATION DES JDD DE VARIABLES EXPLICATIVES -----------------
####### a - Les points GPS des points d ecoute : pod_site #####

#pod_site = subset(pod2, ANNEE == "2002" ) # qu'est ce que cest que cette ligne que j ai fait 
pod_site <- unique(pod[,2:4])
View(pod_site) 

#il faut ajouter les caracteristiques de chaque point + est-ce que le point a changer de milieu 

####### b - Caractéristique de l'habitat : ######

#### en attente des donnees 

####### c - Les noms vernaculaires des oiseaux : code_crbpo #####

library(readxl)
code_crbpo <- read_excel("DATA/noms_vernaculaires.xlsx", col_names = FALSE)#chargement du jdd 
titre<-c("ESPECE", "NOM_FR_BIRD")#nom des variables dans la matrice
colnames(code_crbpo) <-titre
View(code_crbpo)#visualisation du jdd
dim(PE)
#jonction du jdd principal avec code_crbpo pour avoir les noms vernaculaires dans le jdd 
PE <- merge(PE,code_crbpo, all.x = TRUE, by = "ESPECE")
dim(PE)#verif que merge fonctionne
#Pour reprendre l ordre des colonnes : 
library(dplyr)
PE <-select(PE, ANNEE, SITE, ESPECE, NOM_FR_BIRD,ABONDANCE)#data puis ordre des colonnes
View(PE)


####### d - La famille et l ordre de chaque espece : info_esp #####



library(readr)
info_esp <- read_csv("DATA/espece.csv")
View(info_esp)
summary(info_esp)
#Faire la correction du code crbpo diff entre les jdd :
info_esp$code_crbpo <- ifelse(info_esp$pk_species == "LANSEN" , "LANSER" , info_esp$pk_species)
#fonction ifelse = ptite boucle avec Si ... alors ... sinon ...) 
#Jonction des deux jdd : 
PE_info <- merge(PE,info_esp, all.x = TRUE, by.x = "ESPECE", by.y = "code_crbpo")
#PE_info <- PE_info[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
View(PE_info)
#/!\ Attention /!\ 
#saisi des codes crbpo diff entre les jdd 
#il faut verifier que tous les codes soient les memes :
rea <- unique(subset(PE_info, is.na(family_tax), select = "ESPECE"))#recherche des mauvais code espece 
rea # si egal à 0 alors c est ok 




####### e - Le poids des esp, leur regime alimentaire et autre : geb #####

#chargement du jdd avec les poids moyen des esp 
#attention triche : je l ai converti en csv 
library(readr)
geb <- read.csv2("DATA/geb12127-sup-0002-ap.csv",skip = 6)# skip pour sauter les premieres lignes 
geb$code <- casefold(geb$code, upper=T)#permet de tout mettre en majuscule (pour merge apres)
#/!\ CODE CRBPO A CORRIGER /// meme oiseau == autre nom 
geb$code_crbpo <- ifelse(geb$code == "LANSEN" , "LANSER" , geb$code) # maj des codes crbpo
#expl de la fontion : SI (...condition... , ALORS ... , SINON ...)
#geb <- geb[,c(2:3)]#enlever les colonnes non desirees
summary(geb)
View(geb)
#geb$POIDS <- as.numeric(geb$POIDS) #ne pas faire pour l'instant car introduit des NA

PE_info <- merge(PE_info,geb, all.x = TRUE, by.x = "ESPECE", by.y = "code")#fusion des deux jdd 


####### f - Gradient de specialisation : ind_fonction  #######

library(readr)
ind_fonction<- read_csv("DATA/espece_indicateur_fonctionel.csv")
View(ind_fonction)
ind_fonction$pk_species<- ifelse(ind_fonction$pk_species == "LANSEN" , "LANSER" , ind_fonction$pk_species)
#j ai remplace directement dans la variable pk_species 



####### g - La meteo de grand lieu : meteo_gl #######

library(readr) 
meteo_gl<- read_csv2("DATA/AnnualData19602021.csv")
View(meteo_gl)
summary(meteo_gl)
dim(meteo_gl)
meteo_gl$RR <- as.numeric(meteo_gl$RR) #mettre en numerique ce dont on a besoin 
meteo_gl$Date_m <- as.numeric(meteo_gl$Date_m)
meteo_gl <- meteo_gl[!is.na(meteo_gl$RR),] # supprimer la valeur egal a NA /!\ est ce que jai bien fait ?
#drop_na(nom_de_la_colonne) #autre facon de supprimer les NA 
dim(meteo_gl)#1 ligne en moins, on avait 1 na ---> c est ok 


#faire les calculs/synthese et 
TM_y <- ave(meteo_gl$TM,meteo_gl$Date_y)#temperature moyenne par an 
TM_y
meteo_gl <- cbind(meteo_gl,TM_y)
meteo_gl

RR_y <- ave(meteo_gl$RR,meteo_gl$Date_y)#precipitation moyenne par an 
RR_y
meteo_gl <- cbind(meteo_gl,RR_y)
meteo_gl

#extraire les mois de printemps puis calculer moyennes par an :

meteo_gl_spring<-subset(meteo_gl, Date_m == 5 | Date_m == 6 )#pour les mois de mai et juin 
#la barre verticale permet de creer "OU" et donc de mettre +ieurs nombres 
tt_spring <- ave(meteo_gl_spring$TM,meteo_gl_spring$Date_y)
rr_spring <- ave(meteo_gl_spring$RR,meteo_gl_spring$Date_y)
meteo_gl_spring<- cbind(meteo_gl_spring,tt_spring,rr_spring)
summary(meteo_gl_spring)
View(meteo_gl_spring)



#il reste a faire les temperatures de l'hiver precedent, donc prendre de novembre n-1 à fevrier n 
#apres discussion avec Seb, temp de l'hiver n'est pas perninent mais nbre de jours de gels est pertinent ++ 
# donc il faut le nombre de jours en dessous de (0, -2, que choisir?) et faire une variable " jour de gel"



#maintenant il faudrait faire un merge pour avoir annee - TM - RR 

meteo_synthese<-matrix(c(2002:2022),byrow = TRUE, ncol = 1)#il faut automatiser le 2022 
colnames(meteo_synthese)[1] <- c("ANNEE")
#peut etre il faut supprimer ce dont je n'ai pas besoin 
meteo_gl_spring_simp <-cbind(meteo_gl_spring[4], meteo_gl_spring[14:17])
synth_meteo <- merge(meteo_synthese,meteo_gl_spring_simp, all.x = TRUE, all.y = FALSE, by.x = "ANNEE", by.y = "Date_y")
#je n'arrive pas a faire le merge correctement 



####### h - Les niveaux d'eaux de GL  #######

library(readxl)
niv_eau <- read_excel("DATA/Cote Lac GL_1958_2022.xlsx", col_names = T)#chargement du jdd 
summary(niv_eau)
#remettre dans le bon ordre le jdd avec reshape ? 
############## 4 - Analyses descriptives du jdd #######

#les questions qu'on se pose : 
#Comment s'organise la repartition de la communaute entre les regimes alimentaires ? 
#Combien d'especes a-t-on au cours des ans ? 
#Indices de diversite ? 

#POUR LA DESCRIPTION JE CREE UN JDD SANS LES 0 
PE_obs<-subset(PE, ABONDANCE != 0 )
PE_obs

#graph de chaque variable 
plot(PE_obs$ABONDANCE)
hist(PE_obs$ABONDANCE) 

####### a - La variable reponse ####### 
#VARIABLE REPONSE == ABONDANCE
summary(PE$ABONDANCE)#fonctionne pour continue ou discret
table(PE_obs$ABONDANCE)
table(PE$ABONDANCE)
table(PE$ABONDANCE >= 1)
table(PE$ABONDANCE >=1)[2]/length(PE$ABONDANCE)*100 #pour differencier le 0 des autres 
hist(data$nb_pulli_envol, xlab = "nbre bb envol", ylab = "frequence", main= "Frequence du nombre de jeunes ? l'envol par nid")

mean(PE_obs$ABONDANCE); sd((PE_obs$ABONDANCE)) ; var(PE_obs$ABONDANCE)


####### b -Les autres variables #######

#####SITE
length(unique(PE$SITE)) #le nombre de points d ecoute #probleme, 119 ou 120 ?? 
#levels(PE$SITE)#pourquoi NULL ? 
summary(PE$SITE)#pas grand interet 
table(PE$SITE)# pas grand interet 
#comment faire pour avoir le nombre de sites ou sont present chaque espèce ? 





#####ANNEE
max(PE$ANNEE)-min(PE$ANNEE)#Le nombre d annees de suivi est de
table(PE_obs$ANNEE) #le nombre total d'obs par annee est de 
summary(PE$ANNEE)
plot(table(PE_obs$ANNEE), main = "Nombre d'observations d'oiseaux par an ", xlab = "Année", ylab = "obs d'oiseaux")
#faire un truc + beau apres 

#####ESPECE
length(unique(PE$ESPECE)) #est le nombre d espece vu dans ce protocole, toutes annees confondues 
table(PE_obs$NOM_FR_BIRD)#le nombre de fois ou chaque esp a ete vu, toutes annees confondue 
barplot(table(PE_obs$NOM_FR_BIRD))#visualisation de la ligne d'au dessus 
barplot(tail(sort(table(PE_obs$NOM_FR_BIRD)),length(unique(PE$ESPECE)) )) #meme chose avec les espèce dans l'ordre d'obs
pie(table(PE_obs$NOM_FR_BIRD))#le camembert pas très lisible mais permet tout de meme de visualiser quelques especes tres presente

prop.table( table(PE_obs$NOM_FR_BIRD))#pour le mettre en pourcentage, pas tres pertinent

#LES 10 + VU
tail(sort(table(PE_obs$NOM_FR_BIRD)),10)#me donne les 10 + grandes valeurs 
pie(tail(sort(table(PE_obs$NOM_FR_BIRD)),10))#camembert des 10 + presents
barplot(tail(sort(table(PE_obs$NOM_FR_BIRD)),10))

# POURCENTAGES 
prop.table (table (PE_obs$ANNEE)) 
#regarder des relations 
plot(PE_obs$ESPECE~PE_obs$ANNEE)
boxplot(PE_obs$ESPECE~PE_obs$ANNEE)

hist(PE_obs$ESPECE~PE_obs$SITE)
plot(PE_obs$ABONDANCE~PE_obs$ESPECE)

hist(PE_obs$ESPECE)
