###################### PARTIE 1 - CREATION DU JDD ############
############## 1 - CHARGEMENT DU JDD BRUT ########


#setwd("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA")
library(readxl)
pod <- read_excel("DATA/dataPE.xlsx", 
                  col_names = TRUE)
#VISUALISER ET RESUME DU JDD
View(pod) #voir
summary(pod) #resume
dim(pod)# dimensions : nbre de lignes puis de colonnes



############## 2 - CREATION DU JDD EXPLOITABLE SUR R - une ligne par obs ##############


#######
pod<-subset(pod, Site != "TOTAL")#subset pour tout garder sauf les lignes total (avec !=)
pod <- pod[,-c(3:4)]#enlever lat et long
pod[is.na(pod)] <- 0#remplacer les na par des 0 
colnames(pod)[1:2] <- c("ANNEE","SITE")#renommer les colonnes
dim(pod)#dimensions : on a 20 lignes en moins et deux colonnes en moins? alors c est ok
View(pod)

#######
#Fonction reshape pour restructurer le jdd
#melt permet de passer des colonnes en lignes, en creant une nouvelle variable a partir de ces lignes 
PE <- reshape2::melt(pod,id=c("ANNEE","SITE"),value.name = "ABONDANCE")
colnames(PE)[3] <- "ESPECE"



############## 3 - CREATION DES JDD DE VARIABLES EXPLICATIVES ########



####### a - Les points GPS des points d ecoute : pod_site #####
pod_site <- unique(pod[,3:4])

####### b - Les noms vernaculaires des oiseaux : code_crbpo #####
#library(readxl) # au cas ou il faut la relancer
code_crbpo <- read_excel("DATA/noms_vernaculaires.xlsx", col_names = FALSE)#chargement du jdd 
titre<-c("ESPECE", "NOM_FR_BIRD")#nom des variables dans la matrice
colnames(code_crbpo) <-titre
View(code_crbpo)#visualisation du jdd
dim(PE)
#jonction du jdd principal avec code_crbpo pour avoir les noms vernaculaires dans le jdd 
PE <- merge(PE,code_crbpo, all.x = TRUE, by = "ESPECE")
dim(PE)#verif que merge fonctionne
#reprendre l ordre des colonnes
library(dplyr)
PE <-select(PE, ANNEE, SITE, ESPECE, NOM_FR_BIRD,ABONDANCE)#data puis ordre des colonnes
View(PE)

####### c - La famille et l ordre de chaque espece : info_esp #####
library(readr)
info_esp <- read_csv("DATA/espece.csv")
View(info_esp)
summary(info_esp)
info_esp$code_crbpo <- ifelse(info_esp$pk_species == "LANSEN" , "LANSER" , info_esp$pk_species)
#Jonction des deux jdd 
PE_info <- merge(PE,info_esp, all.x = TRUE, by.x = "ESPECE", by.y = "code_crbpo")
#PE_info <- PE_info[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
View(PE_info)
#/!\ Attention /!\ 
#saisi des codes crbpo diff entre les jdd 
#il faut verifier que tous les codes soient les memes :
rea <- unique(subset(PE_info, is.na(family_tax), select = "ESPECE"))#recherche des mauvais code espece 
rea # si egal à 0 alors c est ok 

####### d - Le poids des esp, leur regime alimentaire et autre #####
#chargement du jdd avec les poids moyen des esp 
#attention triche : je l ai converti en csv 
library(readr)
geb <- read.csv2("DATA/geb12127-sup-0002-ap.csv",skip = 6)# skip pour sauter les premieres lignes 
geb$CODE <- casefold(geb$CODE, upper=T)#permet de tout mettre en majuscule (pour merge apres)
#/!\ CODE CRBPO A CORRIGER /// meme oiseau == autre nom 
geb$code_crbpo <- ifelse(geb$code == "LANSEN" , "LANSER" , geb$code) # maj des codes crbpo
#expl de la fontion : SI (...condition... , ALORS ... , SINON ...)
#geb <- geb[,c(2:3)]#enlever les colonnes non desirees
summary(geb)
View(geb)
#geb$POIDS <- as.numeric(geb$POIDS) #ne pas faire pour l'instant car introduit des NA

PE <- merge(PEc,geb, all.x = TRUE, by.x = "ESPECE", by.y = "CODE")#fusion des deux jdd 
####### e - Gradient de specialisation : 
#######