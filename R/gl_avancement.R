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
sw3 <- reshape2::melt(pod,id=c("ANNEE","SITE"),value.name = "ABONDANCE")
colnames(sw3)[3] <- "ESPECE"

############## 3 - CREATION DES JDD DE VARIABLES EXPLICATIVES ########
####### a - Les points GPS des points d ecoute : pod_site #####
pod_site <- unique(pod[,3:4])
##### b - 
####### b - Les noms vernaculaires des oiseaux : code_crbpo #####
#library(readxl) # au cas ou il faut la relancer
#chargement du jdd 
code_crbpo <- read_excel("DATA/noms_vernaculaires.xlsx", col_names = FALSE)
titre<-c("ESPECE", "NOM_FR_BIRD")#nom des variables dans la matrice
colnames(code_crbpo) <-titre
#visualisation du jdd
View(code_crbpo)
dim(PE)
#jonction du jdd principal avec code_crbpo pour avoir les noms vernaculaires dans le jdd 
PE <- merge(PE,code_crbpo, all.x = TRUE, by = "ESPECE")
dim(PE)#verif que merge fonctionne
#reprendre l ordre des colonnes
library(dplyr)
PE <-select(PE, DATE, LIEU, ESPECE, NOM,ABONDANCE)#data puis ordre des colonnes
View(PE)
####### c - La famille et l ordre de chaque espece : info_esp #####
library(readr)
info_esp <- read_csv("DATA/espece.csv")
View(info_esp)
summary(info_esp)

#Jonction des deux jdd 
PEc <- merge(PE,espece, all.x = TRUE, by.x = "ESPECE", by.y = "code_crbpo")
PEc <- PEc[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
PEc

#/!\ Attention /!\ 
#saisi des codes crbpo diff entre les jdd 
#il faut que tous les codes soient les memes 
rea <- unique(subset(PEc, is.na(family_tax), select = "ESPECE"))#recherche des mauvais code espece 
espece$code_crbpo <- ifelse(espece$pk_species == "LANSEN" , "LANSER" , espece$pk_species)



####### d - #####

####### e - Gradient de specialisation : 
#######