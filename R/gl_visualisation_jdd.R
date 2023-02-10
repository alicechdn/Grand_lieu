########## PARTIE VISUALISATION DU JDD ##############"

####### CHARGEMENT DU JDD #######
setwd("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA")
PE<-read.csv2("PE_jdd.csv", header = T, sep = ",")
View(PE)
summary(PE)
library(readxl)
code_crbpo <- read_excel("DATA/noms_vernaculaires.xlsx", col_names = FALSE)
titre<-c("ESPECE", "NOM")#nom des variables dans la matrice
colnames(code_crbpo) <-titre
View(code_crbpo)
dim(PE)
####### SUPPRIMER LES VALEURS 0 #######
table(PE$DATE)
#est-ce-qu'il faut supprimer les observations egales a 0 ? 
PE<-subset(PE, ABONDANCE != 0 )
View(PE)
summary(PE)

####### NOMS VERNACULAIRES DES OISEAUX DANS LE JDD ########
PE <- merge(PE,code_crbpo, all.x = TRUE, by = "ESPECE")
#reprendre l ordre des colonnes
library(dplyr)
PE <-select(PE, DATE, LIEU, ESPECE, NOM,ABONDANCE)#data puis ordre des colonnes
View(PE)
#supprimer la colonne avec les noms ss forme de codes crbpo 
#ne pas faire car autre jdd contient les codes 
#PE2 <- PE[,-3]
#View(PE2) 

#etapes suivantes --> completer le jdd avec les variables environnementales

####### FAMILLE ET ORDRE DE CHAQUE ESPECE #######
library(readr)
espece <- read_csv("DATA/espece.csv")
#esp<-read.csv2("DATA/espece.csv", header = T, sep = ",") #autre facon de l'ouvrir
View(espece)
summary(espece)
?merge
PEc <- merge(PE,espece, all.x = TRUE, by.x = "ESPECE", by.y = "pk_species")
PEc <- PEc[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
PEc
####### POIDS MOYEN DE CHAQUE ESPECE ##########
#chargement du jdd avec les poids moyen des esp 
#attention triche : je l ai converti en csv 
library(readr)
geb <- read.csv2("DATA/geb12127-sup-0002-ap.csv", sep = ";", header = FALSE)
View(geb)
geb <- geb[-c(1:7),c(2:3)]#enlever les lignes et les colonnes non desirees
View(geb)
titre2<-c("CODE", "POIDS")#nom des variables dans la matrice
colnames(geb) <-titre2
summary(geb)
#geb$POIDS <- as.numeric(geb$POIDS) #ne pas faire pour l'instant car introduit des NA
summary(geb)
View(geb)
geb$CODE <- casefold(geb$CODE, upper=T)#permet de tout mettre en majuscule (pour merge apres)
View(geb)
#fusion des deux jdd 
PEe <- merge(PEc,geb, all.x = TRUE, by.x = "ESPECE", by.y = "CODE")




#rentrer le nouveau jdd complet avec donnees environnementales et tout dans un csv
#write.csv(sw2, file = "PE_complet.csv", col.names = TRUE, row.names = FALSE, sep = ",")
