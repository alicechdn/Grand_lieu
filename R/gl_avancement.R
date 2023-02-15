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

pod_site = subset(pod2, ANNEE == "2002" )
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
#reprendre l ordre des colonnes
library(dplyr)
PE <-select(PE, ANNEE, SITE, ESPECE, NOM_FR_BIRD,ABONDANCE)#data puis ordre des colonnes
View(PE)


####### d - La famille et l ordre de chaque espece : info_esp #####



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
#temperature moyenne par an 
TM_y <- ave(meteo_gl$TM,meteo_gl$Date_y)
TM_y
ave(breaks, tension, FUN = function(x) mean(x, trim = 0.1))
cbind(meteo_gl,TM_y)
meteo_gl
#il faudrait la temperature pour le printemps et aussi celui de l'hiver precedent 
#il faudrait deux conditions : annee et mois 
#ou supprimer les mois qu'on ne veut pas ? 

#extraire les mois de printemps puis calculer moyennes par an ? 
meteo_gl$RR <- as.numeric(meteo_gl$RR)
meteo_gl$Date_m <- as.numeric(meteo_gl$Date_m)
meteo_gl_spring<-subset(meteo_gl, Date_m == 5 | Date_m == 6 | Date_m == 7 )#pour les mois de mai à juillet 
#la barre verticale permet de creer "OU" et donc de mettre +ieurs nombres

meteo_gl_spring<- cbind(meteo_gl_spring,ave(meteo_gl_spring$TM,meteo_gl_spring$Date_m))

#on a une valeur = à NA dans le jdd et j'ai l'impression que c'est pour ca que ca ne fonctionne pas : 
#on va la supprimer  
#student[!is.na(student$science),] 


meteo_gl_spring <- meteo_gl_spring[!is.na(meteo_gl_spring$RR),] 
#drop_na(nom_de_la_colonne) #autre facon de supprimer les NA 
summary(meteo_gl_spring)
meteo_gl_spring<- cbind(meteo_gl_spring,ave(meteo_gl_spring$RR,meteo_gl_spring$Date_m))
View(meteo_gl_spring)
#cetait donc bien le na qui bloquait tout/ est ce que cela va poser probleme de lavoir supprimer ? 

####### h - Les niveaux d'eaux de GL  #######

library(readxl)
niv_eau <- read_excel("DATA/Cote Lac GL_1958_2022.xlsx", col_names = T)#chargement du jdd 
summary(niv_eau)
#remettre dans le bon ordre le jdd avec reshape ? 