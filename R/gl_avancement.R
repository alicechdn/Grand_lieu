###################### PARTIE 1 - CREATION DU JDD -----------------------------
############## 1 - CHARGEMENT DU JDD BRUT : pod ########


#setwd("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA")
library(readxl)
pod <- read_excel("DATA/dataPE.xlsx", 
                  col_names = TRUE)
#VISUALISER ET RESUME DU JDD
#View(pod) #voir
summary(pod) #resume
dim(pod)# dimensions : nbre de lignes puis de colonnes



############## 2 - CREATION DU JDD EXPLOITABLE SUR R - une ligne par obs ##############


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


############## 3 - CREATION DES JDD DE VARIABLES EXPLICATIVES -----------------
####### a - Les points GPS des points d ecoute : pod_site #####

#pod_site = subset(pod2, ANNEE == "2002" ) # qu'est ce que cest que cette ligne que j ai fait 
#c'est bon j'ai compris c'était pour ne pas repeter les points d'ecoute mais enfait c'est inutile, pas besoin de le faire 
pod_site <- unique(pod[,2:4])
pod_site<-subset(pod_site, Site != "TOTAL")#garder tout sauf ligne total 
#View(pod_site) #120 lignes = 120 points d ecoute ---> c'est ok

#il faut ajouter les caracteristiques de chaque point + est-ce que le point a changer de milieu 
#apres discussion : si le point a change de milieu = pas pertinent 

####### b - Caractéristique de l'habitat : ######

#### en attente des donnees 

####### c - Les noms vernaculaires des oiseaux : code_crbpo #####

library(readxl)
code_crbpo <- read_excel("DATA/noms_vernaculaires.xlsx", col_names = FALSE)#chargement du jdd 
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


####### d - La famille et l ordre de chaque espece : info_esp #####



library(readr)
info_esp <- read_csv("DATA/espece.csv")
#View(info_esp)
summary(info_esp)
#Faire la correction du code crbpo diff entre les jdd :
info_esp$code_crbpo <- ifelse(info_esp$pk_species == "LANSEN" , "LANSER" , info_esp$pk_species)
#fonction ifelse = ptite boucle avec Si ... alors ... sinon ...) 
#Jonction des deux jdd : 
PE_info <- merge(PE,info_esp, all.x = TRUE, by.x = "ESPECE", by.y = "code_crbpo")
#PE_info <- PE_info[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
#View(PE_info)
#/!\ Attention /!\ 
#saisi des codes crbpo diff entre les jdd 
#il faut verifier que tous les codes soient les memes :
unique(subset(PE_info, is.na(family_tax), select = "ESPECE"))#recherche des mauvais code espece 
# si egal à 0 alors c est ok 




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
#View(geb)
#geb$POIDS <- as.numeric(geb$POIDS) #ne pas faire pour l'instant car introduit des NA

PE_info <- merge(PE_info,geb, all.x = TRUE, by.x = "ESPECE", by.y = "code")#fusion des deux jdd 


####### f - Gradient de specialisation : ind_fonction  #######

library(readr)
ind_fonction<- read_csv("DATA/espece_indicateur_fonctionel.csv")
#View(ind_fonction)
ind_fonction$pk_species<- ifelse(ind_fonction$pk_species == "LANSEN" , "LANSER" , ind_fonction$pk_species)
#j ai remplace directement dans la variable pk_species 



####### g - La meteo de grand lieu : meteo_gl #######

library(readr) 
meteo_gl<- read_csv2("DATA/AnnualData19602021.csv")
#View(meteo_gl)
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
# Filtrer les données pour ne conserver que les mois de décembre, janvier et février
meteo_gl_hiver <- subset(meteo_gl, Date_m %in% c(12, 1, 2))

# Calculer la température moyenne pour chaque année
tt_hiver <- ave(meteo_gl_hiver$TM, meteo_gl_hiver$Date_y)

# Décaler la colonne de température moyenne pour l'hiver précédent
tt_hiver_prec <- c(NA, head(tt_hiver, -1))

# Ajouter les colonnes au jeu de données
meteo_gl_spring <- cbind(meteo_gl_spring, tt_hiver_prec, tt_spring, rr_spring)

# Calculer les moyennes annuelles pour toutes les colonnes
meteo_gl_annual <- aggregate(cbind(Tm, tt_spring, tt_hiver_prec, RR) ~ Date_y, data = meteo_gl_spring, FUN = mean)

# Afficher le tableau final
View(meteo_gl_annual)



#il reste a faire les temperatures de l'hiver precedent, donc prendre de novembre n-1 à fevrier n 
#apres discussion avec Seb, temp de l'hiver n'est pas perninent mais nbre de jours de gels est pertinent ++ 
# donc il faut le nombre de jours en dessous de (0, -2, que choisir?) et faire une variable " jour de gel"



#maintenant il faudrait faire un merge pour avoir annee - TM - RR 

meteo_synthese<-matrix(c(2002:2022),byrow = TRUE, ncol = 1)#il faut automatiser le 2022 
colnames(meteo_synthese)[1] <- "ANNEE"
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

PE_obs_info <-subset(PE_info, ABONDANCE != 0 )
View(PE_obs_info)

####### a - La variable reponse : ABONDANCE ####### 
#VARIABLE REPONSE == ABONDANCE
plot(PE_obs$ABONDANCE)
hist(PE_obs$ABONDANCE) #bcp bcp de petites valeurs
summary(PE$ABONDANCE)
summary(PE_obs$ABONDANCE)#+ de 50% des valeurs sont des 1 ; max a 700 indiv
table(PE_obs$ABONDANCE)
table(PE$ABONDANCE)
table(PE$ABONDANCE >= 1)
table(PE$ABONDANCE >=1)[2]/length(PE$ABONDANCE)*100 #pour differencier le 0 des autres 
hist(data$nb_pulli_envol, xlab = "nbre bb envol", ylab = "frequence", main= "Frequence du nombre de jeunes ? l'envol par nid")

mean(PE_obs$ABONDANCE); sd((PE_obs$ABONDANCE)) ; var(PE_obs$ABONDANCE)
#variance +++ a cause des quelques valeurs tres hautes ? 






####### b - La variable SITE : les points d ecoutes #######

#exploration de base : 
length(unique(PE$SITE)) #le nombre de points d ecoute #probleme, 119 ou 120 ?? 
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
barplot(RS_site$RS, names.arg = RS_site$SITE, xlab = "Site", ylab = "Nombre d'espèces", main = "Richesse spécifique par site", cex.names = 0.5)






####### c - La variable ANNEE #######
summary(PE$ANNEE)
max(PE$ANNEE)-min(PE$ANNEE)#Le nombre d annees de suivi est de
table(PE_obs$ANNEE) #le nombre total d'obs par annee est de 
plot(table(PE_obs$ANNEE), main = "Nombre d'observations d'oiseaux par an ", xlab = "Année", ylab = "obs d'oiseaux")
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
abline(coef(RS_lm), col = "red")
lines(loess(RS_year$RS ~ RS_year$ANNEE), col = "red")
mean_by_year <- tapply(RS_year$RS, RS_year$ANNEE, mean)
lines(names(mean_by_year), mean_by_year, type = "l", col = "red")
#rien ne fonctionne ?! 






####### d - La variable ESPECE ######
#Stats de base :
length(unique(PE$ESPECE)) #est le nombre d espece vu dans ce protocole, toutes annees confondues 
table(PE_obs$NOM_FR_BIRD)#le nombre de fois ou chaque esp a ete vu, toutes annees confondue 
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
PE_aggrege <- aggregate(PE$ABONDANCE, by = list(PE$ESPECE), sum)
names(PE_aggrege) <- c("nom_espece", "abondance_totale")

geb_ss <- merge(geb, PE_aggrege, by.y = "nom_espece", by.x = "code"  )

table(geb_ss$e.seeds.nuts.grain)
fruit <- table(geb_ss$e.fruits.frugivory)
table(geb_ss$e.vegitative)
table(geb_ss$e.invert)
table(geb_ss$e.fish)
table(geb_ss$e.v.sm.mammals)
table(geb_ss$e.lg.mammals)

par(las = 2)
barplot(fruit, main="mangeurs de fruits",
        ylab="nombre despeces mangeuses de fruits",
        col=c("blue", "red"), cex.names = 0.5)


geb_ss<-geb_ss[order(geb_ss$e.bodymass.g.),]
par(las = 2)
barplot(geb_ss$e.bodymass.g., main="Poids des espèces d'oiseaux de grand lieu",
        xlab="espece", ylab="masse",names.arg = geb_ss$code,
        col=c("blue", "red"), cex.names = 0.5)
