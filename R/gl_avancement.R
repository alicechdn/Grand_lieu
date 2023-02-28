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


############## 3 - CREATION DES JDD DE VARIABLES EXPLICATIVES -----------------
####### a - Les points GPS des points d ecoute : pod_site #####

pod_site <- unique(pod[,2:4])
pod_site<-subset(pod_site, Site != "TOTAL")#garder tout sauf ligne total 
#View(pod_site) 
dim(pod_site)#120 lignes = 120 points d ecoute ---> c'est ok

#il faut ajouter les caracteristiques de chaque point 
#apres discussion : si le point a change de milieu = pas pertinent 

rm(pod, pod2)

####### b - Caractéristique de l'habitat : ######

#### en attente des donnees 

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
#Jonction des deux jdd : 
PE_info <- merge(PE,info_esp_complet, all.x = TRUE, by.x = "ESPECE", by.y = "code_crbpo")
#PE_info <- PE_info[,-c(6:17)]#pour ne garder que les colonnes qui m interesse
#View(PE_info)
#/!\ Attention /!\ 
#saisi des codes crbpo diff entre les jdd 
#il faut verifier que tous les codes soient les memes :
unique(subset(PE_info, is.na(family_tax), select = "ESPECE"))#recherche des mauvais code espece 
# si egal à 0 alors c est ok 
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

PE_info <- merge(PE_info,geb, all.x = TRUE, by.x = "ESPECE", by.y = "code")#fusion des deux jdd 


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

#corriger erreur lancen 
#ajouter les noms scientifiques à la liste 
scien_name <- info_esp[,1:5]
scien_name <- scien_name[,-c(2:3)]
View(scien_name)
HWI <- merge(scien_name,HWI_complet, all.x = TRUE, by.x = "scientific_name",
             by.y = "Species name")

table(HWI$`Migration-1`)
table(HWI$`Migration-2`) 
#1 = sédentaire 
#2 = partiellement migrateur 
#3 = totalement migrateur 
table(HWI$`Migration-3`)

barplot(table(HWI$`Migration-2`), main = "repartition des especes migratrices sur gl")

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
tm_y <- aggregate(TM ~ Date_y, data = meteo_gl, mean)
#permet de faire la moyenne des TM en fonction de l'annee
#autre methode possible avec fonction ave()


#Precipitation moyenne par jour et par an : #est-ce-que c'est pertinent ?
rr_y <- aggregate(RR ~ Date_y, data = meteo_gl, mean, na.rm = TRUE)
# na.rm = TRUE pour ne pas prendre en compte les NA
 

#Somme des precipitations de l'annee :
rr_y_sum <- aggregate(RR ~ Date_y, data = meteo_gl, sum, na.rm = TRUE)
colnames(rr_y_sum)[2] <-'RR_sum'


meteo_y <- merge(tm_y,rr_y, all.x = TRUE, by = "Date_y")
meteo_y <-merge(meteo_y,rr_y_sum,all.x = T, by = "Date_y")


library(data.table)
DT_meteo <- meteo_gl#faire une copie du jdd
setDT(DT_meteo)#a quoi ca sert deja ?

dt_y_printemps <- DT_meteo[Date_m %in% c(4,5,6),.(RR_sum_spring = sum(RR,na.rm = TRUE), TM_spring = mean(TM,na.rm=TRUE)),by = Date_y]
#ecriture particuliere de data.table qui permet de creer des moyennes avec des conditions 

meteo_y <-merge(meteo_y,dt_y_printemps, all.x = T, by = "Date_y")



#Creation de la variable "nombre de jours de gel dans l'hiver precedent" : 

#Nouvelle variable qui corrige l'annee pour les mois de octo, nov, dec : 
meteo_gl$fin_hiver <- ifelse(meteo_gl$Date_m %in% c(1:3,10:12),
                             ifelse(meteo_gl$Date_m %in% c(1:3),
                                    meteo_gl$Date_y,meteo_gl$Date_y + 1), NA)

#Creation nouvel objet qui contient le nbre de jours de gel :
gel <- aggregate(TM ~ fin_hiver,data = meteo_gl,
                 FUN = function(X) sum(as.numeric(X < 0)));colnames(gel) <-c("Date_y", "j_gel")
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

plot(meteo_y_etude$j_gel~ meteo_y_etude$Date_y,
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
n_eau_prov <- reshape2::melt(niv_eau,id=c("date"),value.name = "hauteur (m)")#passage colonnes en lignes 
library(stringr)
Date <-str_split(n_eau_prov$date, "-", simplify = TRUE) ; colnames(Date) <- c("annee","mois","jour")
#
n_eau_complet<- data.frame(n_eau_prov$date, Date[,2:3] , annee = n_eau_prov$variable, hauteur = n_eau_prov$hauteur)
colnames(n_eau_complet)[1] <- "date"
library(dplyr)
n_eau_complet <-select(n_eau_complet, date, annee, mois, jour, hauteur)#data puis ordre des colonnes
#View(PE)
summary(n_eau_complet)#variables sous un mauvais format : 
n_eau_complet$mois <- as.numeric(n_eau_complet$mois)
n_eau_complet$jour <- as.numeric(n_eau_complet$jour)
n_eau_complet$annee <- as.character(n_eau_complet$annee)#passer en character
n_eau_complet$annee <- as.numeric(n_eau_complet$annee)#puis le passer en numeric 

#creation du jdd des niveaux d'eaux final --> printemps 2000/2022
n_eau<- subset(n_eau_complet, annee >=2000)# garder les annees sup ou egal à 2000 
n_eau<- subset(n_eau, mois == "4" | mois == "5" | mois == "6" | mois == "7")#garder les mois de spring
summary(n_eau)
n_eau$date <- as.Date(n_eau$date)
n_eau$jj <- c(1:nrow(n_eau))

#Graphique : 

n_eau_2001 <- subset(n_eau, annee == 2001)
n_eau_2001$jj_y <- c(1:nrow(n_eau_2001))
n_eau_2002<- subset(n_eau, annee == 2002)
n_eau_2002$jj_y<- c(1:nrow(n_eau_2002))
ggplot() +
  geom_line(data = n_eau_2001,aes(x = jj_y, y = hauteur, color = "2001")) +
  geom_line(data = n_eau_2002,aes(x = jj_y, y = hauteur,color = "2002" ) ) +
  scale_color_manual(values = c("blue", "red")) +
  #scale_y_continuous(name = "TM",limits = c(min(n_eau$TM), max(n_eau$TM)))+
  labs(x = "jj_y",y = "hauteur", title = "hauteur niveau d'eau ")

#n_eau$mois <- factor(n_eau$mois, levels = month.name)

ggplot(n_eau, aes(x = date, y = hauteur, group = as.factor(annee), color = as.factor(annee))) +
  geom_line() +
  labs(x = "date", y = "Hauteur (m)", title = "Hauteur du niveau d'eau par année") +
  scale_color_manual(values = rev(rainbow(length(unique(n_eau$annee))))) +
  theme_bw()

library(gridExtra)

# Créer une liste pour stocker les graphiques
plots_list <- list()

# Boucle pour créer un graphique pour chaque année
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
hist(PE_obs$ABONDANCE) #bcp bcp de petites valeurs#att valeurs extremes
summary(PE$ABONDANCE)
summary(PE_obs$ABONDANCE)#+ de 50% des valeurs sont des 1 ; max a 700 indiv
table(PE_obs$ABONDANCE)
table(PE$ABONDANCE)
table(PE$ABONDANCE >= 1)
table(PE$ABONDANCE >=1)[2]/length(PE$ABONDANCE)*100 #pour differencier le 0 des autres 

mean(PE_obs$ABONDANCE); sd((PE_obs$ABONDANCE)) ; var(PE_obs$ABONDANCE)
#variance +++ a cause des quelques valeurs tres hautes ? 






####### b - La variable SITE : les points d ecoutes #######

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






####### c - La variable ANNEE #######
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
#les infos des regimes alimentaires sont stockees dans le jdd "geb" 
#bien que je ne comprends pas tous les noms de variables, 
#on va regarder tout ca : 

geb_ss <- merge(geb, liste_esp, by.y = "nom_espece", by.x = "code"  )

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
par(las = 2)
barplot(fruit, main="mangeurs de fruits",
        ylab="nombre despeces mangeuses de fruits",
        col=c("blue", "red"), cex.names = 0.5)


#Proportion de mangeurs d'invertebres :
par(las = 2)
barplot(invert, main="mangeurs d invertebre",
        ylab="nombre despeces mangeuses d 'invert",
        col=c("blue", "red"), cex.names = 0.5)


#Proportion de mangeurs de poissons :
par(las = 2)
barplot(fish, main="mangeurs de poissons",
        ylab="nombre despeces mangeuses de poissons",
        col=c("blue", "red"), cex.names = 0.5)



#Poids des espèces (pas tres pertinent)
geb_ss<-geb_ss[order(geb_ss$e.bodymass.g.),]
par(las = 2)
barplot(geb_ss$e.bodymass.g., main="Poids des espèces d'oiseaux de grand lieu",
        xlab="espece", ylab="masse",names.arg = geb_ss$code,
        col=c("blue", "red"), cex.names = 0.5)

####### g - Le poids des oiseaux #######
