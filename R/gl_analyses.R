############"
#
#
#########         GL ANALYSES                #####
#
#
#
#############"

####### CHARGEMENT ET MISE EN FORME DU JDD #####
library(readxl)
PE <- read_excel("C:/git/Grand_lieu/DATA/PE.xlsx",
                  col_names = TRUE)
###### annee en facteur 
PE$ANNEE_txt <- as.factor(PE$ANNEE)
library(data.table)
setnames(PE,"ESPECE","CODE")
PE$CODE <- as.factor(PE$CODE)
PE$SITE <- as.factor(PE$SITE)
summary(PE)
####### annee centre et reduite en numerique 
PE$annee_sc = scale(PE$ANNEE)


###### enlever les especes avec trop peu de données (PE2)
#creer variable avec le nombre d'occurence de chaque esp
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x == 1))
occ_esp
#tapply permet de compter le nombre de fois ou il y a 1 
# Identifier les espèces qui ont moins de 100 observations 
rare_species <- names(occ_esp[occ_esp < 100])
rare_species
# Filtrer le jeu de données pour exclure les espèces rares
PE2 <- PE[!(PE$CODE %in% rare_species),]




####### autres jdd
library(readxl)
info_esp <- read_excel("C:/git/Grand_lieu/DATA/info_especes.xlsx",
                 col_names = TRUE)
pod_site <- read_excel("C:/git/Grand_lieu/DATA/pod_site2.xlsx",
                 col_names = TRUE)#mettre les donnees habitats 
n_eau <- read_excel("C:/git/Grand_lieu/DATA/table_niveau_eau.xlsx",
                       col_names = TRUE)
meteo <- read_excel("C:/git/Grand_lieu/DATA/meteo_gl_final.xlsx",
                       col_names = TRUE)


######## ANALYSES JDD COMPLET ####### 

###### PRE TRAITEMENT DU JDD ######

###### TRI --> enlever les esp trop rares #####
#Apres avoir fait un test de puissance (voir script dedié), on ne garde que les esp 
# qui ont + de 50 occurences sur l'ensemble du jeu de donnees 

###### enlever les especes avec trop peu de données (PE2) ####
#creer variable avec le nombre d'occurence de chaque esp
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x >0)) ; occ_esp #occurence de chaque esp
rare_species_50 <- names(occ_esp[occ_esp < 50]) ; rare_species_50# liste des esp rares
#tapply permet de compter le nombre de fois ou il y a une presence d'oiseaux 


###### Filtrer le jeu de données pour exclure les espèces rares #####
PE2 <- PE[!(PE$CODE %in% rare_species_50),]
head(PE2)

###### correlation entre variables expl #####
cor.test(elev,forest)#pas de correlation lineaire entre les variables
#on observe peut etre une correlation, mais pas lineaire
plot(elev,forest)# tres grande variance 
lines(lowess(elev,forest))#creer une ligne de tendance avec la fonction lowess



library(glmmTMB)#package pour faire glmm

# summary(modb2)
# res.dev<-deviance(modb2)
# nul.dev<-deviance(glm(pres~1,binomial,data1))
# # le pourcentage de déviance expliquée (%DEV) est alors:
# (nul.dev-res.dev)/nul.dev*100 

# %DEV, c'est une (sous-)estimation.
# library(rsq)
# rsq(modb6) 



##### VARIATION D'ABONDANCE D'OISEAUX TOTALE #######
library(glmmTMB)#package pour faire glmm
###### annee en facteur ######
md1 <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) + (1|CODE),data = PE ,  family = nbinom2, ziformula = ~1)
#glmmTMB(var rep ~ var expl eff fixe + (1| var expl eff aleatoire), data = data, family = type de distribution)
#effet aleatoire des esp car 2 points d'une meme espece ne sont pas aleatoire 
#Ici, on a bcp de 0, ce qui cree un ecart a la moyenne
#ziformula = ~1 permet de prendre en compte le "trop plein" de 0 (betement recopie)
#la distribution negative binomiale est une distribution de comptage, utilise en cas de surdispersion 
#il en existe 2, nbinom1 et nbinom2 
#nbinom2 plus precise ? 
smd1 <- summary(md1)
print(smd1)
#ANALYSES DES RESIDUS
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md1, plot = F)#met du temps 
testZeroInflation(simulationOutput)
plot(simulationOutput)

confint(md1)#donne les intervalles de confiance avec l'estimate 

# et puis ne pas oublier un modèle nul, qui servira à évaluer la performance absolue du modèle sélectionné
modb0<-glm(pres~1, family=binomial, data=data1)
summary(modb0)

# GRAPHIQUES 
library(ggplot2)
library(ggeffects)
ggpred <- ggpredict(md1,terms = c("ANNEE_txt"))
plot(ggpred)
summary(ggpred)
ggpred$x <- as.numeric(as.character(ggpred$x))
ggplot(data = ggpred, aes(x= x)) + 
  geom_point(aes( y= predicted, color = "predict")) +
  geom_line(aes( y= predicted, color = "predict")) +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width=0.1) +
  labs(y="Variation d'abondance",x="Années", title = "Variation d'abondance de l'ensemble des oiseaux en fonction des annees", color = "Legende")
  #geom_pointrange(aes(x = x ,y = predicted,  ymin = conf.low , ymax= conf.high)) # autre methode pour errorbar 

##### annee en numerique #####

######## ANALYSES ESP/ESP ########  

library(glmmTMB)#package pour faire glmm
library(DHARMa)#residus
library(ggplot2)#graph
library(ggeffects)#graph


#espece par espece 




#Phragmite des joncs 
#ss-jeu de donnees 
ACRSCH_data <- subset(PE2, CODE == "ACRSCH")
#### ENLEVER LES SITES VIDES
occ_site <- tapply(ACRSCH_data$ABONDANCE, ACRSCH_data$SITE, function(x) sum(x > 0))
hist(tapply(ACRSCH_data$ABONDANCE, ACRSCH_data$SITE, function(x) sum(x > 0))) 
site_vide <- names(occ_site[occ_site < 1])
site_vide
# Filtrer le jeu de données pour exclure les sites vides 
ACRSCH_data2<- ACRSCH_data[!(ACRSCH_data$SITE %in% site_vide),]
#### ENLEVER LES ANNEES VIDES
occ_year <- tapply(ACRSCH_data$ABONDANCE, ACRSCH_data$ANNEE_txt, function(x) sum(x > 0))
occ_year
# Filtrer le jeu de données pour exclure annees de debut vide
ACRSCH_data3<- ACRSCH_data2[!(ACRSCH_data2$SITE %in% YEAR_vide),]
ifelse(occ_year[2002]== 0, ACRSCH_data2 == ACRSCH_data2[!(ACRSCH_data2$ANNEE_txt == "2002"),])
#j'ai tente mais faudra prendre une esp qui n'a pas toutes les annees
#enlever les annees ou l'esp n'est pas presente et les lieux ou elle ne l'ai pas nn plus 






# Variation d'abondance : Modèle linéaire mixte avec glmmTMB pour chaque espèce
md_va_ACRSCH <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) , data = ACRSCH_data2, family = nbinom2, ziformula = ~1)
summary(md_va_ACRSCH)
#Residus + (1|SITE) 
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md_va_ACRSCH, plot = T)#met du temps 
testZeroInflation(simulationOutput)
# Graphique d'abondance en fonction de l'année 
gg2<- ggpredict(md_va_ACRSCH,terms = c("ANNEE_txt"))
plot_var_ab <- ggplot(gg2, aes(x = x, y = predicted)) +
  geom_point() +
  geom_line(aes(group = 1)) +
 #geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=0.1) + 
  labs(x = "Année", y = "Abondance", title = paste("Variation d'abondance de phragmite des joncs au fil du temps"))
print(plot_var_ab)
# Créer une table pour la sortie du modèle
print(gg2)
ref <- gg2$predicted[1] 
d_pred <- data.frame(annee = gg2$x,abondance_var = gg2$predicted / ref, ICinf = gg2$conf.low/ref , ICsup = gg2$conf.high/ref)
print(d_pred)
plot(d_pred$annee, d_pred$abondance_var, xlab = "Annee", ylab = "abondance")




#tendance de l'espece : Modèle linéaire mixte avec glmmTMB
md_td_ACRSCH <- glmmTMB(ABONDANCE ~ annee_sc + (1|SITE) , data = ACRSCH_data, family = nbinom2, ziformula = ~1)
summary(md_td_ACRSCH)# resume du modèle
#Residus 
simulationOutput <- simulateResiduals(fittedModel = md_td_ACRSCH, plot = T)#met du temps 
testZeroInflation(simulationOutput)
#Graphique de la tendance 
confint(md_td_ACRSCH)
gg3<- ggpredict(md_td_ACRSCH,terms = c("annee_sc"))
plot_var_ab <- ggplot(gg3, aes(x = x, y = predicted)) +
  #geom_point() +
  geom_line(aes(group = 1)) +
  #geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width=0.1) + 
  labs(x = "Année", y = "Abondance", title = paste("Abondance de phragmite des joncs au fil du temps"))
print(plot_var_ab)
print(gg3)
ref <- gg3$predicted[1] 
d_pred <- data.frame(annee = gg3$x,abondance_var = gg3$predicted / ref, ICinf = gg3$conf.low/ref , ICsup = gg3$conf.high/ref)
print(d_pred)
plot(d_pred)
d_pred$annee_orig <- (d_pred$annee * sd(PE$ANNEE)) + mean(PE$ANNEE)


#dernière etape, aprs la selection du meilleur modele 
plot(md_td_ACRSCH, add.smooth=F, which=1, cex=1.6, cex.lab=1.5)  # résidus vs valeurs prédites
res5<-residuals(md_td_ACRSCH)
hist(res5, nclass=20, main="", xlab="Residuals", ylab="Occurrence", cex.lab=1.5,col='grey') # distribution des résidus

library(gridExtra)
# Combinaison de la table de sortie et du graphique en une seule image
#grid.arrange(plot_var_ab, tableGrob(output_table), ncol = 2, widths = c(2, 1))


#GRAPHIQUES


gg2<- ggpredict(mde,terms = c("ANNEE_txt"))
ggplot(gg2, aes(x = x, )) +
  geom_point(aes( y= predicted)) +
  geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width=0.1) + 
  labs(x = "Année", y = "Abondance", title = paste("Abondance de phragmite des joncs au fil du temps"))



















###### Boucle pour faire toutes les especes en meme temps  #####




####################
tri_jdd_esp <- function(x ,d , affiche = TRUE){
  
  #Garder seulement l'espece cible
  esp_data <- subset(d, CODE == x) #d le jdd complet et x le nom de l'espece
  
  #Extraire les sites ou l'esp n'est jamais presente : 
  occ_site <- tapply(esp_data$ABONDANCE, esp_data$SITE, function(x) sum(x > 0))
  site_vide <- names(occ_site[occ_site < 1])
  
  # Filtrer le jeu de données pour exclure les points d'ecoute vide
  esp_data_sanssite <- esp_data[!(esp_data$SITE %in% site_vide),]
  
  if(affiche){
    print(head(esp_data_sanssite))
  }
  return(esp_data_sanssite)
}
esp_data_sanssite


tri_jdd_esp(x = "ACRSCH", d = PE2)

########################



for (i in unique(PE2$CODE)) {
  # Créer un sous-ensemble de données pour chaque espèce
  esp_data <- subset(PE2, CODE == i)
  #enlever les sites ou l'esp n'a jamais ete vu, et enlever les annees avant la premiere donnee # rtrim
  
  # Modèle linéaire mixte avec glmmTMB pour chaque espèce
  mde <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) , data = esp_data,  family = nbinom2, ziformula = ~1)
  
  # Afficher le résumé du modèle
  summary(mde)
  
  # Tracer un graphique d'abondance en fonction de l'année pour chaque espèce
  gg2<- ggpredict(mde,terms = c("ANNEE_txt")) #object mauvais avec gg2 
  ggplot(gg2, aes(x = x, )) +
    geom_point(aes( y= predicted, color = "predict")) +
    geom_line(aes(group = 1)) +
    geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width=0.1) + 
    labs(x = "Année", y = "Abondance", title = paste("Abondance de ", i , " au fil du temps"))
  
  # Sauvegarder le graphique dans un fichier PNG
  ggsave(paste0("var_ab_", i,".png"), width = 6, height = 4) #dpi 72 pour ordi 
}

#ajouter une ligne qui reli les points entre eux 
#faire la tendance en meme temps 
#ecrire la pente sur le graphique avec intervalle de confiance 




# #analyses communautés 
# #analyses especes
# #meteo 
# 
# #stoc : grille de 2km/2km mi mai + mi juin avec 10 points et PE dur 5 min 
# 
# #1ere phase : 
# "calcul des tendances de gl effets fixe de l'annee et aléatoire du site  "
# 
# #faire sous qgis 
# - envoyer carré stoc eps / faire le cercle des 300 km pour savoir quel carré il faut prendre 
# 

# #centrer reduire scail pour faire une distribution normale, sinon intervalle possible à mettre, 
# #apres on peut faire une negative binomiale 
# #quantifier les effets avec centre reduire, à faire sur toutes les variables quantitatives 
# #centre reduire les annees aussi !!
# #faire colonne annee_txt 
# 
# glmmTMB( ABONDANCE ~ ANNEE_txt + (1|site),data = PE ,  family = negbin)#habitat #ZE = 0 (exces de 0)
# 
# install.packages("DHARMa'")#test et agencement du modele #voir page aide 

