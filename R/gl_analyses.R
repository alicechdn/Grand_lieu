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
#annee en facteur : 
PE$ANNEE_txt <- as.factor(PE$ANNEE)
library(data.table)
setnames(PE,"ESPECE","CODE")
PE$CODE <- as.factor(PE$CODE)
PE$SITE <- as.factor(PE$SITE)
summary(PE)

#mettre les donnees habitats 





######## ANALYSES JDD COMPLET ####### 

library(glmmTMB)#package pour faire glmm
md1 <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE),data = PE ,  family = nbinom2, ziformula = ~1)
#glmmTMB(var rep ~ var expl eff fixe + (1| var expl eff aleatoire), data = data, family = type de distribution)
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
  #geom_pointrange(aes(x = x ,y = predicted,  ymin = conf.low , ymax= conf.high)) 
  

######## ANALYSES ESP/ESP ########  

library(glmmTMB)#package pour faire glmm
library(ggplot2)
library(ggeffects)

#centrer reduire 


###### Enlever les especes avec trop peu de données ####

#creer variable avec le nombre d'occurence de chaque esp
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x == 1))
occ_esp
#tapply permet de compter le nombre de fois ou il y a 1 


# Identifier les espèces qui ont moins de 100 observations 
rare_species <- names(occ_esp[occ_esp < 100])
rare_species
# Filtrer le jeu de données pour exclure les espèces rares
PE2 <- PE[!(PE$CODE %in% rare_species),]

###### Boucle pour faire toutes les especes en meme temps  

for (i in unique(PE2$CODE)) {
  # Créer un sous-ensemble de données pour chaque espèce
  esp_data <- subset(PE2, CODE == i)
  
  # Modèle linéaire mixte avec glmmTMB pour chaque espèce
  mde <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) , data = esp_data)
  
  # Afficher le résumé du modèle
  summary(mde)
  
  # Tracer un graphique d'abondance en fonction de l'année pour chaque espèce
  gg2<- ggpredict(mde,terms = c("ANNEE_txt"))
  ggplot(gg2, aes(x = x, )) +
    geom_point(aes( y= predicted, color = "predict")) +
    geom_errorbar(aes(x = x, ymin = conf.low, ymax = conf.high), width=0.1) + 
    labs(x = "Année", y = "Abondance", title = paste("Abondance de", i , " au fil du temps"))
  
  # Sauvegarder le graphique dans un fichier PNG
  ggsave(paste0(i, ".png"), width = 8, height = 6, dpi = 300)
}

#ajouter une ligne qui reli les points entre eux 




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
