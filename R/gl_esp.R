



######## ANALYSES ESP/ESP ########  

library(glmmTMB)#package pour faire glmm
library(DHARMa)#residus
library(ggplot2)#graph
library(ggeffects)#graph



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
library(ggplot2)
library(ggeffects)
library(ggthemes)
gg2<- ggpredict(md_va_ACRSCH,terms = c("ANNEE_txt"))
plot_var_ab <- ggplot(gg2, aes(x = x, y = predicted)) +
  geom_point() 
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

####################



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

################## a trier plus tard ########### 

# et puis ne pas oublier un modèle nul, qui servira à évaluer la performance absolue du modèle sélectionné
# modb0<-glm(pres~1, family=binomial, data=data1)
# summary(modb0)