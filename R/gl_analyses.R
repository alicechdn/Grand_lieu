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






####### autres jdd
library(readxl)
info_esp <- read_excel("C:/git/Grand_lieu/DATA/info_especes.xlsx",
                 col_names = TRUE)
pod_site <- read_excel("C:/git/Grand_lieu/DATA/pod_site3.xlsx",
                 col_names = TRUE)#mettre les donnees habitats 
n_eau <- read_excel("C:/git/Grand_lieu/DATA/table_niveau_eau.xlsx",
                       col_names = TRUE)
meteo <- read_excel("C:/git/Grand_lieu/DATA/meteo_gl_final.xlsx",
                       col_names = TRUE)

####### PRE TRAITEMENT DU JDD ######


#### enlever les especes avec trop peu de données (PE2) ####
#Apres avoir fait un test de puissance (voir script dedié), on ne garde que les esp 
# qui ont + de 50 occurences sur l'ensemble du jeu de donnees 
#creer variable avec le nombre d'occurence de chaque esp
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x >0)) ; occ_esp #occurence de chaque esp
rare_species_50 <- names(occ_esp[occ_esp < 50]) ; rare_species_50# liste des esp rares
#tapply permet de compter le nombre de fois ou il y a une presence d'oiseaux 
###### Filtrer le jeu de données pour exclure les espèces rares :
PE2 <- PE[!(PE$CODE %in% rare_species_50),]
head(PE2)












# summary(modb2)
# res.dev<-deviance(modb2)
# nul.dev<-deviance(glm(pres~1,binomial,data1))
# # le pourcentage de déviance expliquée (%DEV) est alors:
# (nul.dev-res.dev)/nul.dev*100 

# %DEV, c'est une (sous-)estimation.
# library(rsq)
# rsq(modb6) 

############'
#
########  ANALYSES COMMUNAUTE  #######
#
###########'

##### Les indices de diversite : #####

library(vegan)
# je dois obtenir un nombre d'individus par an 
#le fait d'utiliser des données de comptage, complique non ? 
#Je vais quand meme tenter... permet tout de meme de visualiser des variations non ? 

#### rs : ####

year <- c(2002:2022)
for (i in year) {
  PE_year <- PE[PE$ANNEE_txt == i,]
  tab_diversity <- aggregate(ABONDANCE~ CODE, data = PE_year, FUN = sum)
  tab_diversity2 <- tab_diversity[tab_diversity$ABONDANCE >0,]
  RS <- nrow(tab_diversity2)
  tab_xx<- data.frame(i,RS) 
  if (i == year[1]) {
    tab_RS<-tab_xx } 
  else { tab_RS <- rbind(tab_RS, tab_xx) }
  print(tab_RS) }


plot(tab_RS, type = "b", main = " Richesse spécifique pour chaque année", 
     xlab = "ANNEE",) 

#### simpson / shannon ####

year <- c(2002:2022)
for (i in year) {
  PE_type <- PE[PE$type == 0,]#ne prendre que le meme type de comptage 
  PE_year <- PE_type[PE_type$ANNEE_txt == i,]
  tab_diversity <- aggregate(ABONDANCE~ CODE, data = PE_year, FUN = sum)
  simpson <- diversity(tab_diversity$ABONDANCE, index = "simpson")
  pielou<- diversity(tab_diversity$ABONDANCE, index = "shannon")
  tab_x<- data.frame(i,simpson, pielou) 
  
  if (i == year[1]) {
    tab_DIV<-tab_x } 
  else { tab_DIV <- rbind(tab_DIV, tab_x) }
  print(tab_DIV) }

plot(tab_DIV$simpson~ tab_DIV$i, type = "b",xlab = "Année", ylab= "Indice", 
     main = "Variation de l'indice de simpson en fonction du temps")
plot(tab_DIV$pielou~ tab_DIV$i, type = "b",
     main = "Variation de l'indice de pielou en fonction du temps", 
     xlab = "Année", ylab= "Indice")


library(glmmTMB)
md_test_indice<- glmmTMB(simpson ~ i , data = tab_DIV ,  family =gaussian)
summary(md_test_indice)




##### Les modeles statistiques : #####
library(glmmTMB)#package pour faire glmm
#### annee en facteur ######
md1 <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) + (1|CODE),data = PE2 ,  family = nbinom2)# ziformula = ~1
smd1 <- summary(md1) ; print(smd1)
#ANALYSES DES RESIDUS
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md1, plot = F)#met du temps 
testZeroInflation(simulationOutput)#pvalue significative alors il y a exces de 0
plot(simulationOutput)

confint(md1)#donne les intervalles de confiance avec l'estimate 

# et puis ne pas oublier un modèle nul, qui servira à évaluer la performance absolue du modèle sélectionné
# modb0<-glm(pres~1, family=binomial, data=data1)
# summary(modb0)

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


ggpred <- ggpredict(md1,terms = c("ANNEE_txt"))
print(ggpred)
plot(ggpred)

#Commencer la série temporelle à 1 
ref <- ggpred$predicted[1] 
d_pred <- data.frame(annee = ggpred$x,abondance_var = ggpred$predicted / ref, ICinf = ggpred$conf.low/ref , ICsup = ggpred$conf.high/ref)
print(d_pred) ; summary(d_pred)
d_pred$annee <- as.numeric(as.character(d_pred$annee))
plot(d_pred$abondance_var~d_pred$annee)

write.csv(d_pred, file = "d_pred_ab_totale.csv", row.names = TRUE)

#tester ce code pour le modèle complet 
library(ggplot2)

# Créer le graphique en utilisant ggplot()
graph_varab <- ggplot(data = d_pred, aes(x = annee, y = abondance_var)) +
  geom_line(aes(group = 1)) +# Ligne pour représenter la tendance
  geom_errorbar(aes( ymin = ICinf, ymax = ICsup), width=0.1, alpha = 0.2) +
  xlab("Année") +  # Étiquette de l'axe x
  ylab("Abondance Variable") +  # Étiquette de l'axe y
  ggtitle("Évolution de l'abondance  au fil du temps")  # Titre du graphique

# Afficher le graphique
print(graph_varab)











#### annee en numerique #####

library(glmmTMB)
md_num <- glmmTMB(ABONDANCE ~ annee_sc + (1|SITE) + (1|CODE),data = PE2 ,  family = nbinom2, ziformula = ~1)
smd_num <- summary(md_num)
print(smd_num)


#### modele avec habitat ######
PE_hab <- merge(PE2, pod_site,all.x = TRUE, by.x = "SITE", by.y = "Site" )
PE_hab$derangement <- as.factor(PE_hab$derangement)
PE_hab$protec <- as.factor(PE_hab$protec)
PE_hab$arbres <- as.factor(PE_hab$arbres)
PE_hab$buissons <- as.factor(PE_hab$buissons)
PE_hab$aqua <- as.factor(PE_hab$aqua)
PE_hab$urbain <- as.factor(PE_hab$urbain)
summary(PE_hab)
library(glmmTMB)
library(tictoc)
tic()
md_num <- glmmTMB(ABONDANCE ~ annee_sc + arbres + buissons + aqua + urbain + derangement + protec + (1|SITE) + (1|CODE),data = PE_hab ,  family = nbinom2)
toc()
smd_num <- summary(md_num)
print(smd_num)

plot(PE_hab$ABONDANCE~ PE_hab$derangement)
hist(PE_hab$ABONDANCE~ PE_hab$derangement)




#### modele avec meteo ######

PE_meteo <- merge(PE2, meteo,all.x = TRUE, by.x = "ANNEE_txt", by.y = "Date_y" )

###### correlation entre variables expl
library(PerformanceAnalytics)
Z<-cbind(PE_meteo$TM,PE_meteo$RR, PE_meteo$RR_sum, PE_meteo$RR_sum_spring, PE_meteo$TM_spring, PE_meteo$j_rude)
colnames(Z)<-c( "TM", "RR", "RR_sum", "RR_sum_spring", "TM_spring", "j_rude")
chart.Correlation(Z,histogram = TRUE)


summary(PE_meteo)
library(glmmTMB)
library(tictoc)
tic()
md_meteo<- glmmTMB(ABONDANCE ~ annee_sc + RR_sum_spring + TM_spring + j_rude + (1|SITE) + (1|CODE),data = PE_meteo ,  family = nbinom2)
toc()
smd_meteo <- summary(md_meteo)
print(smd_meteo)

#### modele migrateur/non migrateur ####### 


#garder migration 1 2 et 3 
select_migr <- subset(info_esp, select = c("ESPECE","NOM_FR_BIRD", "migration_1", "migration_2", "migration_3"))
jdd_migr <- merge(PE2, select_migr, by.x = "CODE", by.y = "ESPECE")
summary(jdd_migr)
jdd_migr$migration_1 <- as.factor(jdd_migr$migration_1)
jdd_migr$migration_2 <- as.factor(jdd_migr$migration_2)
jdd_migr$migration_3 <- as.factor(jdd_migr$migration_3)
#barplot(jdd_migr$ABONDANCE~jdd_migr$migration_2)

#EXPLORATION 
table(select_migr$migration_1)
table(select_migr$migration_2)
table(select_migr$migration_3)

#Analyses : 
library(glmmTMB)
#Avec migration 2 
md_migr <- glmmTMB(ABONDANCE ~ annee_sc + migration_2 + (annee_sc:migration_2)+ (1|SITE) + (1|CODE),data = jdd_migr ,  family = nbinom2)
smd_migr <- summary(md_migr) ; print(smd_migr)

#Analyses des résidus : 
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md_migr, plot = T)#met du temps 
testZeroInflation(simulationOutput)#la ligne rouge indique la proportion attendue de zéros dans la distribution des résidus

#Avec migration 3
md_migr3 <- glmmTMB(ABONDANCE ~ annee_sc + migration_3 + (annee_sc:migration_3)+ (1|SITE) + (1|CODE),data = jdd_migr ,  family = nbinom2)
smd_migr <- summary(md_migr) ; print(smd_migr)


#comment faire avec les NA des migrations ? Quelles conclusions tirer de ces resultats ?
#les diff migrations sont contradictoires entre elles, comment se decider ? 

#### modele régime alimentaire #####

#Version simple, on ne garde que diet : 
#garder diet 
select_RA <- subset(info_esp, select = c("ESPECE","NOM_FR_BIRD", "Diet"))
select_RA_PE2 <- subset(select_RA, select_RA$ESPECE %in% PE2$CODE)
jdd_RA <- merge(PE2, select_RA, by.x = "CODE", by.y = "ESPECE")
summary(jdd_RA)
jdd_RA$Diet <- as.factor(jdd_RA$Diet)

#exploration 
hist(table(select_RA_PE2$Diet))
table(select_RA_PE2$Diet)
barplot(table(select_RA_PE2$Diet))
#Analyses : 
library(glmmTMB)
md_RA <- glmmTMB(ABONDANCE ~ annee_sc * Diet + (1|SITE) + (1|CODE),data = jdd_RA ,  family = nbinom2)
smd_RA <- summary(md_RA) ; print(smd_RA)

#Analyses des résidus : 
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md_RA, plot = T)#met du temps 
testZeroInflation(simulationOutput)#la ligne rouge indique la proportion attendue de zéros dans la distribution des résidus


#Version + detaillee des regimes alimentaires : 
select_rad <- subset(info_esp, select = c("ESPECE", "e.seeds.nuts.grain", "e.fruits.frugivory" , "e.vegitative", "e.invert", "e.fish", "e.v.sm.mammals", "e.lg.mammals", "e.herptiles", "e.sm.birds", "e.vert", "e.lg.bones", "e.carrion"))
jdd_rad <- merge(PE2, select_rad, by.x = "CODE", by.y = "ESPECE")

### description 
e.invert
select_rad$e.invert <- as.numeric(select_rad$e.invert)
sum(select_rad$e.invert == 1)

######"
#"e.v.sm.mammals", "e.lg.mammals", "e.herptiles", "e.sm.birds", "e.vert", "e.lg.bones", "e.carrion"
jdd_rad$e.seeds.nuts.grain <- as.factor(jdd_rad$e.seeds.nuts.grain)
jdd_rad$e.fruits.frugivory <-as.factor(jdd_rad$e.fruits.frugivory)
jdd_rad$e.vegitative <- as.factor(jdd_rad$e.vegitative)
jdd_rad$e.invert <- as.factor(jdd_rad$e.invert)
jdd_rad$e.v.sm.mammals <- as.factor(jdd_rad$e.v.sm.mammals)
jdd_rad$e.lg.mammals <- as.factor(jdd_rad$e.lg.mammals)
jdd_rad$e.herptiles <- as.factor(jdd_rad$e.herptiles)
jdd_rad$e.sm.birds <- as.factor(jdd_rad$e.sm.birds)
jdd_rad$e.vert <- as.factor(jdd_rad$e.vert)
jdd_rad$e.lg.bones <- as.factor(jdd_rad$e.lg.bones) #inutile car 0 oiseau avec ce regime
jdd_rad$e.carrion <- as.factor(jdd_rad$e.carrion)

######"


summary(jdd_rad)
barplot(select_rad)

#Correlation entre regime : 
###### correlation entre variables expl
library(PerformanceAnalytics)
Z<-cbind(jdd_rad$e.v.sm.mammals,jdd_rad$e.lg.mammals, jdd_rad$e.herptiles, jdd_rad$e.sm.birds, jdd_rad$e.vert, jdd_rad$e.carrion, jdd_rad$e.invert, jdd_rad$e.vegitative, jdd_rad$e.fruits.frugivory,jdd_rad$e.seeds.nuts.grain )
colnames(Z)<-c( "e.seeds.nuts.grain", "e.fruits.frugivory" , "e.vegitative", "e.invert", "e.fish", "e.v.sm.mammals", "e.lg.mammals", "e.herptiles", "e.sm.birds", "e.vert", "e.carrion")
chart.Correlation(Z,histogram = TRUE)

#Analyses : 
library(glmmTMB)
md_rad <- glmmTMB(ABONDANCE ~ annee_sc + e.fruits.frugivory + e.seeds.nuts.grain
                  + e.vegitative + e.invert + e.v.sm.mammals + e.lg.mammals + e.herptiles
                  + e.sm.birds+  e.vert + e.carrion + (1|SITE) + (1|CODE)
                  ,data = jdd_rad ,  family = nbinom2)
smd_rad <- summary(md_rad)
print(smd_rad)

####modele avec ssi et sti  #####

select_ssi <- subset(info_esp, select = c("ESPECE","NOM_FR_BIRD", "ssi", "sti"))
jdd_ssi <- merge(PE2, select_ssi, by.x = "CODE", by.y = "ESPECE")
esp_data_ssi <- aggregate(ABONDANCE ~ CODE, data = jdd_ssi, FUN = sum)
esp_data_ssi2 <- merge(esp_data_ssi, select_ssi, by.x = "CODE", by.y = "ESPECE")

#centrerreduire 
jdd_ssi$ssi_sc = scale(jdd_ssi$ssi)
jdd_ssi$sti_sc = scale(jdd_ssi$sti)

summary(jdd_ssi)
summary(select_ssi)
summary(esp_data_ssi)
plot(esp_data_ssi)
hist(select_ssi$ssi)#pas tres loin du gaussien 
hist(select_ssi$sti)


#correlation : 
library(PerformanceAnalytics)
Z<-cbind(select_ssi$ssi, select_ssi$sti)
colnames(Z)<-c( "ssi", "sti")
chart.Correlation(Z,histogram = TRUE)#pas de correlation entre ssi et sti
#Visualisation graphique : 
plot(jdd_ssi$ABONDANCE~ jdd_ssi$ssi, type ="b")
plot(jdd_ssi$ABONDANCE~ jdd_ssi$sti)
plot(jdd_ssi$ABONDANCE~ jdd_ssi$annee_sc)
#Connaitre le jdd : 
table(select_ssi$ssi)
table(select_ssi$sti)
#distribution des donnees : 
hist(select_ssi$ssi)#pas tres loin du gaussien 
hist(select_ssi$sti)


#Analyses : 
library(glmmTMB)
md_ssi_complet<- glmmTMB(ABONDANCE ~ annee_sc + ssi_sc + sti_sc + ( annee_sc:ssi_sc) + (ssi:sti) + (1|SITE) + (1|CODE)   ,data = jdd_ssi ,  family = nbinom2, na.action = na.exclude )
md_ssi<- glmmTMB(ABONDANCE ~ annee_sc + ssi_sc + sti_sc + (1|SITE) + (1|CODE)   ,data = jdd_ssi ,  family = nbinom2, na.action = na.exclude )


#d_ssi <- glmer(ABONDANCE ~ annee_sc + ssi_sc + sti_sc + (annee_sc:sti_sc)+ (1|SITE) + (1|CODE)   ,data = jdd_ssi ,  family = poisson, na.action = na.exclude )
summary(md_ssi)
library(car)
library(lme4)
Anova(md_ssi)
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md_ssi, plot = T)#met du temps 
testZeroInflation(simulationOutput)
#annee_sc +
#na.exclude me permet de faire l'analyse en gardant les NA (pertinent quand même ?)
smd_ssi <- summary(md_ssi)
print(smd_ssi)
# ?glmmTMB

#pas tres pertinent de faire cette analyse alors qu'il y a bcp desp qui n'ont pas de valeur ? 

##### faire csi, cti ####

###### FAIRE LE CSI ######
head(jdd_ssi)
jdd_ssi$vi_abssi <- jdd_ssi$ABONDANCE*jdd_ssi$ssi
#je ne peux pas prendre le ssi_sc car il a des valeurs négatives 
#Somme des abondances*ssi : 
numerateur_ssi <- aggregate(vi_abssi ~ SITE + ANNEE_txt , data = jdd_ssi, sum) 
#Somme de l'abondance pour chaque site et annee : 
denominateur_ssi <- aggregate(ABONDANCE ~SITE + ANNEE_txt, data = jdd_ssi, sum)
plot(denominateur_ssi$ABONDANCE~denominateur_ssi$ANNEE_txt)
#fusionner les deux jdd temporaires
jdd_csi <- merge(numerateur_ssi,denominateur_ssi, by = c("SITE", "ANNEE_txt"))#on peut merge avec 2 colonnes !
jdd_csi$csi <- jdd_csi$vi_abssi/jdd_csi$ABONDANCE
jdd_csi$annee_num <- as.numeric(as.character(jdd_csi$ANNEE_txt))
plot(jdd_csi$csi~jdd_csi$annee_num)
jdd_csi$csi <- ifelse(jdd_csi$csi == "NaN", 0 ,jdd_csi$csi)#enlever NaN
summary(jdd_csi$csi)
#centrer reduire
jdd_csi$annee_sc = scale(jdd_csi$annee_num)
jdd_csi$csi_sc = scale(jdd_csi$csi)

#changer noms incoherent 
library(data.table)
setnames(jdd_csi,"ABONDANCE","ABONDANCE_PE")

#ANALYSES DU CSI, generaliste/specialiste par la communaute

#Correlation 
library(PerformanceAnalytics)
Z<-cbind(jdd_csi$csi, jdd_csi$ABONDANCE, jdd_csi$vi_abssi )
colnames(Z)<-c( "csi"," somme abondance par PE", " vi, somme ab fois ssi")
chart.Correlation(Z,histogram = TRUE)

#modele
library(glmmTMB)
md_csi_complet<- glmmTMB(ABONDANCE_PE ~ annee_sc + csi_sc + (annee_sc:csi_sc) + (1|SITE),data = jdd_csi ,  family = nbinom2, na.action = na.exclude)
md_csi<- glmmTMB(ABONDANCE_PE ~ annee_sc + csi_sc + (1|SITE),data = jdd_csi ,  family = nbinom2, na.action = na.exclude)
summary(md_csi_complet)
summary(md_csi)

#est ce que le csi evolue en fonction du temps ? 
md_test<- glmmTMB(csi ~ annee_sc + ABONDANCE_PE + (annee_sc:ABONDANCE_PE) + (1|SITE),data = jdd_csi ,  family = gaussian, na.action = na.exclude)
summary(md_test)
md_test2<- glmmTMB(csi ~ annee_sc + ABONDANCE_PE  + (1|SITE),data = jdd_csi,
                  family = gaussian, na.action = na.exclude)
md_test3<- glmmTMB(csi ~ annee_sc  + (1|SITE),data = jdd_csi,
                   family = gaussian, na.action = na.exclude)

summary(md_test2)

#Analyses des résidus : 
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = md_test3, plot = T)#met du temps 
testZeroInflation(simulationOutput)#la ligne rouge indique la proportion attendue de zéros dans la distribution des résidus


#On va un peu trainer sur cette variable pour mieux comprendre ce qu'il en est #graphiquement 
library(ggplot2)
library(ggthemes)
#Variation de certains point d'écoute en fonction du temps 
point_csl <- subset(jdd_csi, SITE == "Communal de Saint-Lumine")
point_bl <- subset(jdd_csi, SITE == "Blourie")

ggplot() +
  geom_line(data = point_csl,aes(x = annee_num, y = csi, color = "csl")) +
  geom_line(data = point_bl,aes(x = annee_num, y = csi,color = "bl" ) ) +
  #scale_color_manual(values = c("blue", "red")) +
  #scale_y_continuous(name = "TM",limits = c(min(n_eau$TM), max(n_eau$TM)))+
  labs(x = "année",y = "csi",
       title = "Variation du csi pour les points d'ecoute de blourie et de communal de st lumine", color = "Legende :") +
  theme_bw() 
#la fonction theme() permet de changer l'apparence du graphique, avec size pour la police en encore family 


#Superposition de toutes les PE (119 courbes)
ggplot(jdd_csi, aes(x = annee_num, y = csi, group = as.factor(SITE) )) +
  geom_line() +# facet_wrap(.~annee,scales = "free_y")
  labs(x = "Annee", y = "csi", title = "variation du csi pour chaque PE", color = "Légende :") +
  #scale_color_manual(values = rev(rainbow(length(unique(jdd_csi$SITE))))) +
  theme_bw()
# on peut mettre des couleurs differentes en faisant #as.factor(SITE) mais legendes prennent trop de place #, color =  blues9










###### FAIRE LE CTI ######

jdd_ssi$vi_absti <- jdd_ssi$ABONDANCE*jdd_ssi$sti
#je ne peux pas prendre le sti_sc car il a des valeurs négatives 
#Somme des abondances*sti : 
numerateur_sti <- aggregate(vi_absti ~ SITE + ANNEE_txt , data = jdd_ssi, sum) 
#Somme de l'abondance pour chaque site et annee : 
denominateur_sti <- aggregate(ABONDANCE ~SITE + ANNEE_txt, data = jdd_ssi, sum)
plot(denominateur_sti$ABONDANCE~denominateur_sti$ANNEE_txt)
#fusionner les deux jdd temporaires
jdd_cti <- merge(numerateur_sti,denominateur_sti, by = c("SITE", "ANNEE_txt"))#on peut merge avec 2 colonnes !
jdd_cti$cti <- jdd_cti$vi_absti/jdd_cti$ABONDANCE
jdd_cti$annee_num <- as.numeric(as.character(jdd_cti$ANNEE_txt))
plot(jdd_cti$cti~jdd_cti$annee_num)
#--> remplacer les NA par 0 
jdd_cti$cti <- ifelse(jdd_cti$cti == "NaN", 0 ,jdd_cti$cti)
#--> scale annee, cti 
jdd_cti$annee_sc = scale(jdd_cti$annee_num)
jdd_cti$cti_sc = scale(jdd_cti$cti)

#changer noms incoherent 
library(data.table)
setnames(jdd_cti,"ABONDANCE","ABONDANCE_PE")

#ANALYSES DU CTI, especes climat chaud/ climat froid 

#Correlation 
library(PerformanceAnalytics)
Z<-cbind(jdd_cti$cti, jdd_cti$ABONDANCE_PE, jdd_cti$vi_absti )
colnames(Z)<-c( "cti"," somme abondance par PE", " vi, somme ab fois sti")
chart.Correlation(Z,histogram = TRUE)

#modele
library(glmmTMB)
md_cti_complet<- glmmTMB(ABONDANCE_PE ~ annee_sc + cti_sc + (annee_sc:cti_sc) + (1|SITE),data = jdd_cti ,  family = gaussian, na.action = na.exclude)
md_cti<- glmmTMB(ABONDANCE_PE ~ annee_sc + cti_sc + (1|SITE),data = jdd_cti ,  family = gaussian, na.action = na.exclude)
summary(md_cti_complet)
summary(md_cti)


plot(jdd_cti$ABONDANCE_PE~jdd_cti$cti)


###### et sans les étourneaux ? #####



jdd_ssi_etour <- subset(jdd_ssi, CODE != "STUVUL")


jdd_ssi_etour$vi_absti <- jdd_ssi_etour$ABONDANCE*jdd_ssi_etour$sti
#je ne peux pas prendre le sti_sc car il a des valeurs négatives 
#Somme des abondances*sti : 
numerateur_sti <- aggregate(vi_absti ~ SITE + ANNEE_txt , data = jdd_ssi_etour, sum) 
#Somme de l'abondance pour chaque site et annee : 
denominateur_sti <- aggregate(ABONDANCE ~SITE + ANNEE_txt, data = jdd_ssi_etour, sum)
plot(denominateur_sti$ABONDANCE~denominateur_sti$ANNEE_txt)
#fusionner les deux jdd temporaires
jdd_cti_etour <- merge(numerateur_sti,denominateur_sti, by = c("SITE", "ANNEE_txt"))#on peut merge avec 2 colonnes !
jdd_cti_etour$cti <- jdd_cti_etour$vi_absti/jdd_cti_etour$ABONDANCE
jdd_cti_etour$annee_num <- as.numeric(as.character(jdd_cti_etour$ANNEE_txt))
plot(jdd_cti_etour$cti~jdd_cti_etour$annee_num)
#--> remplacer les NA par 0 
jdd_cti_etour$cti <- ifelse(jdd_cti_etour$cti == "NaN", 0 ,jdd_cti_etour$cti)
#--> scale annee, cti 
jdd_cti_etour$annee_sc = scale(jdd_cti_etour$annee_num)
jdd_cti_etour$cti_sc = scale(jdd_cti_etour$cti)

#changer noms incoherent 
library(data.table)
setnames(jdd_cti_etour,"ABONDANCE","ABONDANCE_PE")

plot(jdd_cti_etour$ABONDANCE_PE~jdd_cti_etour$cti)

#ANALYSES DU CTI, especes climat chaud/ climat froid 

#Correlation 
library(PerformanceAnalytics)
Z<-cbind(jdd_cti_etour$cti, jdd_cti_etour$ABONDANCE_PE, jdd_cti_etour$vi_absti )
colnames(Z)<-c( "cti"," somme abondance par PE", " vi, somme ab fois sti")
chart.Correlation(Z,histogram = TRUE)

#modele
library(glmmTMB)
md_cti_etour<- glmmTMB(ABONDANCE_PE ~ annee_sc + cti_sc + (annee_sc:cti_sc) + (1|SITE),data = jdd_cti_etour ,  family = gaussian, na.action = na.exclude)
summary(md_cti_etour)


#######

#est ce que le cti evolue en fonction du temps ? 
library(glmmTMB)
md_test<- glmmTMB(cti ~ annee_sc + ABONDANCE_PE  + (1|SITE),data = jdd_cti ,  family = gaussian, na.action = na.exclude)
summary(md_test)
md_test2<- glmmTMB(csi ~ annee_sc + ABONDANCE_PE  + (1|SITE),data = jdd_csi,
                   family = gaussian, na.action = na.exclude)
md_test3<- glmmTMB(csi ~ annee_sc  + (1|SITE),data = jdd_csi,
                   family = gaussian, na.action = na.exclude)






#### modele avec stri #####

select_stri <- subset(info_esp, select = c("ESPECE", "stri"))
jdd_stri <- merge(PE2, select_stri, by.x = "CODE", by.y = "ESPECE")
#centrerreduire 
jdd_stri$stri_sc = scale(jdd_stri$stri)
summary(jdd_stri)
summary(select_stri)

#Analyses : 
library(glmmTMB)
md_stri <- glmmTMB(ABONDANCE ~ annee_sc + stri_sc  + ( annee_sc:stri_sc) + (1|SITE) + (1|CODE),data = jdd_stri ,  family = nbinom2, na.action = na.exclude )
#na.exclude me permet de faire l'analyse en gardant les NA (pertinent quand même ?)
smd_stri <- summary(md_stri) ; print(smd_stri)
# ?glmmTMB

#pas tres pertinent de faire cette analyse alors qu'il y a bcp desp qui n'ont pas de valeur ? 

#faire le ctri, pour voir si la communauté sur chaque PE a évolué ?


jdd_stri$vi_abctri <- jdd_stri$ABONDANCE*jdd_stri$stri
#je ne peux pas prendre le sti_sc car il a des valeurs négatives 
#Somme des abondances*sti : 
numerateur_stri <- aggregate(vi_abctri ~ SITE + ANNEE_txt , data = jdd_stri, sum) 
#Somme de l'abondance pour chaque site et annee : 
denominateur_stri <- aggregate(ABONDANCE ~SITE + ANNEE_txt, data = jdd_stri, sum)
plot(denominateur_stri$ABONDANCE~denominateur_stri$ANNEE_txt)
#fusionner les deux jdd temporaires
jdd_ctri <- merge(numerateur_stri,denominateur_stri, by = c("SITE", "ANNEE_txt"))#on peut merge avec 2 colonnes !
jdd_ctri$ctri <- jdd_ctri$vi_abctri/jdd_ctri$ABONDANCE
jdd_ctri$annee_num <- as.numeric(as.character(jdd_ctri$ANNEE_txt))
plot(jdd_ctri$ctri~jdd_ctri$annee_num)
#--> remplacer les NA par 0 
jdd_ctri$ctri <- ifelse(jdd_ctri$ctri == "NaN", 0 ,jdd_ctri$ctri)
#--> scale annee, ctri 
jdd_ctri$annee_sc = scale(jdd_ctri$annee_num)
jdd_ctri$ctri_sc = scale(jdd_ctri$ctri)
#changer noms incoherent 
library(data.table)
setnames(jdd_ctri,"ABONDANCE","ABONDANCE_PE")

#explorer les donnees 
summary(jdd_ctri$ctri)



#Correlation 
library(PerformanceAnalytics)
Z<-cbind(jdd_ctri$ctri, jdd_ctri$ABONDANCE_PE, jdd_ctri$vi_abctri )
colnames(Z)<-c( "ctri"," somme abondance par PE", " vi, somme ab fois sti")
chart.Correlation(Z,histogram = TRUE)




#modele
library(glmmTMB)
md_ctri_complet<- glmmTMB(ABONDANCE_PE ~ annee_sc + ctri_sc + (annee_sc:ctri_sc) + (1|SITE),data = jdd_ctri ,  family = gaussian, na.action = na.exclude)
md_ctri<- glmmTMB(ABONDANCE_PE ~ annee_sc + ctri_sc + (1|SITE),data = jdd_ctri ,  family = gaussian, na.action = na.exclude)
summary(md_ctri_complet)
summary(md_ctri)

#comment expliquer avoir si peu de resultats avec le stri ? est-ce que ce jeu de données represente bien la chaine trophique 
#j'ai des doutes, bcp insectivores, passereaux ++, peut on vraiment visualiser quelque chose ? 
#si oui, ça voudrait dire qu'il n'y a pas eu de changement d'équilibre dans la chaine trophique ces dernières années ? 


##### modele habitat simple ##### 

select_hab <- subset(info_esp, select = c("ESPECE", "Habitat"))
jdd_hab <- merge(PE2, select_hab, by.x = "CODE", by.y = "ESPECE")
select_hab$Habitat <- as.factor(select_hab$Habitat)
jdd_hab$Habitat <- as.factor(jdd_hab$Habitat)
summary(jdd_hab)
summary(select_hab)

#Analyses : 
library(glmmTMB)
md_hab <- glmmTMB(ABONDANCE ~ annee_sc + Habitat + (annee_sc:Habitat) + (1|SITE) + (1|CODE)   ,data = jdd_hab ,  family = nbinom2, na.action = na.exclude )
#na.exclude me permet de faire l'analyse en gardant les NA (pertinent quand même ?)
smd_hab <- summary(md_hab) ; print(smd_hab)
# ?glmmTMB
PE_hab <- merge(PE2, pod_site,all.x = TRUE, by.x = "SITE", by.y = "Site" )
esp_hab <- merge()























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


