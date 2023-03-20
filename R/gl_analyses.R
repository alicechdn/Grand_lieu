#GL ANALYSES 


library(readxl)
PE <- read_excel("C:/git/Grand_lieu/DATA/PE.xlsx",
                  col_names = TRUE)

#analyses commnautés 
#analyses especes
#meteo 

#stoc : grille de 2km/2km mi mai + mi juin avec 10 points et PE dur 5 min 

#1ere phase : 
"calcul des tendances de gl effets fixe de l'annee et aléatoire du site  "

#faire sous qgis 
- envoyer carré stoc eps / faire le cercle des 300 km pour savoir quel carré il faut prendre 

install.packages("glmmTMB")
#centrer reduire scail pour faire une distribution normale, sinon intervalle possible à mettre, 
#apres on peut faire une negative binomiale 
#quantifier les effets avec entre reduire, à faire sur toutes les variables quantitatives 
#centre reduire les annees aussi !!
#faire colonne annee_txt 

glmmTMB( ABONDANCE ~ ANNEE_txt + (1|site),data = PE ,  family = negbin)#habitat #ZE = 0 (exces de 0)

install.packages("DHARMa'")#test et agencement du modele #voir page aide 
