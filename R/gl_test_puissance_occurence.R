
###### TEST DE PUISSANCE POUR NOMBRE D OCCURENCES 

####### CHARGEMENT ET MISE EN FORME DU JDD #####
library(readxl)
PE <- read_excel("C:/git/Grand_lieu/DATA/PE.xlsx",
                 col_names = TRUE)
###### annee en facteur #####
PE$ANNEE_txt <- as.factor(PE$ANNEE)
library(data.table)
setnames(PE,"ESPECE","CODE")
PE$CODE <- as.factor(PE$CODE) ; PE$SITE <- as.factor(PE$SITE)
summary(PE)
####### annee centre et reduite en numerique ###### 
PE$annee_sc = scale(PE$ANNEE)

library(glmmTMB)#package pour faire glmm

###### Occurences de chaque espece ####
#creer variable avec le nombre d'occurence de chaque esp
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x >0))
occ_esp #correspond au nombre d'occurence de chaque espece
#tapply permet de compter le nombre de fois ou il y a une presence d'oiseaux 
####### Sous categories ######
###### 1ere methode, le faire a la main : 
# Identifier les espèces qui ont moins de n observations 
rare_species_10 <- names(occ_esp[occ_esp < 10])
rare_species_20 <- names(occ_esp[occ_esp < 20])
rare_species_50 <- names(occ_esp[occ_esp < 50])
rare_species_100 <- names(occ_esp[occ_esp < 100])
rare_species_150 <- names(occ_esp[occ_esp < 150])
rare_species_200 <- names(occ_esp[occ_esp < 200])
rare_species_300 <- names(occ_esp[occ_esp < 300])
rare_species_500 <- names(occ_esp[occ_esp < 500])
rare_species_600 <- names(occ_esp[occ_esp < 600])

# Filtrer le jeu de données pour exclure les espèces rares
PE_300 <- PE[!(PE$CODE %in% rare_species_300),]
md_300 <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) + (1|CODE),data = PE_300 ,  family = nbinom2)
summary(md_300)

PE_500 <- PE[!(PE$CODE %in% rare_species_500),]
md_500 <- glmmTMB(ABONDANCE ~ ANNEE_txt + (1|SITE) + (1|CODE),data = PE2 ,  family = nbinom2)
summary(md_500)


#2eme methode, le faire avec des fonctions + des boucles 

r_spf <- function(x, affiche = TRUE){
  assign(paste0("rare_species_",x), names(occ_esp[occ_esp < x]), envir = .GlobalEnv)
  if(affiche){
    str(get(paste0("rare_species_",x)))
  }
}

list_x <- c(10,50,100,150,200,300,500,600)
for (x in list_x) {
  r_spf(x)
}


#la fonction assign permet d'attribuer des valeurs à un object en créant un nom d'objet dynamique

modele_f <- function(x, affiche = TRUE){
  assign(paste0("PE", x), PE[!(PE$CODE %in% paste0("rare_species_",x)),])
  assign(paste0("md_",x), glmmTMB(ABONDANCE ~ annee_sc + (1|SITE) + (1|CODE), data = get(paste0("PE",x)), family = nbinom2))
  if(affiche){
    summary(get(paste0("md_",x)))
  }
}

for (x in list_x) {
  modele_f(x)
}

#Faire une boucle, creer un objet avec et ensuite les comparer entre eux 

#ranger dans une liste les paramètres de ce que je veux puis en faire un test de puissance 
