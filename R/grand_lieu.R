########## PARTIE CONVERSION DU JEU DE DONNEES ##############"

###### CHARGEMENT DU JDD #######
#setwd("C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA")
library(readxl)
pod <- read_excel("DATA/dataPE.xlsx", 
                    col_names = TRUE)
View(pod)

#autre facon d ouvrir le jdd au cas ou 
#library(rJava)
#library(xlsx)
#library(openxlsx)
#pod <- read.xlsx("dataPE.xlsx", colNames = FALSE)
#ouverture possible avec read_excel ou read.xlsx2 egalement

#VISUALISER ET RESUME DU JDD
summary(pod) #/!\character ?
dim(pod)#nbre de lignes puis de colonnes

pod<-subset(pod, Site != "TOTAL")#supprimer les lignes total 
#View(pod)


###### MISE EN FORME DU JDD #######

###### CREATION DE LA BOUCLE 
#mettre dans une variable la taille des lignes et des colonnes
#permet d automatiser le jdd si celui ci change
li<-nrow(pod)#nbre de lignes
co<-ncol(pod)#nbre de colonnes
nomcol<-colnames(pod)#objet contenant ts les noms des especes

first <- c("asupp","asupp","asupp", 0)#attention la premiere ligne du jdd est mauvaise, il faudra la suppr 
sw2<-matrix(first,byrow = TRUE, nrow = 1)
titre<-c("DATE", "LIEU", "ESPECE", "ABONDANCE")#nom des variables dans la matrice
colnames(sw2) <-titre
sw2

#lancement de la boucle pour passer les colonnes especes en lignes (format long)
for (i in 1:li) {#les lignes
  date <- pod[i,1]
  lieu <- pod[i,2]
  for (j in  5:co){#les colonnes
    ab <- pod[i,j] 
    if (is.na(ab)) {#suppression des NA
      ab <- 0    }
    espece <-nomcol[j]
    sec<-c(date,lieu,espece,ab)
    sw2<-rbind(sw2,sec)    #creation du nouveau jdd 
    d2 <-paste(date,",",lieu,",",espece,",", ab) #visualisation pendant la boucle, pas indispensable
    print(d2)
    
  }
}

#supprimer la première ligne 
sw2 <-  sw2[-1,]
sw2

#rentrer le nouveau jdd dans un csv
write.csv(sw2, file = "PE_jdd.csv", col.names = TRUE, row.names = FALSE, sep = ",")
#file.copy(from = "data_ex_export.csv", to = "data_PE_new.csv") #faire une copie du fichier
#lire le csv
PE<-read.csv2("PE_jdd.csv", header = T, sep = ",")
View(PE)
summary(PE)
#miracle

#redaction du processus de la boucle : 
#Pour la ligne "i" compris entre la ligne 1 et "li" (la derniere ligne)
#entrer dans la variable "date" l'intitule de la premiere colonne pod a la ligne i 
#entrer dans la variable "lieu" l'intitule de la deuxieme colonne pod a la ligne i



