for (i in 2:li) {#les lignes
  date <- pod[i,1]
  lieu <- pod[i,2]
  for (j in  5:co){#les colonnes
    ab <- pod[i,j] 
    if (is.na(ab)) {#suppression des NA
      ab <- 0    }
    espece <-pod[1,j]
    print(paste(date,lieu,espece, ab))
  }
}




for (i in 2:li) {#les lignes
  date <- pod[i,1]
  lieu <- pod[i,2]
  for (j in  5:co){#les colonnes
    ab <- pod[i,j] 
    if (is.na(ab)) {#suppression des NA
      ab <- 0    }
    espece <-nomcol[j]
    print(paste(date,",",lieu,",",espece,",", ab))
  }
}




#lancement de la boucle pour passer les colonnes especes en lignes 
for (i in 2:li) {#les lignes
  date <- pod[i,1]
  lieu <- pod[i,2]
  for (j in  5:co){#les colonnes
    ab <- pod[i,j] 
    if (is.na(ab)) {#suppression des NA
      ab <- 0    }
    espece <-nomcol[j]
    d2 <-paste(date,";",lieu,";",espece,";", ab)
    d3 <-paste(d3,"\n", d2) #creation du nouveau jdd 
    