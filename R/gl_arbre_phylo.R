
### RECUPERER LA DISTANCE PHYLOGENETIQUE ENTRE LES ESPECES #####

#CHARGEMENT DES JDD 

library(readxl)
PE <- read_excel("C:/git/Grand_lieu/DATA/PE_new.xlsx",col_names = TRUE)
info_esp <- read_excel("C:/git/Grand_lieu/DATA/info_especes.xlsx",
                       col_names = TRUE)
summary(info_esp)

library(data.table)
setnames(PE,"ESPECE","CODE")
PE$CODE <- as.factor(PE$CODE)#code CRBPO des esp
PE$SITE <- as.factor(PE$SITE)# Point d'ecoute
PE$ANNEE_txt <- as.factor(PE$ANNEE)#annee en facteur 
PE$annee_sc = scale(PE$ANNEE)#annee en numerique et centre reduite
PEx$type <- as.factor(PEx$type)
summary(PE)
nom_scien <- subset(info_esp, select = c("ESPECE", "scientific_name.x"))
PEx <- merge(PE,nom_scien, by.x = "CODE", by.y = "ESPECE")
#creer variable avec le nombre d'occurence de chaque esp
occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x >0)) ; occ_esp #occurence de chaque esp
rare_species_50 <- names(occ_esp[occ_esp < 50]) ; rare_species_50# liste des esp rares
PEx <- PEx[!(PEx$CODE %in% rare_species_50),]
summary(PEx)




#DEBUT DE LA DEMARCHE : 

###### RECUPERER L'ARBRE #######
#(plus besoin maintenant, que la 1ere fois)

#remotes::install_github("wzmli/phyloglmm/pkg")

#Chargement des packages 
pck <- c("rotl","ape","TreeDist"," ape")
lapply(pck, require, character.only = TRUE)#meme chose que require(rotl)

#faire un vecteur avec tous les noms scientifiques des especes #att bien en character
SP_vec <- c(info_esp$scientific_name.x)
sp_analyse <- c(info_esp2$scientific_name.x)

#Recuperer les arbres qui contiennent "value"
res <- studies_find_trees(property="ot:ottTaxonName", value="Saxicola rubicola",detailed=FALSE)
t1 <- list_trees(res)
furry_meta <- get_study_meta(names(t1[1])) ; get_publication(furry_meta) # The citation for the source of the study

#la version officielle pour enregistrer l'arbre 
get_study_tree(
  study_id = names(t1[1]), tree_id = "tree1", # Ici, je prend l’arbre “tree1” de la permiere etude
  tip_label = "ott_taxon_name", file = "C:/git/Grand_lieu/DATA",
  file_format = "newick")

# Enregistrer l'arbre dans le fichier temporaire #solution qui fonctionne pour moi
temp_file <- tempfile()

get_study_tree(
  study_id = names(t1[1]),
  tree_id = "tree1",
  tip_label = "ott_taxon_name",
  file = temp_file,
  file_format = "newick")

# Lire l'arbre depuis le fichier temporaire dans un objet R
# assignation a une var dans l’environnement de travail
tree <- read.tree(temp_file)

# Afficher l'objet tree #attention fichier lourd
print(tree)
library(ape)
plot(tree)



#je pense que cette ligne est inutile 
#tree$tip.label <- gsub("'","",tree$tip.label) # Miscellaneous, les feuilles des abres de ToL semble venir sous la forme « ‘ Genus specie ‘ » (i.e. « ‘Saxicola rubicola’ »), legerement embetant pour lecture R donc mini regex pour eliminer le double « ‘.



#faire un subset de l'arbre avec nos especes en croisant fort les doigts qu'il y ait la totalite 
#on doit faire avec les noms scientifiques... en esperant qu'ils n'ont pas change... 
tr_sub <- keep.tip(tree,SP_vec) #ON A NOS 99 ESP YOUHOUUUUUUUUU
tr_sub2 <- keep.tip(tree,sp_analyse)
plot(tr_sub)
plot(tr_sub2)


#Exporter notre arbre dans le PC pour ne plus avoir a refaire tout ca : 
write.tree(tr_sub, file = "gl_arbre_99.nwk")
write.tree(tr_sub2, file = "gl_arbre_65.nwk")



####### LANCEMENT DU MODELE ##### 
#UNE FOIS QU'ON A L'ARBRE 


#Chargement des packages 
pck <- c("rotl","ape","TreeDist"," ape")
lapply(pck, require, character.only = TRUE)#meme chose que require(rotl)
library(lme4)
library(glmmTMB)
library(phyloglmm)
ls("package:phyloglmm")





#Maintenant qu'on a l'arbre, il nous faut la distance entre chaque esp 
#il me semble que l'on va devoir utiliser le modele phyloglmtmb 

#pour lire l'arbre 
mon_arbre <- read.tree("mon_arbre.nwk")


mod_glmmp <- phylo_glmmTMB(Y ~ X1 + X2 + … + (1 | SP_NOM_SCIENTIFIQUE), # structure random a ajouter sur les espece
                           data = DATA,
                           phylo = tr_sub, # l’arbre phylo subsette
                           phylonm = "SP_NOM_SCIENTIFIQUE", # colonne du jeu de donne avec les noms scientifiques concordant a l’arbre phylo pour faire la jonction observation / similarite (rapprochement) phylogenetique # DOIT ETRE EN FACTEUR
                           doFit = T, # F le modele ne tourne pas (sort une variable utilisable par TMB ?)
                           dispformula = ~1, # dispersion suivant le modele null
                           REML = T,
                           weights = WEIGHTS # WARNING le modele semble assez sensible aux poids, perso ca marche avec des poids normalize (reduction des poids extreme modulo un poids moyen
)








#ESSAI CONCRET 
library(glmmTMB)
remotes::install_github("wzmli/phyloglmm/pkg")
mod_glmmp <- phylo_glmmTMB(ABONDANCE ~ annee_sc + type + (1|CODE) + (1|SITE), # structure random a ajouter sur les espece
                           data = PEx, 
                           family = nbinom2,
                           phylo = tr_sub2, #ok
                           phylonm = "scientific_name.x", #ok
                           na.action = na.fail, 
                           doFit = T, 
                           REML = T, 
                           weights = WEIGHTS, 
                           dispformula = ~1)


                           
,

, # colonne du jeu de donne avec les noms scientifiques concordant a l’arbre phylo pour faire la jonction observation / similarite (rapprochement) phylogenetique # DOIT ETRE EN FACTEUR
doFit = T, # F le modele ne tourne pas (sort une variable utilisable par TMB ?)
dispformula = ~1, # dispersion suivant le modele null
REML = T,
weights = WEIGHTS # WARNING le modele semble assez sensible aux poids, perso ca marche avec des poids normalize (reduction des poids extreme modulo un poids moyen
)
                           
                           
  
?phylo_glmmTMB                         
                          








##### poubelle ##### 
#le calibrage du modele ? 

#test pour faire la patrice de correlation 

phylo_cov <- corBrownian(phy = tr_sub2)

print(phylo_cov)
phylo_cov[1,1]
plot(phylo_cov)
ape::plot.phylo(tr_sub2)
dendro <- as.dendrogram(hclust(as.dist(1 - phylo_cov)))
pca <- phyl.pca(phylo_cov)
#mettre tout dans une matrice de correlation ? 
























