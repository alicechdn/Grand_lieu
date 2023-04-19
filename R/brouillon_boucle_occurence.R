#TEST DE LA BOUCLE 
##### PARTIE SOCLE A NE PAS TOUCHER ######
library(tictoc)
library(readxl)
PE <- read_excel("C:/git/Grand_lieu/DATA/PE.xlsx",
                 col_names = TRUE)
###### annee en facteur
PE$ANNEE_txt <- as.factor(PE$ANNEE)
library(data.table)
setnames(PE,"ESPECE","CODE")
PE$CODE <- as.factor(PE$CODE) ; PE$SITE <- as.factor(PE$SITE)
summary(PE)
####### annee centre et reduite en numerique 
PE$annee_sc = scale(PE$ANNEE)

library(glmmTMB)#package pour faire glmm

occ_esp <- tapply(PE$ABONDANCE, PE$CODE, function(x) sum(x >0))
occ_esp #correspond au nombre d'occurence de chaque espece

r_spf <- function(x, affiche = TRUE){
  assign(paste0("rare_species_",x), names(occ_esp[occ_esp < x]), envir = .GlobalEnv)
  if(affiche){
    str(get(paste0("rare_species_",x)))
  }
}

list_x <- c(500, 600)

for (x in list_x) {
  r_spf(x)
}

modele_f <- function(x, affiche = TRUE){
  assign(paste0("PE", x), PE[!(PE$CODE %in% get(paste0("rare_species_",x))),], envir = .GlobalEnv)
  assign(paste0("md_",x), glmmTMB(ABONDANCE ~ annee_sc + (1|SITE) + (1|CODE), data = get(paste0("PE",x)), family = nbinom2), envir = .GlobalEnv)
  if(affiche){
    summary(get(paste0("md_",x)))
  }
}

modele_f_simple <- function(d,list_rare_sp, affiche = TRUE){
  d_function <- d[!(d$CODE %in% list_rare_sp),]
  md <- glmmTMB(ABONDANCE ~ annee_sc + (1|SITE) + (1|CODE), data = d_function, family = nbinom2)
  smd <- summary(md)  
if(affiche){
    print(smd)
}
  return(md)
}



###### PARTIE DE TEST #######


result_time <- data.frame()
mean_year <- mean(PE$ANNEE)
sd_year <- sd(PE$ANNEE)
tab_md_x <- NULL

for (x in list_x) {
  cat(x,"\n")
  start <- tic()
  #modele_f(x)
  md_x <- modele_f_simple(d=PE,get(paste0("rare_species_",x)))
  smd_x <- summary(md_x)
  
  tab_trend_raw <- as.data.frame(coef(smd_x)$cond)
  trend_raw <- tab_trend_raw[2,1]
  
  trend <- exp(trend_raw)^(1/sd_year)
  
  mdIC <- as.data.frame(confint(md_x)[,1:2])
  colnames(mdIC) <- c("ICinf","ICsup")
  IC_inf_raw <- mdIC$ICinf[2]
  IC_sup_raw <- mdIC$ICsup[2]
  IC_inf <- exp(IC_inf_raw)^(1/sd_year)
  IC_sup <- exp(IC_sup_raw)^(1/sd_year)
  
  
  
  temps <- toc() #elapsed pour avoir un nbre
  temps_sec <- as.numeric(gsub(" sec elapsed", "",temps$callback_msg))
  #Stocker les résultats de temps :
  #assign(paste0("result_",x), cbind(x, temps$callback_msg), envir = .GlobalEnv)
  #result_time <- rbind(result_time, temps$callback_msgget(paste0("result_",x)))




colnames(result_time) <- c("occ","time")
result_time$time2 <- as.numeric(gsub(" sec elapsed", "", result_time$time))
summary(result_time)


conf_tot <- data.frame()
for (x in list_x) {
  tic()
  assign(paste0("conf_",x), rbind(confint(get(paste0("md_",x)))[2,]),envir = .GlobalEnv)
  assign(paste0("conf_",x), cbind(get(paste0("conf_",x)), x),envir = .GlobalEnv)
  conf_tot <- rbind(conf_tot, get(paste0("conf_",x)))
  toc()
}#boucle pour obtenir une variable occurence
colnames(conf_tot) <- c("IC_inf","IC_sup","estimate", "occurence")


#GRAPHIQUE 
plot(conf_tot$estimate~conf_tot$occurence, xlab = "occurence", ylab = "estimate", main = "valeur de l'estimate en fonction du nbre d'occurence")

library(ggplot2)

ggplot(conf_tot, aes(x = occurence)) +
  geom_point(aes(y = IC_inf, color = "IC")) +
  geom_point(aes(y = IC_sup, color = "IC")) +
  geom_point(aes(y = estimate, color = "estimate")) +
  geom_line(aes(y = estimate, color = "estimate")) +
  geom_line(aes(y = IC_inf, color = "IC")) +
  geom_line(aes(y = IC_sup, color = "IC")) +
  labs(x = "occurence",y = "estimate", title = "valeur de l'estimate du modele en fonction du minimum d'occurence")





#ENREGISTRER DANS UN DOCUMENT EXTERNE LES SUMMARY DES MODELES 
fileConn<-file("results.txt")
# Rediriger la sortie vers le fichier
sink(fileConn)
for (x in list_x) {
tt <-summary(get(paste0("md_",x)))
print(tt)
}
# Fermer le fichier et arrêter la redirection de la sortie
sink()
close(fileConn)
yy <- read.table("results.txt", header =T)

file.exists("results.txt")
sink.number()


