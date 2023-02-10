nounou <- matrix(1:4, nrow = 1, ncol = 4)
nounou




# python 3.x
import pandas as pd
# List of Tuples
fruit_list = [ ('Orange', 34, 'Yes' )]
#Create a DataFrame object
df = pd.DataFrame(fruit_list, columns = ['Name' , 'Price', 'Stock'])
#Add new ROW
df=df.append({'Name' : 'Apple' , 'Price' : 23, 'Stock' : 'No'} , ignore_index=True)
df=df.append({'Name' : 'Mango' , 'Price' : 13, 'Stock' : 'Yes'} , ignore_index=True)
print(df)

ligne.b <- c("ligne")
df <-data.frame(ligne.b)
df
df[nrow(df) + 1,] = c("valeur1")
df
df[nrow(df) + 1,] = c("valeur2")
df
grigri <- c("ouioui")
ligne.c <- c("truc")
dff <-data.frame(ligne.c)
dff
dff[nrow(dff) + 1,] = grigri
dff
dff[nrow(dff) + 1,] = c("valeur4")
dff 

xxx<- data.frame(annee = df, de2 = dff)  
xx <- merge(df,dff, all.x=TRUE)
xx
xxx
#pour supprimer les totaux 
data <-subset(data,age>21 ))# on remplace age >21 par lieu == "TOTAL"



# writing row in the csv file
write.table(row, file = csv_fname, sep = ",",
            append = TRUE, quote = FALSE,
            col.names = FALSE, row.names = FALSE)






# cr?ation du fichier A contenant le texte "file A\n"
# "\n" permet de creer une nouvelle ligne vide a la fin du fichier
cat("file A\n", file = "A")
# cr?ation du fichier B contenant le texte "file B\n"
cat("file B\n", file = "B")
# concat?nation : le fichier A contiendra le texte :
# file A
# file B
file.append("A", "B") 

cat("date", file = "A")
cat("lieu", file = "B")
didi <- didi[file.append("A", "B") ]


de_1 <- c(2, 3, 4, 1, 2, 3, 5, 6, 5, 4)
de_2 <- c(1, 4, 2, 3, 5, 4, 6, 2, 5, 3)
lanceur <- rep(c("Luc", "Kim"), each = 5)
data_ex <- data.frame(de1 = de_1, de2 = de_2, lanceur = lanceur)            
data_ex



rbind(data frame A, data frame B) # ajoute data frame B à la suite de data frame A (en ligne).


df <- data.frame("Employee" : c('Jonny', 'Grey', 'Mouni'),
                 + "Salary" : c(23000, 41000, 32344))
print (df)

write.csv(df, "C:/Users/SPECTRE/Desktop/PROFESSIONNEL/STAGE/SNPN/DATA/testcreation.csv", row.names = FALSE)
write.table(x = data_ex, file = "data_ex_export.txt", fileEncoding = "UTF-8")


ar <- numpy.array([1.1, 2, 3.3, 4], [2.7, 10, 5.4, 7], [5.3, 9, 1.5, 15])
df <- pandas.DataFrame(ar, index = ['a1', 'a2', 'a3'], columns = ['A', 'B', 'C', 'D']
                       
)))
