
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only=TRUE)
}
packages <-  c("ggpubr", "tidyverse", "cluster", "factoextra","readxl",  "NbClust", "dplyr", "clustertend", "Hmisc")
ipak(packages)
Protein_glucose_drug <- read_excel("C:/Users/jdtel/OneDrive - Escuela Superior Politécnica del Litoral/Daniel/ESPOL/Scientific Computing/Data Science/Datos FTIR.xlsx", 
                                   sheet ='(BSA+G)(cte)+M FTIR',     skip = 4 )

#BSA (mg/ml)=5
Protein_drug <- read_excel("C:/Users/jdtel/OneDrive - Escuela Superior Politécnica del Litoral/Daniel/ESPOL/Scientific Computing/Data Science/Datos FTIR.xlsx", 
                           sheet="BSA+M FTIR",      skip = 3 )


Protein_glucose <- read_excel("C:/Users/jdtel/OneDrive - Escuela Superior Politécnica del Litoral/Daniel/ESPOL/Scientific Computing/Data Science/Datos FTIR.xlsx", 
                              sheet="BSA+G FTIR",     skip = 3 )

c1 <- hopkins(data=Protein_glucose[, 2:17], n=nrow(Protein_glucose)-1)
c2 <- hopkins(data=Protein_drug[, 2:17], n=nrow(Protein_glucose)-1)
c3 <- hopkins(data=Protein_glucose_drug, n=nrow(Protein_glucose)-1)


hist.data.frame(Protein_drug[, 2:9])

stripchart(Protein_drug[, 2:9])
boxplot(Protein_drug$`MSA+M5`)


data <- Protein_glucose_drug[, 2:17]
data2[1]= Protein_glucose_drug[, 17]
g1 <- fviz_nbclust(data, kmeans, method="wss")
g2 <- fviz_nbclust(data, kmeans, method="silhouette")
g3 <- fviz_nbclust(data, kmeans, method="gap")
ggarrange(g1, g2, g3)


#clust <- NbClust(data, distance="euclidean", min.nc =2, max.nc=10, method="kmeans",  index="alllong")
#fviz_nbclust(clust)

data <- Protein_glucose_drug[1:1000, 2:4]


k=kmeans(data, center=2, nstart = 25)
fviz_cluster(k, data=data)


fviz_cluster(k, data=data, ellipse.type = "euclid", repel = TRUE, star.plot=TRUE)
fviz_cluster(k, data=data, ellipse.type="norm")




view(USArrests)
