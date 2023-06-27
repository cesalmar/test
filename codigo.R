
## Paquetes utilizados
library(ade4)
library(devtools)
#install.packages("JLutils")
library(JLutils)
library(readr)
library(vegan)
library(xtable)
library(RColorBrewer)
library(MASS)
library(FactoMineR)
library(explor)
library(shiny)
library(factoextra)
library(bindrcpp)
library(class)
library(scales)
library(explor)
library(readxl)
library(FactoClass)
library(NbClust)
library(STAT)
library(reshape)

setwd("C:/Users/first/Documents/Consultorias trabajos tesis/Aguas Buga/2022")
dir()
## Base de datos
datos <- read.csv("Ag_buga_2022_1.csv", header=T, sep=";",dec=",")
attach(datos)
View(datos)
names(datos)
dim(datos)


##Análsis de correspondencia múltiple usando MCA y dudi.acm
par(mfrow = c(2, 2))
res.mca <- MCA(datos, graph = TRUE)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
dim(res.mca$var$contrib)
View(res.mca$var$contrib)
fviz_contrib(res.mca, choice = "var", axes = c(1,2), top = 20)
fviz_contrib(res.mca,choice = "var",axes=c(1,2),repel = T)
  +labs(title = "Contribuciones de Categorías en el primer plano factorial")

fviz_mca_var(res.mca, col.var = "contrib", repel = TRUE)
#restringiendo a las var que mas aportan


fviz_mca_biplot(res.mca, col.var = "contrib", repel = TRUE, label ="var")


res.mca <- MCA(datos, quali.sup = c(1,2,4,5,6,7,8,9,10,11,13,15,16,17,18,19,29,
                                    37,38,39,40,41), ncp = 5, graph = TRUE)


# #"Comuna","Var_1.4","Var_1.5","Var_2.3","Var_2.5","Var_5.1"
# ,"Var_5.2","Var_5.3","Var_5.4","Var_5.5","Var_5.6","Var_5.7"
# ,"Var_5.8","Var_6.1","Var_6.3","Var_6.6","Var_7.1","Var_7.2"
# ,"Var_7.3","Var_8.1"

names(datos)
fviz_mca_var(res.mca, col.var = "contrib", repel = TRUE, invisible="quali.sup")
fviz_mca_biplot(res.mca, col.var = "contrib", repel = TRUE,label = "none",invisible="quali.sup")

eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, title = "", ylim = c(0, 23))
fviz_mca_var(res.mca, title="",choice = "mca.cor",
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)

fviz_mca_var(res.mca, col.var = "contrib", invisible = "quali.sup",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)


#individuos

fviz_mca_ind(res.mca, col.ind = "cos2", 
             label = "none",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal())


# Biplot of individuals and variable categories

fviz_mca_biplot(res.mca, repel = TRUE, invisible = "quali.sup",
                label = "none",
                ggtheme = theme_minimal()
                )

fviz_mca_biplot(res.mca, label ="var", 
                invisible = "quali.sup",pointsize = 0.5,repel = TRUE)
#cluster

res.hcpc <- HCPC(res.mca,nb.clust=4, graph = FALSE)
?HCPC
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
)



fviz_cluster(res.hcpc, ellipse = TRUE, ellipse.type = "euclid",
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map", 
             ellipse.level=1,
             pointsize = 0.5,
             label="var"
             )

,
             label = "var")


fviz_cluster(res.hcpc, geom = "point", ellipse.type = "norm" ,outlier.color=C() )
?fviz_cluster


