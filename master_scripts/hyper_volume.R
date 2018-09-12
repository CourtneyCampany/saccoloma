source("plot_objects.R")
source("functions.R")

alldata <- read.csv("calculated_data/alldata.csv")
  alldata$year <- as.factor(alldata$year)

# alldata_no26 <- alldata[!alldata$Indiv == 26,]
#   alldata_no26$year <- as.factor(alldata_no26$year)
  
# packages for PCA and hyper volume
  library(MVA)
  library(psych)
  library(Hmisc)
  library(vegan)
  library(StatMatch)
  library(MASS)
  library(ggfortify)
  
#run PCA
logdata<-log(alldata[(1:28),(5:12)]) # log trasnform the numeric variables only

zdata<-scale(logdata)# Z transform (scale) the data

df_pca<- prcomp(na.omit(zdata))
  summary(df_pca)

autoplot(df_pca, data = alldata, colour ='year', size = 5, scaling=1,
           loadings = TRUE,
           loadings.colour = 'black',
           loadings.label = TRUE,
           loadings.label.size = 4)


library(FactoMineR)
library(factoextra)
get_pca_var(df_pca)
eig.val <- get_eigenvalue(df_pca)
head(eig.val)

ind.coord <- df_pca$x
head(ind.coord[, 1:6])
# write.csv(ind.coord, "calculated_data/sacc_pca_individuals.csv")

# Variable correlation/coordinates
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}
loadings <- df_pca$rotation
View(loadings)
sdev <- df_pca$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord[, 1:4])
var.cos2 <- var.coord^2
head(var.cos2[, 1:6])
# The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:4])

# write.csv(loadings, "calculated_data/sacc_pca_loadings.csv")

# hypervolume plotting??
