alldata <- read.csv("calculated_data/all_data_clean.csv")
# alldata_no26 <- alldata[!alldata$Indiv == 26,]
source("plot_objects.R")
source("functions.R")

View(alldata)
as.factor(alldata$year)


#Here we plot photosynthesis vs chlorophyll
#confidence interval for each year or overall depending on stats (if significant)

#simple model
amassnmass_mod <- lm(amass ~ nmass, data=alldata)
summary(amassnmass_mod)
anova(amassnmass_mod)

# MANOVA
man_mod <- manova(cbind(amass, pnue, iwue, trans, nmass, n15_10, lma, cfr, chl) ~ year, data = alldata)
summary(man_mod)
summary.aov(man_mod)

# PCA
install.packages("MVA")
library(MVA)
install.packages("psych")
library(psych)
install.packages("Hmisc")
library(Hmisc)
install.packages("vegan")
library(vegan)
install.packages("StatMatch")
library(StatMatch)
install.packages("MASS")
library(MASS)
install.packages("ggfortify")
library(ggfortify)

logdata<-log(alldata[(1:28),(5:12)]) # log trnasform the numeric variables only
View(logdata) # check it out
zdata<-scale(logdata)# Z transform (scale) the data
View(zdata)
df_pca<- prcomp(na.omit(zdata))
summary(df_pca)
df_pca

autoplot(df_pca, data = alldata, colour ='year', size = 5, scaling=1,
                      loadings = TRUE,
                      loadings.colour = 'black',
                      loadings.label = TRUE,
                      loadings.label.size = 4)


install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
get_pca_var(df_pca)
eig.val <- get_eigenvalue(df_pca)
head(eig.val)

ind.coord <- df_pca$x
head(ind.coord[, 1:6])
write.csv(ind.coord, "sacc_pca_individuals.csv")

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

write.csv(loadings, "sacc_pca_loadings.csv")

# hypervolumes



#plotting
par(mar=c(5,5,1,1))
plot(amass ~ nmass, data=alldata_no26, pch=21, bg=frondcols[as.factor(frond_age)], 
     cex=1.5, ylab=amasslab, xlab=nmasslab, ylim=c(0, 160), xlim=c(0, 50))
legend("topright", legend=yearsold, pt.bg=frondcols, pch=21, bty='n')
