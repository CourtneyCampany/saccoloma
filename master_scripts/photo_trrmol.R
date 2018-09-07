alldata <- read.csv("calculated_data/alldata.csv")
alldata_no26 <- alldata[!alldata$Indiv == 26,]
source("plot_objects.R")
source("functions.R")

##look for outliers
boxplot(Trmmol ~ leafage, data=alldata)
boxplot(Photo ~ leafage, data=alldata)


#Here we plot photosynthesis vs chlorophyll
#confidence interval for each year or overall depending on stats (if significant)

#simple model
phototransp_mod <- lm(Photo ~ Trmmol, data=alldata_no26)
summary(phototransp_mod)
anova(phototransp_mod)

#plotting
par(mar=c(5,5,1,1))
plot(Photo ~ Trmmol, data=alldata_no26, pch=21, bg=frondcols[as.factor(frond_age)], 
     cex=1.5, ylab=photolab, xlab=trmmollab, ylim=c(0,6), xlim=c(0,6))
legend("topright", legend=yearsold, pt.bg=frondcols, pch=21, bty='n')