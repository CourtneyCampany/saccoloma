alldata <- read.csv("calculated_data/alldata.csv")
alldata_no26 <- alldata[!alldata$Indiv == 26,]
source("plot_objects.R")
source("functions.R")


#Here we plot photosynthesis vs chlorophyll
#confidence interval for each year or overall depending on stats (if significant)

#simple model
amassnmass_mod <- lm(amass ~ nmass, data=alldata_no26)
summary(amassnmass_mod)
anova(amassnmass_mod)

#plotting
par(mar=c(5,5,1,1))
plot(amass ~ nmass, data=alldata_no26, pch=21, bg=frondcols[as.factor(frond_age)], 
     cex=1.5, ylab=amasslab, xlab=nmasslab, ylim=c(0, 160), xlim=c(0, 50))
legend("topright", legend=yearsold, pt.bg=frondcols, pch=21, bty='n')
