alldata <- read.csv("calculated_data/alldata.csv")
alldata_no26 <- alldata[!alldata$Indiv == 26,]
source("plot_objects.R")
source("functions.R")


##look for outliers
boxplot(chl ~ leafage, data=alldata)


#Here we plot photosynthesis vs chlorophyll
#confidence interval for each year or overall depending on stats (if significant)

#simple model
chlphoto_mod <- lm(Photo ~ chl * leafage, data=alldata_no26)
summary(chlphoto_mod)
anova(chlphoto_mod)

#plotting
par(mar=c(5,5,1,1))
plot(Photo ~ chl, data=alldata_no26, pch=21, bg=frondcols[as.factor(frond_age)], 
     cex=1.5,  ylab=photolab, xlab=chllab, ylim=c(0,5), xlim=c(300,700))
legend("topleft", legend=yearsold, pt.bg=frondcols, pch=21, bty='n')
