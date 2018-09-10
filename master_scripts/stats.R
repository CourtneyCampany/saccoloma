##run manova on all traits by year

source("functions.R")

alldata <- read.csv("calculated_data/alldata.csv")
alldata$year <- as.factor(alldata$year)

alldata_no26 <- alldata[!alldata$Indiv == 26,]
alldata_no26$year <- as.factor(alldata_no26$year)

# MANOVA - run multivariate analysis on all traits by year
traitsyear_mod <- manova(cbind(amass, pnue, iwue, Trmmol, nmass, n15, lma ,chl)
                         ~ year, data = alldata)
summary(traitsyear_mod)
summary.aov(traitsyear_mod)


#iwue, nmass significant 