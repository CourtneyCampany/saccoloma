source("plot_objects.R")

leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
lma <- read.csv("raw_data/sacc_lma.csv")
photo <- read.csv("calculated_data/leaf_gasexchange.csv")
chl <- read.csv("raw_data/sacc_chl.csv")

##data cleaning---------

photo$Color <- toupper(photo$Color)
photo$id <- as.factor(paste(photo$Indiv, photo$Color, sep="-"))
names(photo)[3] <- "color"

lma$lma <- with(lma, leaf_mass_g/leaf_area_cm2)
lma$Color <- toupper(lma$Color)
lma$id <- as.factor(paste(lma$Indiv, lma$Color, sep="-"))

chl$Color <- toupper(chl$Color)
chl$id <- as.factor(paste(chl$Indiv, chl$Color, sep="-"))

chl2 <- chl[chl$Individual <= 26,] #similar to gas exchange individuals

##merge datasets
traits <- merge(photo, leafchem)
traits2 <- merge(traits, lma[,c(6:7)])
traits3 <- merge(traits2, chl2[,c(3:4,7)])

#calcualte frond age:

frond_func <- function(x){
  x$frond_age <- ifelse(x$leafage == 2017, 1, "noage")
  x$frond_age <- ifelse(x$leafage == 2016, 2, x$frond_age)
  x$frond_age <- ifelse(x$leafage == 2015, 3, x$frond_age)
  return(x)
}

traits4 <- frond_func(traits3)

traits4$sla <- with(traits4, 1/lma)
traits4$amass <- with(traits4, (Photo*1000) * sla) ####nmols CO2 g s
traits4$nmass <- with(traits4, percN*10) #mg g-1 (g g-1 = .01 (1%) and 1000)

# write.csv(traits4, "calculated_data/alldata.csv", row.names=FALSE)

### plotting-----


#photo vs N
plot(Photo ~ percN, data=traits4, pch=21, bg=frondcols[as.factor(frond_age)], 
     cex=1.5, ylim=c(0, 6), xlim=c(0, 5))
legend("topleft", legend=yearsold, pt.bg=frondcols, pch=21, bty='n')

plot(amass ~ nmass, data=traits4, pch=21, bg=as.factor(frond_age))

plot(Photo ~ Cond, data=traits4, pch=21, bg=as.factor(frond_age), ylim=c(0, 6), xlim=c(0, 1))

plot(iwue ~c13, data=traits4, pch=21, bg=as.factor(frond_age), ylim=c(0, 10), xlim=c(-36, -32))

par(mar=c(5,5,1,1))
boxplot(Photo ~ leafage, data=traits4, ylab=photolab, ylim=c(0,6))

boxplot(Cond ~ leafage, data=traits4[traits4$Cond > 0 & traits4$Cond < .5,], ylab=condlab,
        ylim=c(0, .5))

boxplot(iwue~leafage, data=traits4[traits4$iwue > 0,], ylab="iWUE")

boxplot(Ci~leafage, data=traits4[traits4$Ci > 0,])

boxplot(lma ~ leafage, data=traits4, ylab=lmalab, ylim=c(0, 0.015))

boxplot(percN ~ leafage, data=traits4, ylim=c(0,4.5), ylab="Nitrogen content (%)")

boxplot(chl ~ leafage, data=traits4, ylim=c(0, 700))

