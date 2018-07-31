leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
lma <- read.csv("raw_data/sacc_lma.csv")
photo <- read.csv("calculated_data/leaf_gasexchange.csv")
chl <- read.csv("raw_data/sacc_chl.csv")

##data cleaning

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



plot(Photo ~ percN, data=traits)
plot(Photo ~ Cond, data=traits)
plot(iwue ~c13, data=traits )


