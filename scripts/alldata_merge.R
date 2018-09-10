###all data merge

leafchem <- read.csv("calculated_data/leaf_chemistry.csv")
lma <- read.csv("raw_data/sacc_lma.csv")
photo <- read.csv("calculated_data/leaf_gasexchange.csv")
chl <- read.csv("raw_data/sacc_chl.csv")

##data cleaning---------

photo$Color <- toupper(photo$Color)
photo$id <- as.factor(paste(photo$Indiv, photo$Color, sep="-"))
names(photo)[3] <- "color"

lma$lma <- with(lma, (leaf_mass_g/leaf_area_cm2)*10000) #g m-2
lma$Color <- toupper(lma$Color)
lma$id <- as.factor(paste(lma$Indiv, lma$Color, sep="-"))

chl$Color <- toupper(chl$Color)
chl$id <- as.factor(paste(chl$Indiv, chl$Color, sep="-"))

# chl2 <- chl[chl$Individual <= 26,] #similar to gas exchange individuals

##merge datasets
traits <- merge(photo, leafchem)
traits2 <- merge(traits, lma[,c(6:7)])
traits3 <- merge(traits2, chl[,c(3:4,7)])

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
traits4$pnue <- with(traits4, amass/nmass)

write.csv(traits4, "calculated_data/alldata.csv", row.names=FALSE)
