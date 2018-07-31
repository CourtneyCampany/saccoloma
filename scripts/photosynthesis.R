photo <- read.csv("raw_data/sacc_photo.csv")

addleafage_func <- function(x) {
  x$leafage <- ifelse(x$Color == "w", 2017, "noage")
  x$leafage <- ifelse(x$Color == "o", 2016, x$leafage)
  x$leafage <- ifelse(x$Color == "y", 2015, x$leafage)
  x$leafage <- ifelse(x$Color == "p", 2016, x$leafage)
  return(x)
}

photo <- addleafage_func(photo)

library(doBy)

#individual plant means (spot measurements)
photo_agg <- summaryBy(Photo + Cond + Ci + Trmmol ~ Indiv + leafage + Color, data=photo,
                       FUN=mean, keep.names = TRUE)

photo_agg$iwue <- with(photo_agg, Photo/Trmmol)

##26 is a bad plant
photo_clean <- photo_agg[photo_agg$Indiv != 26, ]

write.csv(photo_clean, "calculated_data/leaf_gasexchange.csv", row.names = FALSE)

boxplot(Photo ~ leafage, data=photo_clean)
boxplot(Cond ~ leafage, data=photo_clean[photo_clean$Cond > 0 & photo_clean$Cond < .5,])
boxplot(iwue~leafage, data=photo_clean[photo_clean$iwue > 0,])
boxplot(Ci~leafage, data=photo_agg[photo_agg$Ci > 0,])


