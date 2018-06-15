#read in and format saccoloma leaf age dataset from La Selva July 2017

chlro <- read.csv("saccoloma/sacc_chl.csv")
photo <- read.csv("saccoloma/sacc_photo.csv")


addleafage_func <- function(x) {
  x$leafage <- ifelse(x$Color == "w", 2017, "noage")
  x$leafage <- ifelse(x$Color == "o", 2016, x$leafage)
  x$leafage <- ifelse(x$Color == "y", 2015, x$leafage)
  x$leafage <- ifelse(x$Color == "p", 2016.5, x$leafage)
return(x)
  }

chlro <- addleafage_func(chlro)
photo <- addleafage_func(photo)


library(doBy)
photo_agg <- summaryBy(Photo + Cond + Ci + Trmmol ~ Indiv + leafage, data=photo,
                       FUN=mean, keep.names = TRUE)

  photo_agg$iwue <- with(photo_agg, Photo/Trmmol)
  #indiviudal 26 seems to have bad h20 data

boxplot(iwue ~ leafage, data=photo_agg, outline=FALSE, xaxt="n")
axis(1, at=1:3, years[1:3])
##wue is likely lower in age 3

boxplot(Photo ~ leafage, data=photo_agg, outline=FALSE)
#An seems to be higher in year 2, a lot of variation in year 1 however

boxplot(Trmmol ~ leafage, data=photo_agg, outline=FALSE)

boxplot(chl ~ leafage, data=chlro, outline=FALSE)
#chlorophyll content unchanged over leaf age categories