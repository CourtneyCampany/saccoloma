#format isotopes

chem <- read.csv("raw_data/leaf_chem.csv")
treat <- read.csv("raw_data/treatments.csv")


age_format <- function(x) {
    x$color <- gsub()
}

#remove id $
chem$color <- gsub("\\d+-", "", chem$id)
#remove leaf age color
chem$plant_id <- gsub("-[A-Z]", "", chem$id)
#add treatment
chem <- merge(chem, treat)


write.csv(chem, "calculated_data/leaf_chemistry.csv", row.names = FALSE)



##basic plots

boxplot(percN ~ year, data=chem)
boxplot(perC ~ year, data=chem)
boxplot(c13~year, data=chem)
boxplot(n15~year, data=chem)
