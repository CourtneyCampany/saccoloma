library(scales)
#plot objects
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
amasslab <- expression(italic(A)[mass]~~(n*mol~g^-1~s^-1))
trmmollab <- expression(italic(E)[L]~~(mmol~m^-2~s^-1))
cilab <- expression(italic(C)[i]~~(ppm))
# specieslab <- expression(italic(Bolbitis~portoricensis), italic())



lmalab <- expression(LMA~~(g~m^-2))

nmasslab<- expression(Leaf~Nitrogen[mass]~~(mg~g^-1))
narealab <- expression(Leaf~Nitrogen[area]~~(mg~m^-2))

#treatment colors
frondcols <- c("chartreuse", "darkgreen", "darkgoldenrod3")
yearsold <- c("1 year", "2 year", "3 year")


# standard error function--------------------------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))
