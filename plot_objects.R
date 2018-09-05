library(scales)
#plot objects
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A[n])~~(mu*mol~m^-2~s^-1))
amasslab <- expression(italic(A)[mass]~~(n*mol~g^-1~s^-1))
trmmollab <- expression(italic(E)[L]~~(mmol~m^-2~s^-1))
cilab <- expression(italic(C)[i]~~(ppm))
c13lab <-expression(paste(delta^{13}, "C (\u2030)"))
itelab <- expression(ITE~~(mmol~CO[2]~mol~H[2]*O^-1))
Elab <- expression(italic(E)[L]~~(mmol~m^-2~s^-1))
chllab <- expression(Chlorophyll~content~~(mg~m^-2))

lmalab <- expression(LMA~~(g~m^-2))

nmasslab<- expression(Leaf~Nitrogen[mass]~~(mg~g^-1))
narealab <- expression(Leaf~Nitrogen[area]~~(mg~m^-2))
nuelab <- expression(atop(PNUE,
                          (mu*mols~CO[2]~g~N^-1~s^-1)))
#treatment colors
frondcols <- c("chartreuse", "darkgreen", "darkgoldenrod3", 'darkgoldenrod4')
frondcols2 <- c( "darkgoldenrod3","darkgreen","chartreuse" )
yearsold <- c("1 year old", "2 years old", "3 years old")


# standard error function--------------------------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))
