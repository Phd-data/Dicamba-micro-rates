#Read csv

Data=read.csv("dicamba1.csv")
Biomass=read.csv("Biomass.csv") 

library(drc)

#Glyphosate 

Gly=drm(IR~RateM, DAT, subset=Soybean=="Glyphosate",  fct=l3 (fixed =c(NA,100,NA)), data= Data)
modelFit(Gly)
summary(Gly)



plot(Gly, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="Glyphosate Resistant Soybeans", ylim=c(0,100))

legend ("bottomright", legend= c ("7 DAT", "13 DAT", "20 DAT", "27 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Gly, c(5,10,20), type="absolute")
abline(h=20, v=1.276655)
EDcomp(Gly, c(10,10))
EDcomp(Gly, c(20,20))



#Glufosinate

Glu=drm(IR~RateM, DAT, subset=Soybean=="Glufosinate",  fct=l3 (fixed =c(NA,100,NA)), data= Data)

summary(Glu)

plot(Glu, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
           xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
           ylab = expression(paste("Crop Injury (%)")),
           bp=0.0001, xlim=c(0,100000), main="Glufosinate Resistant Soybeans", ylim=c(0,100))

legend ("bottomright", legend= c ("7 DAT", "13 DAT", "20 DAT", "27 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Glu, c(5,10,20))

EDcomp(Glu, c(10,10))
EDcomp(Glu, c(20,20))


#Conventional

Con=drm(IR~RateM, DAT, subset=Soybean=="Conventional",  fct=l3 (fixed =c(NA,100,NA)), data= Data)

summary(Con)

plot(Con, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="Conventional Soybeans", ylim=c(0,100))

legend ("bottomright", legend= c ("7 DAT", "13 DAT", "20 DAT", "27 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Con, c(5,10,20))

EDcomp(Con, c(10,10))
EDcomp(Con, c(20,20))


#Organic

Org=drm(IR~RateM, DAT, subset=Soybean=="Organic",  fct=l3 (fixed =c(NA,100,NA)), data= Data)

summary(Org)

plot(Org, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="Organic Soybeans", ylim=c(0,100))


legend ("bottomright", legend= c ("7 DAT", "13 DAT", "20 DAT", "27 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Org, c(5,10,20))

EDcomp(Org, c(10,10))
EDcomp(Org, c(20,20))

#Grape

Gra=drm(IR~RateM, DAT, subset=Soybean=="Grape",  fct=l4 (fixed =c(NA,NA,100,NA)), data= Data)

summary(Gra)

plot(Gra, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="Grape", ylim=c(0,100))


legend ("bottomright", legend= c ("7 DAT", "13 DAT", "20 DAT", "27 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Gra, c(5,10,20))

EDcomp(Gra, c(10,10))
EDcomp(Gra, c(20,20))

#Tomato

Tom=drm(IR~RateM, DAT, subset=Soybean=="Tomato",  fct=l3 (fixed =c(NA,100,NA)), data= Data)

summary(Tom)

plot(Tom, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="Tomato", ylim=c(0,100))

legend ("bottomright", legend= c ("7 DAT", "13 DAT", "20 DAT", "27 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Tom, c(5,10,20))

EDcomp(Tom, c(10,10))
EDcomp(Tom, c(20,20))

# Now looking across Soybeans

#7 d after application

DATS=drm(IR~RateM, Soybean, subset=DAT=="7",  fct=l3 (fixed =c(NA,99,NA)), data= Data)

summary(DATS)

plot(DATS, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="7 DAT", ylim=c(0,100))

legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                                  "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n")

ED(DATS, c(5.05,10.1,20.2))

EDcomp(DATS, c(10.1,10.1))
EDcomp(DATS, c(20.2,20.2))


#13 d after application

DATTT=drm(IR~RateM, Soybean, subset=DAT=="13",  fct=l3 (fixed =c(NA,100,NA)), data= Data)

summary(DATTT)

plot(DATTT, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="13 DAT", ylim=c(0,100))

legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                              "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n")

ED(DATTT, c(5,10,20))

EDcomp(DATTT, c(10,10))
EDcomp(DATTT, c(20,20))

#20 d after application

DATTW=drm(IR~RateM, Soybean, subset=DAT=="20",  fct=l3 (fixed =c(NA,100,NA)), data= Data)
modelFit(DATTW)
summary(DATTW)

plot(DATTW, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="20 DAT", ylim=c(0,100))

legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                              "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n")

ED(DATTW, c(5,10,20))

EDcomp(DATTW, c(10,10))
EDcomp(DATTW, c(20,20))


#27 d after application

DATTS=drm(IR~RateM, Soybean, subset=DAT=="27",  fct=l4 (fixed =c(NA,NA,100,NA)), data= Data)
modelFit(DATTS)
summary(DATTS)

plot(DATTS, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="27 DAT", ylim=c(0,100))


legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                                  "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n")

ED(DATTS, c(5,10,20))

EDcomp(DATTS, c(10,10))
EDcomp(DATTS, c(20,20))



# Biomass Reduction
library(drc) 
Biomass=read.csv("Biomass.csv") 

BR=drm(BR~RateM, Soybean,  fct=l3 (fixed =c(NA,95,NA)), data= Biomass)

summary(BR)

plot(BR, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), cex=1.2,lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")),
     bp=0.0001, xlim=c(0,100000), main="Biomass", ylim=c(0,100))


legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                                  "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n")

ED(BR, c(5.25,10.5,21.1))

EDcomp(BR, c(10.5,10.5))
EDcomp(BR, c(20.1,20.1))





# Biomass


#Glyphosate
Glyp=drm(BR~RateM, subset=Soybean=="Glyphosate", data=Biomass, fct=l3 (fixed =c(NA,95,NA)))
modelFit(Glyp)
plot(Glyp, xlim=c(0,100000), bp=0.0001, ylim=c(0,100), col=1)
summary(Glyp)
mselect(Glyp,
        list(LL.5(), LL.4(), LN.4(), W1.4(), W2.4(), BC.4(), BC.5(), LL.3(), l4()))





#Organic
Organ=drm(BR~RateM, subset=Soybean=="Organic", data=Biomass, fct=BC.5())
modelFit(Organ)
plot(Organ, add=T, bp=0.0001, xlim=c(0,100000), col=4)
summary(Organ)

mselect(Organ,
        list(LL.5(), LL.4(), LN.4(), W1.4(), W2.4(), BC.4(), BC.5(), LL.3(), l4()))



#Glufosinate
Gluf=drm(DW~RateM, subset=Soybean=="Glufosinate", data=Biomass, fct=l3())
modelFit(Gluf)

summary(Gluf)
plot(Gluf, add=T, xlim=c(0,60), col=2)
mselect(Organ,
        list(LL.5(), LL.4(), LN.4(), W1.4(), W2.4(), BC.4(), BC.5(), LL.3(), l4()))


#Conventional
Conv=drm(DW~RateM, subset=Soybean=="Conventional", data=Biomass, fct=l3())
modelFit(Conv)
plot(Conv, add=T, xlim=c(0,60), col=3)
summary(Conv)


mselect(Conv,
        list(LL.5(), LL.4(), LN.4(), W1.4(), W2.4(), l3(), BC.5(), LL.3(), l4()))


plot (DW~RateM, data = Biomass, las=1,
      xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
      ylab = expression(paste("Dry Weight (g)")), type="none",
      ylim=c(5,52), xlim=c(0,60), cex.lab=1.3, font.axis=2, main="Dry Weight")

legend ("topright", legend= c ("Grape", "Tomato"),
col=c(6,5), lty=c(5,6),lwd=1, pch=c(5,6), bty="n")




#Tomato
Tomat=drm(DW~RateM, subset=Soybean=="Tomato", data=Biomass, fct=l3())
modelFit(Tomat)
plot(Tomat, add=T, xlim=c(0,60), col=5)
mselect(Tomat,
        list(LL.5(), LL.4(), LN.4(), W1.4(), W2.4(), l3(), BC.5(), LL.3(), l4()))
#Grape
Grape=drm(DW~RateM, subset=Soybean=="Grape", data=Biomass, fct=l3())
modelFit(Grape)
plot(Grape, add=T, xlim=c(0,60), col=6)
summary(Grape)

mselect(Grape,
        list(LL.5(), LL.4(), LN.4(), W1.4(), W2.4(), BC.4(), BC.5(), l3(), l4()))

