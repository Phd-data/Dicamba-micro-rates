#Read csv

Data1=read.csv("dicamba2.csv")
str(Data1)



library(drc)

#Glyphosate at R2

Gly1=drm(IR~RateM, DAT, subset=Soybean=="Glyphosate",  fct=l4 (fixed =c(NA,NA,60,NA)), data= Data1)
modelFit(Gly1)
summary(Gly1)


plot(Gly1, col=c(1,2,3), legend=F, lty=c(1,2,3), pch=c(1,2,3), lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Crop Injury (%)")), 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="Glyphosate-Resistant Soybeans")

legend ("topleft", legend= c ("8 DAT", "15 DAT", "22 DAT"),
        col=c(1,2,3), lty=c(1,2,3),lwd=2, pch=c(1,2,3), bty="n")

ED(Gly1, c(5,10,20), type="absolute")

# 16.66 is 10% crop injury
EDcomp(Gly1, c(16.66, 16.66))
# 33.33 is 20% crop injury
EDcomp(Gly1, c(33.33,33.33))

#Glufosinate at R2

Glu1=drm(IR~RateM, DAT, subset=Soybean=="Glufosinate",  fct=l3 (fixed =c(NA,60,NA)), data= Data1)

summary(Glu1)

plot(Glu1, col=c(1,2,3,4), legend=F, lty=c(1,2,3,4), pch=c(1,2,3,4), lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Control (%)")), 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="Glufosinate-Resistant Soybeans")

legend ("topleft", legend= c ("8 DAT", "15 DAT", "22 DAT"),
        col=c(1,2,3), lty=c(1,2,3),lwd=2, pch=c(1,2,3), bty="n")

ED(Glu1, c(8.33,16.66,33.33))

# 16.66 is 10% crop injury
EDcomp(Glu1, c(16.66, 16.66))
# 33.33 is 20% crop injury
EDcomp(Glu1, c(33.33,33.33))

#Conventional

Con1=drm(IR~RateM, DAT, subset=Soybean=="Conventional",  fct=l3 (fixed =c(NA,60,NA)), data= Data1)

summary(Con1)


plot(Con1, col=c(1,2,3), legend=F, lty=c(1,2,3), pch=c(1,2,3), lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Control (%)")), 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="Conventional Soybeans")

legend ("topleft", legend= c ("8 DAT", "15 DAT", "22 DAT"),
        col=c(1,2,3), lty=c(1,2,3),lwd=2, pch=c(1,2,3), bty="n")

ED(Con1, c(8.33,16.66,33.33))

# 16.66 is 10% crop injury
EDcomp(Con1, c(16.66, 16.66))
# 33.33 is 20% crop injury
EDcomp(Con1, c(33.33,33.33))


#Organic

Org1=drm(IR~RateM, DAT, subset=Soybean=="Organic",  fct=l3 (fixed =c(NA,70,NA)), data= Data1)

summary(Org1)


plot(Org1, col=c(1,2,3), legend=F, lty=c(1,2,3), pch=c(1,2,3), lwd=2,
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Control (%)")), 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="Organic Soybeans")

legend ("topleft", legend= c ("8 DAT", "15 DAT", "22 DAT"),
        col=c(1,2,3), lty=c(1,2,3),lwd=2, pch=c(1,2,3), bty="n")


ED(Org1, c(7.15,14.3,28.6))

# 14.3 is 10% crop injury
EDcomp(Org1, c(14.3,14.3))

# 28.6 is 20% crop injury
EDcomp(Org1, c(28.6,28.6))


#Grape

Gra1=drm(IR~RateM, DAT, subset=Soybean=="Grape",  fct=l3 (fixed =c(NA,40,NA)), data= Data1)

summary(Gra1)



plot(Gra1, col=c(1,2,3), legend=F, lty=c(1,2,3), pch=c(1,2,3),
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Control (%)")), lwd=2, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="Grape")

legend ("topleft", legend= c ("8 DAT", "15 DAT", "22 DAT"),
        col=c(1,2,3), lty=c(1,2,3),lwd=2, pch=c(1,2,3), bty="n")


ED(Gra1, c(12.5,25,50))
# 25 is 10% crop injury
EDcomp(Gra1, c(25,25))
# 50 is 20% crop injury
EDcomp(Gra1, c(50,50))

#Tomato

Tom1=drm(IR~RateM, DAT, subset=Soybean=="Tomato",  fct=l3 (fixed =c(NA,NA,NA)), data= Data1)

summary(Tom1)


plot(Tom1, col=c(1,2,3), legend=F, lty=c(1,2,3), pch=c(1,2,3),
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Control (%)")), lwd=2, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="Tomato")

legend ("topleft", legend= c ("8 DAT", "15 DAT", "22 DAT"),
        col=c(1,2,3,4), lty=c(1,2,3,4),lwd=2, pch=c(1,2,3,4), bty="n")

ED(Tom1, c(8.33,16.66,33.33))
#abline(h=20, v=1.1291)

# 16.66 is 10% crop injury
EDcomp(Tom1, c(16.66,16.66))
# 33.33 is 20% crop injury
EDcomp(Tom1, c(33.33,33.33)) 



# Now looking across Soybeans

#8 d after application

DATS1=drm(IR~RateM, Soybean, subset=DAT=="8",  fct=l3 (fixed =c(NA,55,NA)), data= Data1) 

summary(DATS1) 


plot(DATS1,  col=c(1,2,3,4,6,5), legend=F, lty=c(1,2,3,4,5,6), pch=c(1,2,3,4,5,6),
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")),
     ylab = expression(paste("Control (%)")), lwd=2, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="7 DAT")


legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                              "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n") 

ED(DATS1, c(8.33,16.66,33.33)) 
# 16.66 is 10% crop injury
EDcomp(DATS1, c(16.66,16.66))
# 33.33 is 20% crop injury
EDcomp(DATS1, c(33.33,33.33)) 


#15 d after application

DATTT1=drm(IR~RateM, Soybean, subset=DAT=="15",  fct=l3 (fixed =c(NA,60,NA)), data= Data1)

summary(DATTT1)

plot(DATTT1, col=c(1,2,3,4,6,5), legend=F, lty=c(1,2,3,4,5,6), pch=c(1,2,3,4,5,6),
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Control (%)")), lwd=2, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="15 DAT")


legend ("topleft", legend= c ("Glyphosate", "Glufosinate", "Conventional", "Organic",
                              "Grape", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=2, pch=c(1,2,3,4,5,6), bty="n")

ED(DATTT1, c(5,10,20))
# 16.66 is 10% crop injury
EDcomp(DATTT1, c(16.66,16.66))
# 33.33 is 10% crop injury
EDcomp(DATTT1, c(33.33,33.33))


#22 d after application

DATTW1=drm(IR~RateM, Soybean, subset=DAT=="22",  fct=l4 (fixed =c(NA,NA,100,NA)), data= Data1)
modelFit(DATTW1)
summary(DATTW1)

plot(DATTW1, col=c(1,2,3,4,6,5), legend=F, lty=c(1,2,3,4,5,6), pch=c(1,2,3,4,5,6),
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Control (%)")), lwd=2, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="22 DAT")

legend ("topleft", legend= c ("Conventional", "Glufosinate", "Glyphosate", "Grape",
                              "Organic", "Tomato"),
        col=c(1,2,3,4,6,5), lty=c(1,2,3,4,5,6),lwd=1, pch=c(1,2,3,4,5,6), bty="n")

ED(DATTW1, c(7.15,14.3,28.6))
# 14.3 is 10% crop injury
EDcomp(DATTW1, c(14.3,14.3))
# 28.6 is 20% crop injury
EDcomp(DATTW1, c(28.6,28.6))
