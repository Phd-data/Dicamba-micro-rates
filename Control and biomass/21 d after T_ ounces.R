#Read csv
Dicam=read.csv("21DAT.csv")
str(Dicam)

library(drc)
#20 d after application
x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Gy=drm(IR~RateUS, subset=Soybean=="Glyphosate",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Gy)
summary(Gy)

plot(Gy, col="orange", legend=F, lty=1, pch=1, type = "none",  cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

ED(Gy,c(10, 20, 30, 50), type="absolute")



Gu=drm(IR~RateUS, subset=Soybean=="Glufosinate",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Gu)
summary(Gu)

plot(Gu,  add=T, col="gray48", legend=F, lty=1, pch=2, cex=2, type = "none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Gu,c(10,20,30,50), type="absolute")


Cnt=drm(IR~RateUS, subset=Soybean=="Conventional",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Cnt)
summary(Cnt)

plot(Cnt,  add=T, col="pink", legend=F, lty=1, pch=3, cex=2, type = "none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Cnt,c(10,20, 30, 50), type="absolute")

Or=drm(IR~RateUS, subset=Soybean=="Organic",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Or)
summary(Or)

plot(Or,  add=T, col="blue", legend=F, lty=1, pch=4, cex=2, type = "none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Or,c(10,20, 30, 50), type="absolute")


Gr=drm(IR~RateUS, subset=Soybean=="Grape",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Gr)
summary(Gr)

plot(Gr,  add=T, col="green", legend=F, lty=1, pch=5, cex=2, type = "none", 
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Gr,c(10,20, 30, 50), type="absolute")

Tm=drm(IR~RateUS, subset=Soybean=="Tomato",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Tm)
summary(Tm)

plot(Tm,  add=T, col="red", legend=F, lty=1, pch=6, cex=2, type = "none", 
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

legend ("bottomright", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5,
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=5,bty="n")

ED(Tm,c(10,20, 30, 50), type="absolute")

#Read csv
Dicam=read.csv("22DATR2.csv")
str(Dicam)

library(drc)
#22 d after application
x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Gy=drm(IR~RateUS, subset=Soybean=="Glyphosate",  fct=l3 (fixed =c(NA,70,NA)), data= Dicam)
modelFit(Gy)
summary(Gy)

plot(Gy, col="orange", legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")), type="none",
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

ED(Gy,c(10,20,30,50), type="absolute")



Gu=drm(IR~RateUS, subset=Soybean=="Glufosinate",  fct=l3 (fixed =c(NA,70,NA)), data= Dicam)
modelFit(Gu)
summary(Gu)

plot(Gu,  add=T, col="gray48", legend=F, lty=1, pch=2, cex=2, type="none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Gu,c(10,20,30,50), type="absolute")


Cnt=drm(IR~RateUS, subset=Soybean=="Conventional",  fct=l3 (fixed =c(NA,70,NA)), data= Dicam)
modelFit(Cnt)
summary(Cnt)

plot(Cnt, add=T,  col="pink", legend=F, lty=1, pch=3, cex=2, type="none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Cnt,c(10,20,30,50), type="absolute")


Or=drm(IR~RateUS, subset=Soybean=="Organic",  fct=l3 (fixed =c(NA,100,NA)), data= Dicam)
modelFit(Or)
summary(Or)

plot(Or,  add=T, col="blue", legend=F, lty=1, pch=4, cex=2, type="none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Or,c(10,20,30,50), type="absolute")


Gr=drm(IR~RateUS, subset=Soybean=="Grape",  fct=l3 (fixed =c(NA,30,NA)), data= Dicam)
modelFit(Gr)
summary(Gr)

plot(Gr,  add=T, col="green", legend=F, lty=1, pch=5, cex=2, type="none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

ED(Gr,c(10,20,30,50), type="absolute")


Tm=drm(IR~RateUS, subset=Soybean=="Tomato",  fct=l3 (fixed =c(NA,70,NA)), data= Dicam)
modelFit(Tm)
summary(Tm)

plot(Tm,  add=T, col="red", legend=F, lty=1, pch=6, cex=2, type = "none",
     xlab = expression(paste("Dicamba (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100), main="21 DAT after R2")

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5,
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=5,bty="n")

ED(Tm,c(10,20,30,50), type="absolute")


