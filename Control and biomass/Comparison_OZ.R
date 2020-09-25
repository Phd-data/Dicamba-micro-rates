
library(drc)


Glyphosate=read.csv("Glyphosate.csv")
str(Glyphosate)

x11()
par(mfrow=c(2,3))
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Gly=drm(IR~RateUS, subset=Stage=="2nd",  fct=l3 (fixed =c(NA,100,NA)), data= Glyphosate)
modelFit(Gly)
summary(Gly)



plot(Gly, col=4, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


Gly2=drm(IR~RateUS, subset=Stage=="Rtwo",  fct=l3 (fixed =c(NA,70,NA)), data= Glyphosate)
modelFit(Gly2)
summary(Gly2)


plot(Gly2, add=T, col=2, legend=F, lty=1, pch=2, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")),  
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans"),
        cex=2, bty="n")
legend ("bottomright", legend= c ("Second Trifoliate", "Full Flower"),
        cex=2,
        col=c(4,2), lty=c(1,1), pch=c(1,2), lwd=4, bty="n")


#______________________________________________________________________________________________

Glufosinate=read.csv("Glufosinate.csv")
str(Glufosinate)

x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Glu=drm(IR~RateUS, subset=Stage=="Sec",  fct=l3 (fixed =c(NA,100,NA)), data= Glufosinate)
modelFit(Glu)
summary(Glu)



plot(Glu, col=4, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")),  
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


Glu2=drm(IR~RateUS, subset=Stage=="Rtwo",  fct=l3 (fixed =c(NA,70,NA)), data= Glufosinate)
modelFit(Gly2)
summary(Gly2)


plot(Glu2, add=T, col=2, legend=F, lty=1, pch=2, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=4, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


legend ("topleft", legend= c ("Glufosinate-Resistant Soybeans"),
        cex=2, bty="n")
legend ("bottomright", legend= c ("Second Trifoliate", "Full Flower"),
        cex=2,
        col=c(4,2), lty=c(1,1), pch=c(1,2), lwd=4, bty="n")

#______________________________________________________________________________________________

Conventional=read.csv("Conventional.csv")
str(Conventional)

x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Con=drm(IR~RateUS, subset=Stage=="2nd",  fct=l3 (fixed =c(NA,100,NA)), data= Conventional)
modelFit(Con)
summary(Con)



plot(Con, col=4, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


Con2=drm(IR~RateUS, subset=Stage=="Rtwo",  fct=l3 (fixed =c(NA,70,NA)), data= Conventional)
modelFit(Con2)
summary(Con2)


plot(Con2, add=T, col=2, legend=F, lty=1, pch=2, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")),  
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

legend ("topleft", legend= c ("Conventional Soybeans"),
        cex=2, bty="n")
legend ("bottomright", legend= c ("Second Trifoliate", "Full Flower"),
        cex=2,
        col=c(4,2), lty=c(1,1), pch=c(1,2), lwd=4, bty="n")

#______________________________________________________________________________________________


Organic=read.csv("Organic.csv")
str(Organic)

x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Org=drm(IR~RateUS, subset=Stage=="2nd",  fct=l3 (fixed =c(NA,100,NA)), data= Organic)
modelFit(Org)
summary(Org)



plot(Org, col=4, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")),  
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


Org2=drm(IR~RateUS, subset=Stage=="Rtwo",  fct=l3 (fixed =c(NA,100,NA)), data= Organic)
modelFit(Org2)
summary(Org2)


plot(Org2, add=T, col=2, legend=F, lty=1, pch=2, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

legend ("topleft", legend= c ("Organic Soybeans"),
        cex=2, bty="n")
legend ("bottomright", legend= c ("Second Trifoliate", "Full Flower"),
        cex=2,
        col=c(4,2), lty=c(1,1), pch=c(1,2), lwd=4, bty="n")


#______________________________________________________________________________________________

Grape=read.csv("Grape.csv")
str(Grape)

x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Gr=drm(IR~RateUS, subset=Stage=="2nd",  fct=l3 (fixed =c(NA,100,NA)), data= Grape)
modelFit(Gr)
summary(Gr)



plot(Gr, col=4, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


Gr2=drm(IR~RateUS, subset=Stage=="Rtwo",  fct=l3 (fixed =c(NA,30,NA)), data= Grape)
modelFit(Gr2)
summary(Gr2)


plot(Gr2, add=T, col=2, legend=F, lty=1, pch=2, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (g ae ha "^"-1",")")), 
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

legend ("topleft", legend= c ("Grape"),
        cex=2, bty="n")
legend ("bottomright", legend= c ("Second Trifoliate", "Full Flower"),
        cex=2,
        col=c(4,2), lty=c(1,1), pch=c(1,2), lwd=4, bty="n")

#______________________________________________________________________________________________

Tomato=read.csv("Tomato.csv")
str(Tomato)

x11()
par(mar=c(5,5,1,2), mgp=c(3.0,1,0))
Tom=drm(IR~RateUS, subset=Stage=="2nd",  fct=l3 (fixed =c(NA,100,NA)), data= Tomato)
modelFit(Tom)
summary(Tom)



plot(Tom, col=4, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")),  
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))


Tom2=drm(IR~RateUS, subset=Stage=="Rtwo",  fct=l3 (fixed =c(NA,70,NA)), data= Tomato)
modelFit(Tom2)
summary(Tom2)


plot(Tom2, add=T, col=2, legend=F, lty=1, pch=1, cex.axis = 1.8, cex.lab=2, cex=2,
     xlab = expression(paste("Clarity (oz acre "^"-1",")")),  
     ylab = expression(paste("Crop Injury (%)")), lwd=5, 
     bp=0.0001, xlim=c(0,100000), ylim=c(0,100))

legend ("topleft", legend= c ("Tomato"),
        cex=2, bty="n")
legend ("bottomright", legend= c ("Second Trifoliate", "Full Flower"),
        cex=2,
        col=c(4,2), lty=c(1,1), pch=c(1,2), lwd=4, bty="n")
