
#Barplot 21 DA second trifoliate

#ED50

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED, inside = T,
                      names.arg = Bar$ED,
                      beside = T, las = 2, 
                      ylim = c(0,25), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = F, axes = TRUE) 

box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
 #    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 14.5, "10.5", cex=2)
text(1.9, 8, "5.9", cex=2)
text(3.1, 8, "5.8", cex=2)
text(4.3, 3.3, "2.2", cex=2)
text(5.5, 13.5, "10.9", cex=2)
text(6.7, 22.5, "18.4", cex=2)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 2)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 
#___________________________________________________________________________________________________________________________________

#ED30

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED30,
                      names.arg = Bar$ED30,
                      beside = true, las = 2, 
                      ylim = c(0,10), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 

box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 2.1, "1.5", cex=2)
text(1.9, 1.8, "1.3", cex=2)
text(3.1, 1.3, "0.9", cex=2)
text(4.3, 0.9, "0.6", cex=2)
text(5.5, 8.1, "6.7", cex=2)
text(6.7, 7, "5.7", cex=2)

segments(barCenters, Bar$ED30 - Bar$SE30, barCenters,
         Bar$ED30 + Bar$SE30, lwd = 2)

arrows(barCenters, Bar$ED30 - Bar$SE30, barCenters,
       Bar$ED30 + Bar$SE30, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 

#_______________________________________________________________________________

#ED20

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED20,
                      names.arg = Bar$ED20,
                      beside = true, las = 2, 
                      ylim = c(0,8), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 

box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 0.8, "0.43", cex=2)
text(1.9, 0.9, "0.52", cex=2) 
text(3.1, 0.56, "0.26", cex=2)
text(4.3, 0.55, "0.24", cex=2)
text(5.5, 6.3, "4.9", cex=2)
text(6.7, 3.6, "2.72", cex=2)

segments(barCenters, Bar$ED20 - Bar$SE20, barCenters,
         Bar$ED20 + Bar$SE20, lwd = 2)

arrows(barCenters, Bar$ED20 - Bar$SE20, barCenters,
       Bar$ED20 + Bar$SE20, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 

#_______________________________________________________________________________

#ED 10

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED10,
                      names.arg = Bar$ED10,
                      beside = true, las = 2, 
                      ylim = c(0,5), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 

box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 0.2, "0.06", cex=2)
text(1.9, 0.3, "0.1", cex=2)
text(3.1, 0.2, "0.04", cex=2)
text(4.3, 0.2, "0.06", cex=2)
text(5.5, 4.3, "3.1", cex=2)
text(6.7, 1.3, "0.9", cex=2)

segments(barCenters, Bar$ED10 - Bar$SE10, barCenters,
         Bar$ED10 + Bar$SE10, lwd = 2)

arrows(barCenters, Bar$ED10 - Bar$SE10, barCenters,
       Bar$ED10 + Bar$SE10, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 



#__________________________________________________________________________________________________________________











#Barplot 21 DA R2

#ED50

Bar=read.csv("Barplot2.csv")
str(Bar)

par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED50,
                      names.arg = Bar$ED50,
                      beside = true, las = 2, 
                      ylim = c(0,350), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 
box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 137, "95.2", cex=2)
text(1.9, 81, "54.9", cex=2)
text(3.1, 256, "182.4", cex=2)
text(4.3, 26, "16.7", cex=2)
text(5.5, 10, "NA", cex=2)
text(6.7, 100, "53.1", cex=2)

segments(barCenters, Bar$ED50 - Bar$SE50, barCenters,
         Bar$ED50 + Bar$SE50, lwd = 2)

arrows(barCenters, Bar$ED50 - Bar$SE50, barCenters,
       Bar$ED50 + Bar$SE50, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 

#_____________________________________________________________________________________________________________


Bar=read.csv("Barplot2.csv")
str(Bar)

par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED30,
                      names.arg = Bar$ED30,
                      beside = true, las = 2, 
                      ylim = c(0,20), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 
box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 11, "8.9", cex=2)
text(1.9, 9, "6.8", cex=2)
text(3.1, 12.5, "10.2", cex=2)
text(4.3, 3.8, "2.8", cex=2)
text(5.5, 0.5, "NA", cex=2)
text(6.7, 6.5, "4.4", cex=2)

segments(barCenters, Bar$ED30 - Bar$SE30, barCenters,
         Bar$ED30 + Bar$SE30, lwd = 2)

arrows(barCenters, Bar$ED30 - Bar$SE30, barCenters,
       Bar$ED30 + Bar$SE30, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 

#_______________________________________________________________________
#ED20

Bar=read.csv("Barplot2.csv")
str(Bar)

x11()
par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED20,
                      names.arg = Bar$ED20,
                      beside = true, las = 2, 
                      ylim = c(0,150), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 

box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 7, "2.6", cex=2)
text(1.9, 7, "1.7", cex=2) 
text(3.1, 8, "2.3", cex=2)
text(4.3, 6, "0.9", cex=2)
text(5.5, 145, "79.9", cex=2)
text(6.7, 6, "1.2", cex=2)

segments(barCenters, Bar$ED20 - Bar$SE20, barCenters,
         Bar$ED20 + Bar$SE20, lwd = 2)

arrows(barCenters, Bar$ED20 - Bar$SE20, barCenters,
       Bar$ED20 + Bar$SE20, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 
#______________________________________________________________________________________

#ED10
Bar=read.csv("Barplot2.csv")
str(Bar)

par(mar=c(1,5,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED10,
                      names.arg = Bar$ED10,
                      beside = true, las = 2, 
                      ylim = c(0,7), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Dicamba (g ae ha "^"-1",")")),
                      border = "NA", axes = TRUE) 
box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = Bar$Treat, cex=2, xpd = TRUE)

segments(barCenters, Bar$ED - Bar$SE, barCenters,
         Bar$ED + Bar$SE, lwd = 1.5)

arrows(barCenters, Bar$ED - Bar$SE, barCenters,
       Bar$ED + Bar$SE, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 0.8, "0.5", cex=2)
text(1.9, 0.5, "0.2", cex=2)
text(3.1, 0.6, "0.3", cex=2)
text(4.3, 0.5, "0.2", cex=2)
text(5.5, 6, "4.0", cex=2)
text(6.7, 0.5, "0.2", cex=2)

segments(barCenters, Bar$ED10 - Bar$SE10, barCenters,
         Bar$ED10 + Bar$SE10, lwd = 2)

arrows(barCenters, Bar$ED10 - Bar$SE10, barCenters,
       Bar$ED10 + Bar$SE10, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Dicamba label rate 560 (g ae ha "^"-1",")")), cex=2,
        col="black", bty="n") 
