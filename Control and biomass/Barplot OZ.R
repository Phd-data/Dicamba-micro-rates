
#Barplot 21 DA second trifoliate

#ED50

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,6,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$EDOZ, inside = T,
                      names.arg = Bar$EDOZ,
                      beside = T, las = 2, 
                      ylim = c(0,1), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Clarity (oz acre "^"-1",")")),
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

text(0.7, 0.41, "0.3", cex=2)
text(1.9, 0.22, "0.16", cex=2)
text(3.1, 0.24, "0.16", cex=2)
text(4.3, 0.1, "0.06", cex=2)
text(5.5, 0.39, "0.31", cex=2)
text(6.7, 0.64, "0.52", cex=2)

segments(barCenters, Bar$EDOZ - Bar$SEOZ, barCenters,
         Bar$EDOZ + Bar$SEOZ, lwd = 2)

arrows(barCenters, Bar$EDOZ - Bar$SEOZ, barCenters,
       Bar$EDOZ + Bar$SEOZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 
#___________________________________________________________________________________________________________________________________

#ED30

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,6,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED30OZ,
                      names.arg = Bar$ED30OZ,
                      beside = true, las = 2, 
                      ylim = c(0,0.3), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      yaxt="n",  
                      border = "NA", axes = TRUE) 
axis(2, las=2, cex.axis=1.8)
mtext(expression(paste("Clarity (oz acre "^"-1",")")),cex=2, side=2,col=1,line=4) 
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

text(0.7, 0.06, "0.04", cex=2)
text(1.9, 0.06, "0.04", cex=2)
text(3.1, 0.05, "0.03", cex=2)
text(4.3, 0.03, "0.02", cex=2)
text(5.5, 0.23, "0.19", cex=2)
text(6.7, 0.2, "0.16", cex=2)

segments(barCenters, Bar$ED30OZ - Bar$SE30OZ, barCenters,
         Bar$ED30OZ + Bar$SE30OZ, lwd = 2)

arrows(barCenters, Bar$ED30OZ - Bar$SE30OZ, barCenters,
       Bar$ED30OZ + Bar$SE30OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 

#_______________________________________________________________________________
#ED20

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,7,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED20OZ,
                      names.arg = Bar$ED20OZ,
                      beside = true, las = 2, 
                      ylim = c(0,0.2), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = "",
                      border = "NA", axes = TRUE) 
axis(2, las=2, cex.axis=1.8)
mtext(expression(paste("Clarity (oz acre "^"-1",")")),cex=2, side=2,col=1,line=4) 
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

text(0.7, 0.02, "0.01", cex=2)
text(1.9, 0.02, "0.01", cex=2) 
text(3.1, 0.016, "0.007", cex=2)
text(4.3, 0.015, "0.006", cex=2)
text(5.5, 0.175, "0.14", cex=2)
text(6.7, 0.105, "0.08", cex=2)

segments(barCenters, Bar$ED20OZ - Bar$SE20OZ, barCenters,
         Bar$ED20OZ + Bar$SE20OZ, lwd = 2)

arrows(barCenters, Bar$ED20OZ - Bar$SE20OZ, barCenters,
       Bar$ED20OZ + Bar$SE20OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 
#______________________________________________________________________________________

#ED 10

Bar=read.csv("Barplot.csv")
str(Bar)

x11()
par(mar=c(1,7,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED10OZ,
                      names.arg = Bar$ED10OZ,
                      beside = true, las = 2, 
                      ylim = c(0,0.12), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = "",
                      border = "NA", axes = TRUE) 
axis(2, las=2, cex.axis=1.8)
mtext(expression(paste("Clarity (oz acre "^"-1",")")),cex=2, side=2,col=1,line=4) 
box()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 0.006, "0.002", cex=2)
text(1.9, 0.008, "0.003", cex=2)
text(3.1, 0.006, "0.001", cex=2)
text(4.3, 0.008, "0.002", cex=2)
text(5.5, 0.115, "0.08", cex=2)
text(6.7, 0.035, "0.02", cex=2)

segments(barCenters, Bar$ED10OZ - Bar$SE10OZ, barCenters,
         Bar$ED10OZ + Bar$SE10OZ, lwd = 2)

arrows(barCenters, Bar$ED10OZ - Bar$SE10OZ, barCenters,
       Bar$ED10OZ + Bar$SE10OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 


#____________________________________________________________________________________

Bar=read.csv("Barplot2.csv")
str(Bar)

par(mar=c(1,6,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED50OZ,
                      names.arg = Bar$ED50OZ,
                      beside = true, las = 2, 
                      ylim = c(0,10), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Clarity (oz acre "^"-1",")")),
                      border = "NA", axes = TRUE) 
box()

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 3.9, "2.7", cex=2)
text(1.9, 2.8, "1.7", cex=2)
text(3.1, 7.3, "5.2", cex=2)
text(4.3, 0.8, "0.5", cex=2)
text(5.5, 0.3, "NA", cex=2)
text(6.7, 2.9, "1.5", cex=2)

segments(barCenters, Bar$ED50OZ - Bar$SE50OZ, barCenters,
         Bar$ED50OZ + Bar$SE50OZ, lwd = 2)

arrows(barCenters, Bar$ED50OZ - Bar$SE50OZ, barCenters,
       Bar$ED50OZ + Bar$SE50OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Clarity label rate 16 (oz acre"^"-1",")")), cex=2,
        col="black", bty="n") 

#_______________________________________________________________________

#stoped here
Bar=read.csv("Barplot2.csv")
str(Bar)

par(mar=c(1,6,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED30OZ,
                      names.arg = Bar$ED30OZ,
                      beside = true, las = 2, 
                      ylim = c(0,0.8), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Clarity (oz acre "^"-1",")")),
                      border = "NA", axes = TRUE) 
box()


# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 0.36, "0.3", cex=2)
text(1.9, 0.26, "0.2", cex=2)
text(3.1, 0.37, "0.3", cex=2)
text(4.3, 0.11, "0.08", cex=2)
text(5.5, 0.02, "NA", cex=2)
text(6.7, 0.17, "0.1", cex=2)

segments(barCenters, Bar$ED30OZ - Bar$SE30OZ, barCenters,
         Bar$ED30OZ + Bar$SE30OZ, lwd = 2)

arrows(barCenters, Bar$ED30OZ - Bar$SE30OZ, barCenters,
       Bar$ED30OZ + Bar$SE30OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 

#_______________________________________________________________________
#ED20

Bar=read.csv("Barplot2.csv")
str(Bar)

x11()
par(mar=c(1,6,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED20OZ,
                      names.arg = Bar$ED20OZ,
                      beside = true, las = 2, 
                      ylim = c(0,5), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = expression(paste("Clarity (oz acre "^"-1",")")),
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

text(0.7, 0.2, "0.07", cex=2)
text(1.9, 0.2, "0.04", cex=2) 
text(3.1, 0.2, "0.06", cex=2)
text(4.3, 0.2, "0.03", cex=2)
text(5.5, 4.2, "2.3", cex=2)
text(6.7, 0.2, "0.03", cex=2)

segments(barCenters, Bar$ED20OZ - Bar$SE20OZ, barCenters,
         Bar$ED20OZ + Bar$SE20OZ, lwd = 2)

arrows(barCenters, Bar$ED20OZ - Bar$SE20OZ, barCenters,
       Bar$ED20OZ + Bar$SE20OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend ("left", legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 
#______________________________________________________________________________________

#stoped here
Bar=read.csv("Barplot2.csv")
str(Bar)

par(mar=c(1,7,1,2), mgp=c(3.0,1,0))
barCenters <- barplot(height = Bar$ED10OZ,
                      names.arg = Bar$ED10OZ,
                      beside = true, las = 2, 
                      ylim = c(0,0.2), cex.lab=2.0, cex.axis = 1.8,
                      col=c("orange","gray48", "pink", "blue", "green", "red"),
                      cex.names = 0.75, xaxt = "n", 
                      ylab = "",
                      border = "NA", axes = TRUE) 
box()
axis(2, las=2, cex.axis=1.8)
mtext(expression(paste("Clarity (oz acre "^"-1",")")),cex=2, side=2,col=1,line=4) 


# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
#    adj = 1, labels = Bar$ED, cex=1.5, xpd = TRUE)

text(0.7, 0.02, "0.01", cex=2)
text(1.9, 0.02, "0.01", cex=2)
text(3.1, 0.017, "0.01", cex=2)
text(4.3, 0.01, "0.004", cex=2)
text(5.5, 0.145, "0.1", cex=2)
text(6.7, 0.02, "0.01", cex=2)

segments(barCenters, Bar$ED10OZ - Bar$SE10OZ, barCenters,
         Bar$ED10OZ + Bar$SE10OZ, lwd = 2)

arrows(barCenters, Bar$ED10OZ - Bar$SE10OZ, barCenters,
       Bar$ED10OZ + Bar$SE10OZ, lwd = 2, angle = 90,
       code = 3, length = 0.05)

legend ("topleft", legend= c ("Glyphosate-Resistant Soybeans", "Glufosinate-Resistant Soybeans","Conventional Soybeans", 
                              "Organic Soybeans", "Grape","Tomato"), cex=1.5, bg = "light blue",
        col=c("orange","gray48", "pink", "blue", "green", "red"), lty=c(1,1,1,1,1,1),lwd=20, bty="n")

legend (locator(1), legend= expression(paste("Clarity label rate 16 (oz acre "^"-1",")")), cex=2,
        col="black", bty="n") 
