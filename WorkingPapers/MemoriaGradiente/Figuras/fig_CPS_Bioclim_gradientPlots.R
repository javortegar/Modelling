# source("D:/RRNNRR/RStudioTraining/DEM tesis/Hueicolla/Altura_promedio_sitios.R")

rm(list=setdiff(ls(),c("altitud")))

mypath <- "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Tesis/Climate_tesis"
cpsv6 <- read.table(file.path(mypath, "ClimaticParameters_sites_transect_v6.txt"), header=TRUE)
ix <- c(2,8,9,10,13,15,16,17,18,19,20,23,24,25,27,28,30,38,40,41,43,44,45,46,47,48,49,52,53,54,55,57,58,59,60,62,63,64,65,66,67,68) #CR2 - puntos_transecto (mayo/2020)
est_tm <- read.csv(file.path(mypath, "estacionesResumen_tmean.csv"), header=TRUE)
est_ppt <- read.csv(file.path(mypath, "estacionesResumen_ppt.csv"), header=TRUE)

altitud <- read.csv(file.path(mypath, "altura_promedio_27sites_tesis.csv"), row.names = NULL)
altitud$X <- NULL

mypath <- "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Tesis/Climate_tesis/dataBioclim"
bioclim_cr2 <- read.table(file.path(mypath, "bioclim_table_CR2_simus_v4.txt"), header = TRUE)

par_orig <- par()


############### version 2: Grafico de parametros climaticos de mis sitios de simulacion con CR2 y ERA
# tiff(filename="C:/Users/pepe/Downloads/PPR_nueva_exFig6-7_bw.tiff",width=2419, height=1915, units="px", compression = c("none"), res=300)
## PRECIPITACION ANUAL
par(mfrow=c(5,1), mar=c(1,4,1,4), cex=0.8, omi=c(0.9,0,0,0), family="serif")
plot(altitud$alt_prom, ylab=NA,ylim=c(0,2500), xlab = NA, type = "l", col = "#EEEEEE", xaxt="n", yaxt="n")
mtext("  a) Precipitación anual acumulada", line=0, side=3, adj=0, las=1, cex=0.8)
x2 <- c(1, seq(1:27), 27)
y2 <- c(0, as.numeric(altitud[,2]), 0)
polygon(x2,y2, col="#EEEEEE", border=NA)
axis(side=4, at = c(0,500,1500,2500), cex=0.8, las=2)
# mtext(side=4, "Altitud (m)", cex= 0.8, line=3)
par(new=TRUE)
# plot(cpsv6$sumP[-ix], main="", xlab=NA, type = "p", pch=16, col="blue", xaxt="n", yaxt="n", ylab="", ylim = c(0, 5200)) # azul
plot(cpsv6$sumP[-ix], main="", xlab=NA, type = "p", pch=16, xaxt="n", yaxt="n", ylab="", ylim = c(0, 5200)) # monocromatico
axis(side=2, at = seq(from = 0, to = 5500, by = 1500), cex=0.8,las=2)
# lines(cpsv6$sumP[-ix], col="blue") #azul
lines(cpsv6$sumP[-ix])
mtext(side=2, "mm", cex=0.8, line=3)
xp <- c(9, 10, 11, 11, 7, 2, 24, 27, 26,25,23,22)
name_est <- c("TRI", "RUP", "FUT", "ANT", "VEN", "LC", "LB", "JUN", "BAR", "LM", "PL") #se eliminó AdolfoMatthei
points(x=xp[-4], y=est_ppt$PPT_ANUAL[-4], pch=24, col="black", bg="white", cex=1)
text(x=xp[-4], y=est_ppt$PPT_ANUAL[-4], labels=name_est, cex=0.6, pos=c(rep(1,4),3,3,3,3,1), adj=1)
segments(6.5,0,6.5,6000, lty=2, col="darkgray")
segments(9.5,0,9.5,6000, lty=2, col="darkgray")
segments(24.5,0,24.5,6000, lty=2, col="darkgray")
# legend( "topright", c("Precipitación anual","Altitud", "Estaciones meteorológicas"), 
        # pch = c(NA, 15, 24), 
        # col= c("darkblue","darkgrey", "brown"),
        # lty=c(1,NA,NA), 
        # inset=c(0,0), cex=0.7, pt.bg = c(NA,NA,"darkorange"))
## TEMPERATURA MEDIA ANUAL
plot(altitud$alt_prom, ylab=NA,ylim=c(0,2500), xlab = NA, type = "l", col = "#EEEEEE", xaxt="n", yaxt="n")
mtext("  b) Temperatura media anual", line=0, side=3, adj=0, las=1, cex=0.8)
x2 <- c(1, seq(1:27), 27)
y2 <- c(0, as.numeric(altitud[,2]), 0)
polygon(x2,y2, col="#EEEEEE", border=NA)
axis(side=4, at = c(0,500,1500,2500), cex=0.8, las=2)
# mtext(side=4, "Altitud (m)", cex= 0.8, line=3)
par(new=TRUE)
# plot(cpsv6$annualT[-ix], main = "", ylab=NA,
     # ylim= c(0,13), xlab=NA, type = "p", col="red", pch=16, xaxt="n",yaxt="n") # rojo
plot(cpsv6$annualT[-ix], main = "", ylab=NA,
 ylim= c(0,13), xlab=NA, type = "p", pch=16, xaxt="n",yaxt="n") #monocromatico
# lines(cpsv6$annualT[-ix], col="red") # rojo
lines(cpsv6$annualT[-ix])
axis(side=2, at = seq(from = 0, to = 12, by = 4), cex=0.8, las = 2)
mtext(side=2, "°C", cex=0.8, line=3)
xt <- c(8, 25, 8, 11, 6, 12)
name_tm <- c("OSO", "BAR", "RUP", "QLC", "RUC", "")#Se eliminó palermo-la union
points(x=xt[-3], y=est_tm$TEMP_ANUAL[-3], pch=24, col="black", bg="white", cex=1)
text(x=xt[-3], y=est_tm$TEMP_ANUAL[-3], labels=name_tm, cex=0.6, pos=1, adj=1)
segments(6.5,-6,6.5,15, lty=2, col="darkgray")
segments(9.5,-6,9.5,15, lty=2, col="darkgray")
segments(24.5,-6,24.5,15, lty=2, col="darkgray")
# legend( "topright", c("Temperatura media anual"), 
        # col= c("darkred"), 
        # lty=1,
        # inset=c(0,0), cex=0.7)
## TEMPERATURA MINIMA DE INVIERNO
# par(mfrow=c(3,1), mar=c(1,6,1,6), cex=0.8, omi=c(0.9,0,0.3,0))
plot(altitud$alt_prom, ylab=NA,ylim=c(0,2500), xlab = NA, type = "l", col = "#EEEEEE", xaxt="n", yaxt="n")
mtext("  c) Temperatura mínima de invierno", line=0, side=3, adj=0, las=1, cex=0.8)
x2 <- c(1, seq(1:27), 27)
y2 <- c(0, as.numeric(altitud[,2]), 0)
polygon(x2,y2, col="#EEEEEE", border=NA)
axis(side=4, at = c(0,500,1500,2500), cex=0.8, las=2)
mtext(side=4, "Altitud (m)", cex= 0.8, line=3)
par(new=TRUE)
plot(bioclim_cr2$mWiT, main="", ylab="°C", xaxt="n", las=2,yaxt="n",
     type="p", pch=19)
lines(bioclim_cr2$mWiT[1:27], lty=1)
# mtext(side=3, "a) Temperatura mínima de invierno (°C)", cex= 0.8, line=1)
axis(side=2, at = seq(from = -6, to = 6, by = 3), cex=0.8, las=2)
segments(6.5,-6,6.5,7.8, lty=2, col="darkgray")
segments(9.5,-6,9.5,10, lty=2, col="darkgray")
segments(24.5,-6,24.5,10, lty=2, col="darkgray")
# legend( "topright", c("Temperatura mínima de invierno"), lty=1, inset=c(0,0), cex=0.7)

## SUMA MINIMA DE DIAS GRADOS
plot(altitud$alt_prom, ylab=NA,ylim=c(0,2500), xlab = NA, type = "l", col = "#EEEEEE", xaxt="n", yaxt="n")
mtext("  d) Suma anual mínima de días-grados", line=0, side=3, adj=0, las=1, cex=0.8)
x2 <- c(1, seq(1:27), 27)
y2 <- c(0, as.numeric(altitud[,2]), 0)
polygon(x2,y2, col="#EEEEEE", border=NA)
axis(side=4, at = c(0,500,1500,2500), cex=0.8, las=2)
# mtext(side=4, "Altitud (m)", cex= 0.8, line=3)
par(new=TRUE)
plot(bioclim_cr2$DDAn, main="", ylab="°C·día", xaxt="n", las=2,
     type="p", pch=19)
lines(bioclim_cr2$DDAn, lty=1)
# mtext(side=3, "b) Suma mínima de dias-grados (°C·día)", cex= 0.8, line=0.5)
axis(side=2, at = seq(from = 0, to = 2500, by = 500), cex=0.8, las = 2)
segments(6.5,0,6.5,3000, lty=2, col="darkgray")
segments(9.5,0,9.5,3000, lty=2, col="darkgray")
segments(24.5,0,24.5,3000, lty=2, col="darkgray")
# legend( "topright", c("Suma mínima de dias-grados"), lty=1, inset=c(0,0), cex=0.7)
#### INDICE DE SEQUIA

plot(altitud$alt_prom, ylab=NA,ylim=c(0,2500), xlab = NA, type = "l", col = "#EEEEEE", xaxt="n", yaxt="n")
mtext("   e) Índice de sequía anual", line=0, side=3, adj=0, las=1, cex=0.8)
x2 <- c(1, seq(1:27), 27)
y2 <- c(0, as.numeric(altitud[,2]), 0)
polygon(x2,y2, col="#EEEEEE", border=NA)
axis(side=4, at = c(0,500,1500,2500), cex=0.8, las=2)
# mtext(side=4, "Altitud (m)", cex= 0.8, line=3)
par(new=TRUE)
plot(bioclim_cr2$DrAn, main="", ylab=NA, xaxt="n", las=2,
     type="p", pch=19, ylim = c(0,0.5),yaxt="n")
axis(side=2, at = seq(0,0.5,0.2), cex=0.8, las=2)
lines(bioclim_cr2$DrAn, lty=1)
# legend( "topleft", c("Índice de sequía"), lty=1, inset=c(0,0), cex=0.7)
axis(1, at = 1:27, labels=round(cpsv6$long[-ix], digits=2), las=2)
# mtext(side=3, "c) Índice de sequía", cex=0.8, line=0.5)
segments(6.5,0,6.5,1, lty=2, col="darkgray")
segments(9.5,0,9.5,1, lty=2, col="darkgray")
segments(24.5,0,24.5,1, lty=2, col="darkgray")

axis(side=1, at=c(0, 6.5, 9.5, 24.5, 28), line=4, labels=FALSE)
mtext(side=1, "Costa", line=4, at=3.5,cex=0.8)
mtext(side=1, "Depresión", line=4, at=8, cex=0.8)
mtext(side=1, "Intermedia", line=5, at=8, cex=0.8)
mtext(side=1, "Andes", line=4, at=17, cex=0.8)
mtext(side=1, "Estepa", line=4, at=26, cex=0.8)


# dev.off()
