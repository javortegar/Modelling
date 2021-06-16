# TODO: Add comment
# 
# Author: Javier
# Grafico de patron de estructura para el transecto en Ecorregion Valdiviana
###############################################################################

rm(list=ls(all=TRUE))

# rm(list=setdiff(ls(),c("datos.swiss_1", "datos.swiss_2", "datos.swiss_3", "sitenames1", "sitenames2", "bgfill", "spp.names", "y_lim")))

mypath <- "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Tesis/Climate_tesis"
cpsv6 <- read.table(file.path(mypath, "ClimaticParameters_sites_transect_v6.txt"), header=TRUE)
ix <- c(2,8,9,10,13,15,16,17,18,19,20,23,24,25,27,28,30,38,40,41,43,44,45,46,47,48,49,52,53,54,55,57,58,59,60,62,63,64,65,66,67,68) #CR2 - puntos_transecto (mayo/2020)
long <- cpsv6$long[-ix]

#color function
col.f <- function(x.df)            
{                              
  tmpred <- x.df
  x <- colnames(tmpred)
  color <- rep("white", ncol(tmpred))  
  color [(x == spp.names[1])] <-  "#c164a6" #AEPU
  color [(x == spp.names[2])] <-  "darkolivegreen" #AMLU
  color [(x == spp.names[3])] <- "darkred" #AMME
  color [(x == spp.names[4])] <- "purple" #AUCH
  color [(x == spp.names[5])] <- "#d9ff73" #CAPA
  color [(x == spp.names[6])] <- "#dfffdf" #DADI
  color [(x == spp.names[7])] <-  "#FF7F00" #DRWI
  color [(x == spp.names[8])] <-  "blue" #EMCO
  color [(x == spp.names[9])] <-  "#A6CEE3" #EUCO
  color [(x == spp.names[10])] <-  "antiquewhite2" #FICU - Piso veg P77
  color [(x == spp.names[11])] <- "#610093" #GEAV
  color [(x == spp.names[12])] <-  "springgreen"#LAPH - Piso veg P72
  color [(x == spp.names[13])] <-  "#7c7c7c" #LASE
  color [(x == spp.names[14])] <-  "#704A00" #LOFE
  color [(x == spp.names[15])] <- "orangered" #LOHI
  color [(x == spp.names[16])] <- "#E31A1C" #LUAP
  color [(x == spp.names[17])] <- "#CAB2D6" #MYOV
  color [(x == spp.names[18])] <- "#B2DF8A" #MYPL
  color [(x == spp.names[19])] <- "#1F78B4" #NOAN
  color [(x == spp.names[20])] <- "royalblue" #NOBE
  color [(x == spp.names[21])] <- "royalblue4" #NODO - Piso veg P73
  #color [(x == spp.names[22])] <- "orange3" #NONE-NOAL
  #color [(x == spp.names[23])] <- "yellow" #NONI
  color [(x == spp.names[22])] <- "#006767" #NOOB - Piso veg P54
  color [(x == spp.names[23])] <- "#B15928" #NOPU - Piso veg P62
  color [(x == spp.names[24])] <- "#FDBF6F" #PELI
  #color [(x == spp.names[25])] <- "pink" #PIUV
  color [(x == spp.names[25])] <- "#FFFF99" #PONU
  color [(x == spp.names[26])] <- "grey" #POSA
  color [(x == spp.names[27])] <- "yellowgreen" #SACO
  color [(x == spp.names[28])] <- "deeppink2" #TEST
  color [(x == spp.names[29])] <- "#009155" #WETR - Piso veg P82
  #color [(x == spp.names[33])] <- "white" #Others
  color
}


#----------- FIG. A Worldclim data

spp <- c("Aepu", "Amlu", "Amme", "Auch", "Capa", "Dadi", "Drwi", "Emco", "Euco", 
         "Ficu", "Geav", "Laph", "Lase", "Lofe", "Lohi", "Luap", "Myov", "Mypl",
         "Noan", "Nobe", "Nodo", "Noob", "Nopu", "Peli", "Ponu", "Posa", "Saco", 
         "Test", "Wetr")

mypath <- "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Paper/kLatNuevo/Results/SimusMultispecific"

version <- "v6"

result.table <- read.csv(file.path(mypath, paste("Spp_",version, "/Biometria_from_DATfile_Multi_exp_Spp_",version,".csv", sep="")), header=TRUE)
riq_spp <- read.csv(file.path(mypath, paste("Spp_",version, "/riqueza_acuerdos_spp",version,".csv", sep="")), header=TRUE)

biomasa <- NULL
for (i in 1:length(spp)){
  subdata <- subset(result.table, result.table$spp == spp[i]) #Aqui definir si es CON umbral o SIN umbral
  subdata <- subdata[order(subdata$site),]
  dummy <- c(spp[i], subdata$biomasa)
  biomasa <- rbind(biomasa, dummy)
}

biomasa <- as.data.frame(biomasa)
names(biomasa) <- c("spp", as.character(subdata$site))

result.table <- t(apply(biomasa, 2, as.numeric))
result.table <- result.table[2:length(result.table[,1]),]

simu1 <- as.data.frame(result.table)

sitenames1 <- c(sprintf("site0%d",seq(1:9)), sprintf("site%d",seq(from = 10, to=27)))

site_esp <- gsub("site", "Sitio ", sitenames1)
#datos.swiss <- datos.swiss[,2:ncol(datos.swiss)]

spp.names <- c("A. punctatum", "A. luma", "A. meli", "A. chilensis", "C. paniculata", "D. diacanthoides", "D. winteri", 
               "E. coccineum", "E. cordifolia", "F. cupressoides", "G. avellana", "L. philippiana", "L. sempervirens", 
               "L. ferruginea", "L. hirsuta", "L. apiculata", "M. ovata","M. planipes", "N. antarctica", "N. betuloides", 
               "N. dombeyi", "N. obliqua","N. pumilio","P. lingue","P. nubigena","P. saligna", "S. conspicua" ,
               "T. stipularis", "W. trichosperma")

acu_pres_1 <- round(riq_spp$acuerdos_1/29*100, digits=1)
acu_pres_0 <- round(riq_spp$acuerdos_0/29*100, digits=1)
comi_p <- round(riq_spp$comisiones/29*100, digits=1)
omi_p <- round(riq_spp$omisiones/29*100, digits=1)

percent <- cbind(acu_pres_1, acu_pres_0, comi_p, omi_p)

names(simu1)<-spp.names

bgfill <- col.f(simu1)
# }

percent <- as.data.frame(percent)
row.names(percent) <- sitenames1

##

jaccard <- read.csv(file.path(mypath, paste("Spp_",version, "/jaccard_Exp_Multi_Spp_TRF_PPR_",version,".csv", sep="")), header=TRUE)

par_orig <- par()

biomasa <- simu1

# tiff(filename="C:/Users/pepe/Downloads/PPR_nueva_exFig9_simuv6.tiff",width=1814, height=1525, units="px", compression = c("none"), res=300)
par(mfrow=c(3,1), cex.lab=0.8, mar=c(0,4,1,0), oma=c(7,0,0,1), family="serif")
b <- barplot(t(biomasa), ylab="Índice Jaccard", main="",
             col = "white", border = "white", xaxt="n", ylim = c(-0.1,1), las=1, cex.lab=1.2)
par(new=T)
# lines(b, jaccard$similarity, lwd=1, col = "darkred")
lines(b, jaccard$similarity, lwd=1)
points(b,jaccard$similarity, ylim=c(0,1), main = "",# las=2,
       type = "p", pch=16, ylab="Índice Jaccard", xaxt="n", yaxt="n",xlab="", las=1)
mtext("  a)", line=-1, side=3, adj=0, las=1, cex=0.8)
segments(7.3,0,7.3,250, lty=2)
segments(10.9,0,10.9,250, lty=2)
segments(28.9,0,28.9,250, lty=2)

barplot(t(percent), col=c("#000000","#808080","black", "#FFFFFF"), 
        density=c(NA,NA,20,0),
        las=1, ylab="%", cex.lab=1.2, las=1, xaxt="n")
mtext("  b)", line=-1, side=3, adj=0, las=1, cex=0.8)
segments(7.3,0,7.3,250, lty=2)
segments(10.9,0,10.9,250, lty=2)
segments(28.9,0,28.9,250, lty=2)

barplot(t(percent), las=1, ylim=c(-1,30), col="white", border="white", xaxt="n", cex.lab=1.2, ylab="# especies")

points(x=b, y=riq_spp$acuerdos_1+riq_spp$comisiones, pch=1, xaxt="n")
for(i in 1:length(riq_spp$obs)){
  segments(b[i],0, b[i], riq_spp$obs[i], lty=3)}
points(x=b, y=riq_spp$obs, pch=4, cex=1)
mtext("  c)", line=-1, side=3, adj=0, las=1, cex=0.8)
segments(7.3,0,7.3,250, lty=2)
segments(10.9,0,10.9,250, lty=2)
segments(28.9,0,28.9,250, lty=2)
axis(1, at = b, labels=round(long, digits=2), las=2)
legend("topright", c("Presencia", "Ausencia", "Comisión", "Omisión","","","",""), 
       fill=c("#000000","#808080","black", "#FFFFFF", rep("white",4)),
       density=c(NA,NA,30,rep(NA,5)),
       angle=c(NA,NA,45,rep(NA,5)),
       border=c(rep("black", 4), rep("white", 4)),
       cex=0.9, ncol=2, inset=c(0.05,0))

legend("topright", c("Predicho", "Observado"), pch=c(1,4),
       cex=0.9, ncol=1, inset=c(0.051,0), bty="n")

axis(side=1, at=c(0, 7.3, 10.9, 28.9, 33), line=4, labels=FALSE)
mtext(side=1, "Costa", line=4, at=3.7,cex=0.7)
mtext(side=1, "Depresión", line=4, at=9.1,cex=0.7)
mtext(side=1, "Intermedia", line=5, at=9.1,cex=0.7)
mtext(side=1, "Andes", line=4, at=19.9,cex=0.7)
mtext(side=1, "Estepa", line=4, at=30.7,cex=0.7)

# dev.off()