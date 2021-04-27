# TODO: Add comment
# 
# Author: Alvaro + Javier
# Figura de patron estructural para el transecto en Ecorregion Valdiviana
# Se eliminan tres especies de las simulaciones: N. alpina, N. nitida, P. uviferum
# Eleccion nuevo sitio 36 - CPS_sites_v6.txt
###############################################################################

rm(list=ls(all=TRUE))

rm(list=setdiff(ls(),c("datos.swiss_1", "datos.swiss_2", "datos.swiss_3", "sitenames1", "sitenames2", "bgfill", "spp.names", "y_lim")))

mypath <- "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Tesis/Climate_tesis"
cpsv6 <- read.table(file.path(mypath, "ClimaticParameters_sites_transect_v6.txt"), header=TRUE)
ix <- c(2,8,9,10,13,15,16,17,18,19,20,23,24,25,27,28,30,38,40,41,43,44,45,46,47,48,49,52,53,54,55,57,58,59,60,62,63,64,65,66,67,68) #CR2 - puntos_transecto (mayo/2020)
long <- cpsv6$long[-ix]

base.path <-  "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Tesis/Exp_kG"

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

mypath <- "G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/CorrerForclim/Paper/kLatNuevo/Results/SimusMonospecific"

exp <- c("v3", "v4")

for (version in exp){
  
result.table <- read.csv(file.path(mypath, paste("Spp_",version, "/Biometria_Exp_Mono_Spp_TRF_PPR_",version,".csv", sep="")), header=TRUE)
  
areabasal <- NULL
for (i in 1:length(spp)){
  subdata <- subset(result.table, result.table$sp == spp[i]) #Aqui definir si es CON umbral o SIN umbral
  subdata <- subdata[order(subdata$site),]
  dummy <- c(spp[i], subdata$AB)
  areabasal <- rbind(areabasal, dummy)
}

areabasal <- as.data.frame(areabasal)
names(areabasal) <- c("spp", as.character(subdata$site[1:18]), "site36", as.character(subdata$site[19:25]), "site69")

result.table <- t(apply(areabasal, 2, as.numeric))
result.table <- result.table[2:length(result.table[,1]),]

if (version == exp[1]) simu1 <- as.data.frame(result.table)
if (version == exp[2]) simu2 <- as.data.frame(result.table)

#sitenames1 <- row.names(datos.swiss)

sitenames1 <- c(sprintf("site0%d",seq(1:9)), sprintf("site%d",seq(from = 10, to=27)))

site_esp <- gsub("site", "Sitio ", sitenames1)
#datos.swiss <- datos.swiss[,2:ncol(datos.swiss)]

spp.names <- c("A. punctatum", "A. luma", "A. meli", "A. chilensis", "C. paniculata", "D. diacanthoides", "D. winteri", 
               "E. coccineum", "E. cordifolia", "F. cupressoides", "G. avellana", "L. philippiana", "L. sempervirens", 
               "L. ferruginea", "L. hirsuta", "L. apiculata", "M. ovata","M. planipes", "N. antarctica", "N. betuloides", 
               "N. dombeyi", "N. obliqua","N. pumilio","P. lingue","P. nubigena","P. saligna", "S. conspicua" ,
               "T. stipularis", "W. trichosperma")

if (version == exp[1]) names(simu1)<-spp.names
if (version == exp[2]) names(simu2)<-spp.names

if (version == exp[1]) bgfill <- col.f(simu1)
if (version == exp[2]) bgfill <- col.f(simu2)
}

#Figura
par(mfrow=c(1,1),xpd=TRUE)

##################
#Grafico distribucion area basal

target <- c("Luap", "Saco","Geav") #ojo: el orden segun contraste de colores
ix.sp.c <- NULL
for(x in 1:length(target)){
  dummy <- which(spp==target[x])
  ix.sp.c <- c(ix.sp.c, dummy)
}

target <- c("Aepu", "Posa", "Lase") #ojo: el orden segun contraste de colores
ix.sp.d <- NULL
for(x in 1:length(target)){
  dummy <- which(spp==target[x])
  ix.sp.d <- c(ix.sp.d, dummy)
}

target <- c("Luap","Saco","Nopu") #ojo: el orden segun contraste de colores
ix.sp.a <- NULL
for(x in 1:length(target)){
  dummy <- which(spp==target[x])
  ix.sp.a <- c(ix.sp.a, dummy)
}


# experimento de simulacion 1
y_lim <- c(0, 200)

plots <- c("costa","depr","andes")

# tiff(filename="C:/Users/pepe/Downloads/test.tiff",width=2000, height=2000, units="px", compression = c("none"), res=300)
par(mfrow=c(3,2), mar=c(3,2,4,1), oma=c(2,2,0,0))
for(p in plots){
  
if (p == "costa") ix.sp <- ix.sp.c
if (p == "depr") ix.sp <- ix.sp.d
if (p == "andes") ix.sp <- ix.sp.a
  
plot(simu1[,ix.sp[1]], type="l", ylim=y_lim, xlab="",
     xaxt="n", yaxt="n", cex.lab=0.8, col="white")
for(i in 1:length(ix.sp)){ #stacked curves by target species
x2 <- c(1, seq(1:27), 27)
y2 <- c(0, simu1[,ix.sp[i]], 0)
polygon(x2,y2, col=adjustcolor(bgfill[ix.sp[i]],alpha.f=0.7), border=bgfill[ix.sp[i]])
}
segments(6.5,0,6.5,200, lty=2, col="darkgray")
segments(9.5,0,9.5,200, lty=2, col="darkgray")
segments(24.5,0,24.5,200, lty=2, col="darkgray")
axis(side=1, at=seq(1,27,by=4), labels=long[seq(1,27,by=4)],cex.axis=0.8, las=2, cex.axis=0.8) #x-axis ticks
axis(side=2, at=c(seq(0,200, by=25)), labels=seq(0, 200, by=25), line=0, las=2, cex.axis=0.8) #y-axis ticks
mtext(side=2, text="Area basal [m2/ha]", outer=F, font=1, line=2.5, cex=0.8) #y label
mtext(side=1, text="Longitud (°O)", outer=F, font=1, line=3.5, cex=0.8) #x label
if (p=="costa") mtext(side=3, text="Param. bioclimaticos: umbral Mediana", outer=F, font=1, line=0.5, cex=0.7) #main label
# legend("topright", c("Precipitaci?n anual CR2Met", "Precipitaci?n anual ERA5-Land", "Altitud", "Estaciones meteorol?gicas"), pch = c(NA, NA,15,24), col= c("blue", "darkblue", "darkgrey", "brown"), lty=c(1,2,NA,NA), inset=c(0,0), cex=0.9, pt.bg = c(NA,NA,NA,"darkorange"))
legend("topright", spp.names[sort(ix.sp)], fill=bgfill[sort(ix.sp)], cex=0.7, ncol=1)#, inset=c(0.01,0))

# experimento de simulacion 2
plot(simu2[,ix.sp[1]], type="l",ylab="", xlab="", ylim=c(0,200),
     xaxt="n", yaxt="n", cex.lab=0.8, col="white")
for(i in 1:length(ix.sp)){ #stacked curves by target species
  x2 <- c(1, seq(1:27), 27)
  y2 <- c(0, simu2[,ix.sp[i]], 0)
  polygon(x2,y2, col=adjustcolor(bgfill[ix.sp[i]],alpha.f=0.7), border=bgfill[ix.sp[i]])
}
segments(6.5,0,6.5,200, lty=2, col="darkgray")
segments(9.5,0,9.5,200, lty=2, col="darkgray")
segments(24.5,0,24.5,200, lty=2, col="darkgray")
axis(side=1, at=seq(1,27,by=4), labels=long[seq(1,27,by=4)],cex.axis=0.8, las=2, cex.axis=0.8) #x-axis ticks
axis(side=2, at=c(seq(0,200, by=25)), labels=seq(0, 200, by=25), line=0, las=2, cex.axis=0.8) #y-axis ticks
if (p=="costa") mtext(side=3, text="Param. bioclimaticos: Chile LHS v12", outer=F, font=1, line=0.5, cex=0.7) #main label
mtext(side=1, text="Longitud (°O)", outer=F, font=1, line=3.5, cex=0.8) #x label
# legend("topright", c("Precipitaci?n anual CR2Met", "Precipitaci?n anual ERA5-Land", "Altitud", "Estaciones meteorol?gicas"), pch = c(NA, NA,15,24), col= c("blue", "darkblue", "darkgrey", "brown"), lty=c(1,2,NA,NA), inset=c(0,0), cex=0.9, pt.bg = c(NA,NA,NA,"darkorange"))
legend("topright", spp.names[sort(ix.sp)], fill=bgfill[sort(ix.sp)], cex=0.7, ncol=1)#, inset=c(0.01,0))

}

dev.off()




















axis(side=1, at=c(0, 6.5, 9.5, 24.5, 28), line=4, labels=FALSE)
mtext(side=1, "Costa", line=4.3, at=3.5)
mtext(side=1, "Depresi?n", line=4.3, at=8)
mtext(side=1, "Intermedia", line=5.3, at=8)
mtext(side=1, "Andes", line=4.3, at=17)
mtext(side=1, "Estepa", line=4.3, at=26)
