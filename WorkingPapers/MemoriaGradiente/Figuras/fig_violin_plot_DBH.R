##### ------------------------------------ Grafico de violin basico
rm(list=ls())

col.f <- function(x.df)            
{                              
  tmpred <- x.df
  x <- colnames(tmpred)
  color <- rep("white", ncol(tmpred))  
  color [(x == spp.names[1])] <-  "#c164a6" #AEPU
  #color [(x == spp.names[2])] <-  "darkolivegreen" #AMLU
  color [(x == spp.names[2])] <- "darkred" #AMME
  color [(x == spp.names[3])] <- "purple" #AUCH
  color [(x == spp.names[4])] <- "#d9ff73" #CAPA
  color [(x == spp.names[5])] <- "#dfffdf" #DADI
  color [(x == spp.names[6])] <-  "#FF7F00" #DRWI
  color [(x == spp.names[7])] <-  "blue" #EMCO
  color [(x == spp.names[8])] <-  "#A6CEE3" #EUCO
  color [(x == spp.names[9])] <-  "antiquewhite2" #FICU - Piso veg P77
  #color [(x == spp.names[11])] <- "#610093" #GEAV
  color [(x == spp.names[10])] <-  "springgreen"#LAPH - Piso veg P72
  color [(x == spp.names[11])] <-  "#7c7c7c" #LASE
  color [(x == spp.names[12])] <-  "#704A00" #LOFE
  #color [(x == spp.names[15])] <- "orangered" #LOHI
  color [(x == spp.names[13])] <- "#E31A1C" #LUAP
  #color [(x == spp.names[17])] <- "#CAB2D6" #MYOV
  #color [(x == spp.names[18])] <- "#B2DF8A" #MYPL
  color [(x == spp.names[14])] <- "#1F78B4" #NOAN
  color [(x == spp.names[15])] <- "royalblue" #NOBE
  color [(x == spp.names[16])] <- "royalblue4" #NODO - Piso veg P73
  #color [(x == spp.names[22])] <- "orange3" #NONE-NOAL
  #color [(x == spp.names[23])] <- "yellow" #NONI
  color [(x == spp.names[17])] <- "#006767" #NOOB - Piso veg P54
  color [(x == spp.names[18])] <- "#B15928" #NOPU - Piso veg P62
  color [(x == spp.names[19])] <- "#FDBF6F" #PELI
  #color [(x == spp.names[25])] <- "pink" #PIUV
  color [(x == spp.names[20])] <- "#FFFF99" #PONU
  color [(x == spp.names[21])] <- "grey" #POSA
  color [(x == spp.names[22])] <- "yellowgreen" #SACO
  #color [(x == spp.names[28])] <- "deeppink2" #TEST
  color [(x == spp.names[23])] <- "#009155" #WETR - Piso veg P82
  #color [(x == spp.names[33])] <- "white" #Others
  color
}



# Library
library(ggplot2)

# Most basic violin chart
# p <- ggplot(data, aes(x=Species, y=dInc, fill = Species)) + # fill=name allow to automatically dedicate a color for each group
#   geom_violin()
# 
# # Girar el grafico
# 
# p + coord_flip()
# 
# # Elegir items para graficar en el eje x
# 
# p + scale_x_discrete(limits=c("N. pumilio"))
# 
# # Use custom color palettes
# #p+scale_color_manual(values=bgfill)
# 
# # Use custom color palettes
# 
# p+scale_fill_manual(values=bgfill)
# 
# 
# # Poner fondo blanco : theme_minimal()
# p + theme_minimal()
# 
# 
# 

##### ------------------------------------ Grafico de violin horizontal

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
# library(viridis)



######## --------------------- Mi data de DAP incremento


mypath <- 'G:/Mi unidad/MODELADORES_FORCLIM/Trabajo_JavierOrtega/Parametrizacion/kG/'

data = read.csv(file.path(mypath, "MAX_DBH_CM_GROWTH_DATA_SPP_COMBINED_AGUTIERREZ_SADA_ITRDB.csv"), header = TRUE, sep= ",")
data <- data[,-(31)]

dataList <- data
sppList  = colnames(dataList)
Nspp     = length(sppList)
Ncores_spp = as.data.frame( matrix(NA, nrow = Nspp, ncol=6) );
for (ii in 1:Nspp){
  v = unlist(dataList[ii])
  vix = !is.na(v)
  v   = v[vix]
  Ncores_spp[ii,1] = sppList[ii];
  Ncores_spp[ii,2] = length(v);
  Ncores_spp[ii,3] = max(na.omit(data[,ii]))
  Ncores_spp[ii,4] = quantile(dataList[,ii], probs=c(0.9), na.rm =TRUE);
  Ncores_spp[ii,5] = quantile(dataList[,ii], probs=c(0.95), na.rm =TRUE);
  Ncores_spp[ii,6] = quantile(dataList[,ii], probs=c(0.99), na.rm =TRUE)
}

names(Ncores_spp) <- c("Species", "Nspp", "MaxDBHinc", "P-90", "P-95", "P-99")
Ncores_spp <- cbind(Ncores_spp, N = seq(1:length(Ncores_spp[,1])))


ix <- c(1,3,5,6,7,8,9,10,11,12,13,14,15,16,18,21,22,23,25,26,27,28,29)
spp <- sppList[ix]



spp.names <- c("A. punctatum", "A. meli", "A. chilensis", "C. paniculata", "D. diacanthoides", "D. winteri", 
               "E. coccineum", "E. cordifolia", "F. cupressoides", "L. philippiana", "L. sempervirens", 
               "L. ferruginea", "L. apiculata", "N. antarctica", "N. betuloides", "N. dombeyi", "N. obliqua",
               "N. pumilio","P. lingue","P. nubigena","P. saligna", "S. conspicua", "W. trichosperma")

dataList <- dataList[,ix]                
dataList <- dataList[,order(spp)]

Ncores_spp <- Ncores_spp[ix,]
Ncores_spp <- Ncores_spp[order(Ncores_spp$Species),]
Ncores_spp <- cbind(Ncores_spp, Spp.names = spp.names)
#Ncores_spp <- Ncores_spp[order(Ncores_spp$Species),]
#Ncores_spp <- Ncores_spp[c(3,1,5,6,8,9,10,11,14,15,16,17,18,19,20,21,22,23),]



#puntos <- as.data.frame(cbind(Ncores_spp$Species, Ncores_spp$`P-95`))

  
names(dataList) <- spp.names

bgfill <- col.f(dataList)

data <- NULL
for (i in 1:length(spp)){
  if (i == 18){
    dataList[which(dataList$`N. pumilio` > 3)[1],i] <- NA
    dataList[which(dataList$`N. pumilio` > 3)[1],i] <- NA
  }
  dummy <- na.omit(dataList[,i])
  spp.dap <- cbind(Species=spp.names[i], dInc=dummy, bgfill[i], Ncores_spp$`P-95`[i])
  data <- rbind(data, spp.dap)
}

data <- as.data.frame(data)
data$dInc <- as.numeric(as.character(data$dInc))

ix <- order(unique(data$Species))

del <- 

# tiff(filename="C:/Users/pepe/Downloads/PPR_GraficoViolin.tiff",width=1209, height=1814, units="px", compression = c("none"), res=300)
p <- ggplot() + # fill=name allow to automatically dedicate a color for each group
  geom_violin(data=data, aes(y=factor(Species, level = rev(spp.names)), 
                             x=dInc, fill = V3, color=V3), scale="width", show.legend = F)+
  scale_fill_manual(values=rep("darkgray", 17), name="Especies") +
  scale_color_manual(values=rep("darkgray", 17), name="Especies") +
  labs(title = "", x= "Incremento diamétrico anual (cm)", y = "Especies") +
  geom_boxplot(data=data, aes(y=Species, x=dInc), width=0.2, fill="white", color="black", outlier.size = 0.5, na.rm=TRUE) +
  theme(axis.text.y = element_text(face = "italic", family="serif"),
        axis.title.x = element_text(family="serif"),
        axis.title.y = element_text(family="serif"),
        legend.title = element_text(family="serif"),
        legend.text = element_text(face = "italic", family="serif"),
        text = element_text(family = "serif"),
        panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour="lightgray"),
        axis.ticks = element_line(colour="white"))
p
# dev.off()
