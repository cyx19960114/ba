s<-dkt



someData = data.frame(
  area = c("superior temporal"),
  p = sample(seq(0,.5,.001), 4),
  strings_As_Factors = FALSE)


someData = data.frame(
  area = c("transverse temporal", "insula",
           "pre central","superior parietal"),
  p = sample(seq(0,.5,.001), 4),
  strings_As_Factors = FALSE)


x<-ggseg(.data=someData, mapping=aes(fill=p),atlas = "s") +
  labs(title="A nice plot title", fill="p-value") +
  scale_fill_gradient(low="firebrick",high="goldenrod")

ggseg(.data=someData, mapping=aes(fill=p),atlas = "dkt") +
  labs(title="A nice plot title", fill="p-value") +
  scale_fill_gradient(low="firebrick",high="goldenrod")

############################################

for (i in 1:80) {
  x<-s[[6]][[i]]
  x<-x[,-4]
  x<-x[,-4]
  x<-x[,-4]
  s[[6]][[i]]<-x
  
}

x<-s[[6]][[5]]
x$.id<-as.character(101)

gehirn[[6]][[1]]<-x


i=0

{
i<-i+1
dkt[[6]][[i]]
}

#########################################################

setwd("C:/Users/cyx19/Desktop/Rcode")

library(readxl)
library(png)
library(parallel)
library(reshape2)
library(dplyr)
library(tidyverse)
library(igraph)
library(networkD3)
options(digits = 5)
rgbBase<- read_excel("rgbBase.xlsx")
regionMax<-nrow(rgbBase)
x<-1:regionMax

funcionAchseUndRegion<-function(n){
  library(jpeg)
  library(png)
  library(readxl)
  library(parallel)
  library(reshape2)
  rgbBase<- read_excel("rgbBase.xlsx")#Farbe jedes Regions
  picture<-readPNG("Grafik.png")
  achseUndRegion<-data.frame()
  s<-1
  longImage <- melt(picture)
  rgbImage <- reshape(longImage, timevar = 'Var3', idvar = c('Var1','Var2'), direction = 'wide')
  rgb<-as.numeric(c(rgbBase[n,1],rgbBase[n,2],rgbBase[n,3]))
  row_max<-as.numeric(nrow(rgbImage))
  for (j in 1:row_max) {
    rrggbb<-c(rgbImage[j,3],rgbImage[j,4],rgbImage[j,5])
    if((rgb[1]-0.04<rrggbb[1])&&(rrggbb[1]<rgb[1]+0.04)&&(rgb[2]-0.04<rrggbb[2])&&(rrggbb[2]<rgb[2]+0.04)&&(rgb[3]-0.04<rrggbb[3])&&(rrggbb[3]<rgb[3]+0.04)){
      achseUndRegion[s,1]<-rgbImage[j,2]
      achseUndRegion[s,2]<-rgbImage[j,1]
      achseUndRegion[s,3]<-rgbBase[n,4]
      s<-s+1
    }
  }
  names(achseUndRegion)<-c(".long",".lat",".id")
  return(achseUndRegion)
}#Die Funktion markiert, welche Pixel zum 1-74-Region gehoeren.

cl<-makeCluster(4)
clusterExport(cl,"funcionAchseUndRegion")
results<-parLapply(cl,x,funcionAchseUndRegion)#Multithreading-Betrieb
achseUndRegion<-do.call('rbind',results)
stopCluster(cl)



for (i in 8:74) {
  results[[i]]$.long<-results[[i]]$.long/100
  results[[i]]$.lat<-results[[i]]$.lat/100
}

results[[7]]$.long<-results[[7]]$.long/100
results[[7]]$.lat<-results[[7]]$.lat/100



for (i in 1:74) {
   gehirn[[6]][[i]]<-results[[i]]  
}

someData = data.frame(
  area = c("S_temporal_sup"),
  p = sample(seq(0,.5,.001), 1),
  strings_As_Factors = FALSE)

ggseg(.data=someData, mapping=aes(fill=p),atlas = "gehirn") +
  labs(title="A nice plot title", fill="p-value") +
  scale_fill_gradient(low="firebrick",high="goldenrod")


##################################################################

library(ggseg)
library(ggseg3d)
library(ggsegExtra)
library(dplyr)
library(tidyr)

someData = data.frame(
  area = Data,
  wert = sample(seq(0,100,3), 74),
  strings_As_Factors = FALSE)

Herr = data.frame(
  area = Data,
  wert = sample(seq(0,1,.001), 74),
  strings_As_Factors = FALSE)

Frau = data.frame(
  area = Data,
  wert = sample(seq(0,1,.001), 74),
  strings_As_Factors = FALSE)


ggseg3d(.data = someData,
        atlas = desterieux_3d,
        colour = "wert", text = "wert",
        palette = c("#ff0000", "#00ff00", "#0000ff"),
        hemisphere = "left",
        na.alpha= .5) %>%
  add_glassbrain("right") %>%
  pan_camera("left lateral") %>%
       remove_axes()
