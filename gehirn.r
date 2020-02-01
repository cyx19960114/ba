setwd("C:/Users/cyx19/Desktop/ba")

library(ggseg)
library(ggseg3d)
library(ggsegExtra)
library(dplyr)
library(tidyr)
library(readxl)

Data <- read_excel("someData.xlsx", col_types = c("text"))

desterieux_neu<-desterieux_3d

for (j in 1:6) {
 for (i in 1:82) {
  desterieux_neu[[4]][[j]][[1]][[i]]<-as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])
}
for (i in 84:149) {
  desterieux_neu[[4]][[j]][[1]][[i]]<-as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1
} 
}



someData = data.frame(
  area = Data,
  wert = sample(seq(0,1,.01), 74),
  strings_As_Factors = FALSE)

Herr = data.frame(
  area = Data,
  wert = sample(seq(0,1,.001), 74),
  strings_As_Factors = FALSE)

Frau = data.frame(
  area = Data,
  wert = sample(seq(0,1,.001), 74),
  testwert = sample(seq(0,30,.01),74),
  strings_As_Factors = FALSE)

ggseg3d(.data = Frau,
        atlas = desterieux_neu,
        colour = "wert", text = "wert",#text 为附加内容并且要带上wert，area改为region1-74
        surface = "LCBC",
        palette = c("red" = 0, "yellow" = 0.5, "blue" = 1),#定死上下限，下限红=0，上限蓝=1，可以更改
        hemisphere = "left",
        na.alpha= .5) %>%
  add_glassbrain("right") %>%
  pan_camera("left lateral") %>%
       remove_axes()

