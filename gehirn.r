setwd("C:/Users/cyx19/Desktop/Rcode")

library(ggseg)
library(ggseg3d)
library(ggsegExtra)
library(dplyr)
library(tidyr)
library(readxl)

Data <- read_excel("someData.xlsx")

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

ggseg3d(.data = someData,
        atlas = desterieux_3d,
        colour = "wert", text = "wert",
        palette = c("red" = 0, "yellow" = 0.5, "blue" = 1),#定死上下限，下限红=0，上限蓝=1，可以更改
        hemisphere = "left",
        na.alpha= .5) %>%
  add_glassbrain("right") %>%
  pan_camera("left lateral") %>%
       remove_axes()
