
setwd("C:/Users/cyx19/Desktop/ba")

library(ggseg)
library(ggseg3d)
library(ggsegExtra)
library(dplyr)
library(tidyr)
library(readxl)

Data <- read_excel("someData.xlsx", col_types = c("text"))

Data2 <- data.frame(
  area = paste("Region",1:74)
)



desterieux_neu<-desterieux_3d


## mark the names of brain region



## make the names pass to left and right brain
for (j in 1:6) {
  if (desterieux_neu[[3]][[j]] == "left") {
    for (i in 1:82) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Left_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]]))
    }
    for (i in 84:149) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Left_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1)
    }
  }else{
    for (i in 1:82) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Right_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]]))
    }
    for (i in 84:149) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Right_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1)
    }
  }
}


## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
region_names <- names(OASIS)[-1:-3]
names(OASIS)[4:77] <-paste("Left_Region",1:74) 
names(OASIS)[78:151] <-paste("Right_Region",1:74)
OASIS_Thinkness <- OASIS[-1:-3]
a1 <- t(OASIS_Thinkness[1,])

example1Data = data.frame(
  area = row.names(a1),
  wert = as.numeric(a1[,1]),
  strings_As_Factors = FALSE
)
example1Data$beschreibung <- paste("Region Names: ",region_names,", Wert ist ",example1Data$wert)

example1Data$area<-as.character(example1Data$area)

## test
someData = data.frame(
  area = Data2,
  wert = sample(seq(0,1,.01), 74),
  strings_As_Factors = FALSE)

someData$beschreibung <- paste("wert ist ", someData$wert,sep = ' ')

Herr = data.frame(
  area = Data,
  wert = sample(seq(0,1,.001), 74),
  strings_As_Factors = FALSE)

Frau = data.frame(
  area = Data,
  wert = sample(seq(0,1,.001), 74),
  testwert = sample(seq(0,30,.01),74),
  strings_As_Factors = FALSE)


###########################
###########################

ggseg3d(.data = example1Data,
        atlas = desterieux_neu,
        colour = "wert", text = "beschreibung",
        surface = "LCBC",
        palette = c("red" = 1, "yellow" = 2, "blue" = 3),
        hemisphere = c("left","right"),
        na.alpha= .5) %>%
  pan_camera("left lateral") %>%
       remove_axes()
