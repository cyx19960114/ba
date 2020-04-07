oa <- read_excel('OASIS.xlsx')
oa[-1:-2] <- lapply(oa[-1:-2],as.numeric)
desterieux_neu<-desterieux_3d
oa1 <- oa[-1:-3][1,]
oa1 <- t(oa1)
oaaa <- data.frame(
  area = as.character(row.names(oa1)),
  wert = oa1[,1],
  stringsAsFactors = FALSE
)
# View(oaaa)

t_name <- names(oa[-1:-3])
for (j in 1:6) {
  if (desterieux_neu[[3]][[j]] == "left") {
    for (i in 1:82) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+1)/2)]
    }
    for (i in 84:149) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc(i/2)]
    }
  }else{
    for (i in 1:82) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+149)/2)]
    }
    for (i in 84:149) {
      desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+148)/2)]
    }
  }
}


ggseg3d(.data=oaaa,atlas=desterieux_neu,hemisphere = c('left','right'))


# rd <- load("desterieux_3d.rda")
