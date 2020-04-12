library(tidyverse)
library(readxl)
library(reshape2)
library(Rmisc)
# data <- read_excel("OASIS_behavioral.xlsx")

add_lm_trace <- function(data,var_explan="reaction_time"){
  cols <- ncol(data)
  explan <- var_explan
  p <- list()
  for(i in seq(1:74)){
    data_thickness <- dplyr::select(data,(cols-148+i),(cols-74+i))
    data_thickness <- bind_cols(explan=data[[explan]],data_thickness)
    data_thickness <- melt(data_thickness,"explan")
    names(data_thickness) <- c(as.character(explan),"thickness","value")
    
    data_thickness$lr <- substr(data_thickness$thickness,1,1)
    data_thickness[which(data_thickness$lr=='l'|data_thickness$lr=="L"),]$lr <- "left"
    data_thickness[which(data_thickness$lr=='r'|data_thickness$lr=="R"),]$lr <- "right"
    data_thickness$lr <- as.factor(data_thickness$lr)
    data_thickness$thickness <- substring(data_thickness$thickness,4)
    data_thickness$thickness <- as.factor(data_thickness$thickness)
    p1 <- ggplot(data_thickness, aes_string(x=explan, y="value")) +
      geom_point() +
      facet_grid(thickness~lr,scales = "fixed")+
      stat_smooth(method=lm, level=0.95) +
      xlab(explan)+
      ylab("thickness (mm)")+
      theme_minimal()
    p <- c(p,list(p1))
  }
  
  return(multiplot(plotlist = p,cols = 1))
}



