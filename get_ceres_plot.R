library(ggplot2)
library(gridExtra)
source("geom_flat_violin.R")


get.ceres.plot <- function(data){
  
  
  cols <- ncol(data)
  data <- data[(cols-273):cols]
  # data <- data[1,]
  name_level <- names(data)
  
  
  
  area1 <- data[,1]
  area2 <- data[,2:92]
  area3 <- data[,93:183]
  area4 <- data[,184:274]
  
  name_level.area1 <- names(area1)
  name_level.area2 <- names(area2)
  name_level.area3 <- names(area3)
  name_level.area4 <- names(area4)
  
  
  ### get the facet name for four areas
  facet.area1 <- function(x){
    return('ICV')
  }
  
  facet.area2 <- function(x){
    if(grepl("*left*",x)&&grepl("*perc*",x)){
      return("perc_left")
    }else if(grepl("*right*",x)&&grepl("*perc*",x)){
      return("perc_right")
    }else if(grepl("*left*",x)){
      return("left")
    }else if(grepl("*right*",x)){
      return("right")
    }else if(grepl("total",x)&&grepl("*perc*",x)){
      return("perc_total")
    }else if(grepl("total",x)){
      return("total")
    }else{
      return("asymmetry")
    }
  }
  
  
  
  
  
  facet.area3 <- function(x){
    if(grepl("*left*",x)&&grepl("*norm*",x)){
      return("norm_left")
    }else if(grepl("*right*",x)&&grepl("*norm*",x)){
      return("norm_right")
    }else if(grepl("*left*",x)){
      return("mean_left")
    }else if(grepl("*right*",x)){
      return("mean_right")
    }else if(grepl("norm",x)){
      return("norm")
    }else if(grepl("mean",x)){
      return("mean")
    }else if(grepl("asymmetry",x)){
      return("asymmetry")
    }else{
      return("mean")
    }
  }
  
  facet.area4 <- function(x){
    if(grepl("perc",x)&&grepl("left",x)){
      return("grey_perc_left")
    }else if (grepl("perc",x)&&grepl("right",x)){
      return("grey_perc_right")
    }else if(grepl("perc",x)){
      return("grey_perc")
    }else if(grepl("left",x)){
      return("grey_left")
    }else if(grepl("right",x)){
      return("grey_right")
    }else if(grepl("asymmetry",x)){
      return("grey_asymmetry")
    }else if (grepl("grey",x)){
      return("grey")
    }
  }
  
  area1 <- melt(area1)
  area2 <- melt(area2)
  area3 <- melt(area3)
  area4 <- melt(area4)
  
  names(area1) <- c("area","thickness")
  names(area2) <- c("area","thickness")
  names(area3) <- c("area","thickness")
  names(area4) <- c("area","thickness")
  
  area1$lr <- lapply(area1$area,facet.area1)
  area2$lr <- lapply(area2$area,facet.area2)
  area3$lr <- lapply(area3$area,facet.area3)
  area4$lr <- lapply(area4$area,facet.area4)
  
  
  
  
  ## fix the facet name
  get_area_name <- function(x){
    x <- gsub("_","",x)
    x <- sub("right","",x)
    x <- sub("left","",x)
    x <- sub("total","",x)
    x <- sub("asymmetry","",x)
    x <- sub("perc","",x)
    x <- sub("cm3","",x)
    x <- gsub("\\.","",x)
    x <- sub("norm","",x)
    x <- sub("mean","",x)
  }
  
  area2$area <- lapply(area2$area, get_area_name)
  area3$area <- lapply(area3$area, get_area_name)
  area4$area <- lapply(area4$area, get_area_name)
  
  
  get_area_factor <- function(x){
    x <- as.character(x)
    name_level <- rev(unique(x))
    factor(x,levels = name_level,ordered = TRUE)
  }
  
  area1$area <- get_area_factor(area1$area)
  area2$area <- get_area_factor(area2$area)
  area3$area <- get_area_factor(area3$area)
  area4$area <- get_area_factor(area4$area)
  
  
  get_lr_factor <- function(x){
    x <- as.character(x)
    name_level <- unique(x)
    factor(x,levels = name_level,ordered = TRUE)
  }
  
  area1$lr <- get_lr_factor(area1$lr)
  area2$lr <- get_lr_factor(area2$lr)
  area3$lr <- get_lr_factor(area3$lr)
  area4$lr <- get_lr_factor(area4$lr)
  
  
  get.plot <- function(area,area.label=NULL){
    t <- switch (area.label,
                 "area1" = "ICV",           
                 "area2" = "thickness",
                 "area3" = "cortical_thickness",
                 "area4" = "grey_matter",
    )
    
    
    
    
    p <- ggplot(area,aes(x=area,y=thickness,fill=area))+
      geom_flat_violin(position=position_nudge(x=0.2,y=0),adjust=1,trim = TRUE)+
      geom_point(position = position_jitter(width=.1),size=.2,aes(color=area),show.legend = FALSE)+
      geom_boxplot(aes(x=as.numeric(area)+0.2,y=thickness),outlier.shape = NA,alpha=0.3,width=0.1,color="BLACK")+
      coord_flip()+
      facet_wrap(~lr,scales = "free_x",ncol = 7)+
      theme_cowplot()+
      ggtitle(t)+
      guides(fill=FALSE)
    
    return(p)
  }
  
  
  
  p1 <- get.plot(area1,"area1")+theme(aspect.ratio = 1/7)
  p2 <- get.plot(area2,"area2")
  p3 <- get.plot(area3,"area3")
  p4 <- get.plot(area4,"area4")
  
  plots <- grid.arrange(p1,p2,p3,p4,ncol=1)
  return(plots)
}

# data <- read_excel("Cerebellum_CamCAN_R.xlsx")
# 
# c <- get.ceres.plot(data)
# c