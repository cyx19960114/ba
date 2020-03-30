library(shiny)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(ggplot2)
library(readxl)
library(plotly)
library(colourpicker)
library(processx)
library(shinydashboard)
## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]
u_age <- sort(as.numeric(unique(id_sex_age$age)))
u_IDs <- id_sex_age$ID
u_sex <- c("F","M")
u_region<-c(paste("L_Region",1:74), paste("R_Region",1:74))
u_hemisphere<-c("left","right")
u_format<-c("svg","pdf","png")

dashboardPage(
  dashboardHeader(title="ggseg3d"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data",NULL,icon = icon("file"),
               fileInput("data_table","TableInput",accept = c("xlsx","xls"))),
      
      menuItem("Atlas",NULL,icon=icon("file-alt"),
               fileInput("name_file","Thinkness Names correction",accept = c("xlsx","xls"))),
      menuItem("Quality Control",NULL,icon=icon("chart-bar"),
               selectInput("fil",label = "Filter",choices = names(OASIS),multiple = TRUE),
               actionButton("dp","Distribution Plot")),
      
      menuItem("Descriptive Statistics",NULL,icon=icon("brain"),
               selectInput("fil2",label = "Filter",choices = names(OASIS),multiple = TRUE),
               actionButton("ab","Brain Map")
               ),
      menuItem("Statistics",NULL)
    )
  ),  
  dashboardBody()
)

