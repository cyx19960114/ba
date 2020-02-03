library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)

## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]
u_age <- sort(as.numeric(unique(id_sex_age$age)))
u_IDs <- id_sex_age$ID
u_sex <- c("F","M")

shinyUI(fluidPage(
  headerPanel("ggseg3d"),
  sidebarPanel(
    selectizeInput(inputId = "sex",
                label = "waehlen Geschlecht:",
                choices = c("sex"="",u_sex)),
    selectInput(inputId = "age",
                label = "waehlen Alt:",
                choices = c("age"="",u_age)),
    selectInput(inputId = "id",
                label = "waehlen ID",
                choices = c("id"="",u_IDs)
                ),
    ),
  
  
  
  mainPanel(
    # plotlyOutput("ggseg3d"),
    # plotlyOutput("ggseg3d1")
    
  )
))