library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(ggplot2)
library(readxl)
library(plotly)

## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]
u_age <- sort(as.numeric(unique(id_sex_age$age)))
u_IDs <- id_sex_age$ID
u_sex <- c("F","M")
u_region<-c(paste("Left_Region",1:74), paste("Right_Region",1:74))

fluidPage(
  titlePanel("ggseg3d"),
  
  fluidRow(
    column(3,
           selectizeInput(inputId = "sex",
                          label = "waehlen Geschlecht:",
                          choices = c("All"="",u_sex))
    ),
    column(3,
           selectInput(inputId = "age",
                       label = "waehlen Alt:",
                       choices = c("All"="",u_age))
    ),
    column(3,
           selectInput(inputId = "id",
                       label = "waehlen ID",
                       choices = c("All"="",u_IDs))
    ),
    # column(3,
    #        selectInput(inputId = "region",
    #                    label = "waehlen Region",
    #                    choices = c("All"="",u_region))
    # ),
    column(3, 
           tags$button("Restart", id="restart", type="button", class="btn btn-danger action-button", onclick="history.go(0)"))
    
  ),
  
  fluidRow(
    column(2,checkboxInput("single_region","single region",FALSE)),
    
    column(4,
           conditionalPanel(
             condition = "input.single_region==1",
             selectInput(inputId = "region",
                         label = "waehlen Region",
                         choices = c("All"="",u_region))
           ))
    
  ),
  
  DT::dataTableOutput("table"),
  actionButton("ab","3d brain zeigen"),
  plotlyOutput("ggseg3d")
  
  
)
