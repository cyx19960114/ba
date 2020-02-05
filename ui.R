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
  checkboxInput("com","composite display",FALSE),
  fluidRow(
    column(3,
           selectizeInput(inputId = "sex",
                          label = "waehlen Geschlecht:",
                          choices = c("All"="",u_sex))
    ),
    
    ## single person auswahl
    #################################
    
    conditionalPanel(
      condition = "input.com==0",
      column(3,
             selectizeInput(inputId = "age",
                            label = "waehlen Alt:",
                            choices = c("All"="",u_age))
      ),
      column(3,
             selectInput(inputId = "id",
                         label = "waehlen ID",
                         choices = c("All"="",u_IDs))
      )
    ),
    
    #############################
    ##composite display
    #############################
    conditionalPanel(
      condition = "input.com==1",
      column(5,
             sliderInput("age_range","Age Range",
                         min = min(u_age),max=max(u_age),
                         value = c(min(u_age),max(u_age)))
      )
    )
    
    
  ),
  fluidRow(
    ## single person auswahl
    #################################
    conditionalPanel(
      condition = "input.com==0",
      column(2,checkboxInput("single_region","single region",FALSE)),
      column(4,
             conditionalPanel(
               condition = "input.single_region==1",
               selectInput(inputId = "region",
                           label = "waehlen Region",
                           choices = u_region)
             ))
    )
    
    #############################
    ##composite display
    #############################
    
    
  ),
  fluidRow(
    column(3, 
           tags$button("Restart", id="restart", type="button", class="btn btn-danger action-button", onclick="history.go(0)")
    )
  ),
  
  DT::dataTableOutput("table"),
  actionButton("ab","3d brain zeigen"),
  plotlyOutput("ggseg3d"),
  plotOutput("distributionPlot")
  
  
)
