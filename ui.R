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
u_region<-c(paste("L_Region",1:74), paste("R_Region",1:74))

fluidPage(
  titlePanel("ggseg3d"),
  sidebarLayout(
    sidebarPanel(
      ## single person auswahl
      #################################
      conditionalPanel(
        condition = "input.com==0",
        selectizeInput(inputId = "sex",
                       label = "waehlen Geschlecht:",
                       choices = c("All"="",u_sex)
        ),
        selectizeInput(inputId = "age",
                       label = "waehlen Alt:",
                       choices = c("All"="",u_age)),
        selectInput(inputId = "id",
                    label = "waehlen ID",
                    choices = c("All"="",u_IDs))
      ),
      
      #############################
      ##composite display
      #############################
      conditionalPanel(
        condition = "input.com==1",
        selectizeInput(inputId = "com_sex",
                       label = "waehlen Geschlecht:",
                       choices = c("All",u_sex)
        ),
        sliderInput("age_range","Age Range",
                    min = min(u_age),max=max(u_age),
                    value = c(min(u_age),max(u_age))),
        radioButtons("com_way","median or mean",choices = c("median","mean"),
                     selected = "median",inline = TRUE)),
      
      
      
      
      ## single person auswahl
      #################################
      conditionalPanel(
        condition = "input.com==0",
        checkboxInput("single_region","single region",FALSE),
        
        conditionalPanel(
          condition = "input.single_region == 1",
          selectInput(inputId = "region",
                      label = "waehlen Region",
                      choices = u_region)
        )
      ),
      checkboxInput("com","composite display",FALSE),
      #############################
      ##composite display
      #############################
      
      tags$button("Restart", id="restart", type="button", class="btn btn-danger action-button", onclick="history.go(0)"),
      
      actionButton("ab","3d brain zeigen"),
      
      
      
    ),

      
      
      
      mainPanel(
        tabsetPanel(type = "tabs",id = "tab",
                    tabPanel("Table",DT::dataTableOutput("table")),
                    tabPanel("3D",plotlyOutput("ggseg3d")),
                    tabPanel("DistributionPlot",plotOutput("distributionPlot")))
      )

    
  )
)
