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
      menuItem("Data",tabName = "Data",icon = icon("file"),
               fileInput("data_table","TableInput",accept = c("xlsx","xls"))),
      
      
      menuItem("Atlas",tabName = "Atlas",icon=icon("file-alt"),
               condition="input.sideM==='Atlas'",
               fileInput("name_file","Thinkness Names correction",accept = c("xlsx","xls"))
               
      ),
      
      
      
      menuItem("Quality Control",tabName = "qc",icon=icon("chart-bar"),expandedName = "qc",
               selectInput("qc_fil",label = "Filter",choices = names(OASIS)[-1],multiple = TRUE),
               uiOutput("qc_kon"),
               actionButton("dp","Distribution Plot")
      ),
      
      
      
      menuItem("Descriptive Statistics",expandedName = "ds",icon=icon("brain"),
               conditionalPanel(
                 condition = "input.com==0",
                 selectInput("fil",label = "Filter",
                             choices = names(OASIS),multiple = TRUE),
               ),
               conditionalPanel(
                 condition = "input.com==1",
                 selectInput("fil_com",label = "Filter",
                             choices = names(OASIS)[-1],multiple = TRUE),
               ),
               
               uiOutput("ds_kon"),
               
               conditionalPanel(
                 condition = "input.com==1",
                 radioButtons("com_way_c",label = "Central tendency",
                              choices = c("median","mean"),selected="median",inline = TRUE),
               ),
               
               conditionalPanel(
                 condition = "input.com==1",
                 radioButtons("com_way_d",label = "Dispersion",
                              choices = c("SD","SEM"),selected=character(0),inline = TRUE),
                 
               ),
               
               
               
               
               ###调整调用的函数
               
               radioButtons(inputId = "select_hemisphere",
                            label = "Choose Hemisphere",
                            choices = c(u_hemisphere,"both"),
                            selected = "both",inline = TRUE
               ),
               
               # radioButtons(inputId=)
               
               
               
               checkboxInput("com","composity",value = TRUE),
               actionButton("ab","Brain Map")
      ),
      menuItem("Statistics"),
      
      tags$button("Restart", 
                  id="restart", 
                  type="button", class="btn btn-danger action-button", onclick="history.go(0)"),
      id="sideM"
      
    )
  ),  
  
  
  dashboardBody(
    conditionalPanel(
      condition="input.sidebarItemExpanded=='qc'",
      tabsetPanel(type="tabs",id="qc_tab",
                  tabPanel("Table",DT::dataTableOutput("qc_table")),
                  tabPanel("Quality Raincloud",plotOutput("quality",height = "7500px",width = "1300px"))
                  
      )
      
    ),
    
    
    conditionalPanel(
      condition="input.sidebarItemExpanded=='ds'",
      tabsetPanel(
        type="tabs",id="ds_tab",
        tabPanel("Table",DT::dataTableOutput("ds_table")),
        tabPanel("3D",plotlyOutput("ggseg3d",height = "700px"))
      )
      
    )
  )
)
