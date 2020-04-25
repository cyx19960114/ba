library(shiny)
library(dplyr)
library(tidyr)
# library(ggseg3d)
# library(ggsegExtra)
library(ggplot2)
library(readxl)
library(plotly)
library(colourpicker)
library(processx)
library(Rmisc)
library(plyr)
library(shinydashboard)
library(glmnet)
library(gplots)


dashboardPage(
  
  dashboardHeader(title="ggseg3d"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Data",tabName = "Data",icon = icon("file"),
               fileInput("data_table","TableInput",accept = c("xlsx","xls"))),
      
      
      menuItem("Atlas",tabName = "Atlas",icon=icon("file-alt"),
               conditionalPanel(
                 condition="output.dataFileLoad==true",
                 fileInput("name_file","Thinkness Names correction",accept = c("xlsx","xls"))
               )
      ),
      
      menuItem("Quality Control",tabName = "qc",icon=icon("chart-bar"),expandedName = "qc",
               conditionalPanel(
                 condition="output.dataFileLoad==true",
                 uiOutput("fil_qc"),
                 uiOutput("qc_kon"),
                 actionButton("dp","Distribution Plot")
               )
      ),
      
      
      
      menuItem("Descriptive Statistics",expandedName = "ds",icon=icon("brain"),
               conditionalPanel(
                 condition="output.dataFileLoad==true",
                 uiOutput("fil_ui"),
                 uiOutput("ds_kon"),
                 
                 conditionalPanel(
                   condition = "input.com==1",
                   uiOutput("com_cd")
                 ),
                 
                 
                 radioButtons(inputId = "select_hemisphere",
                              label = "Choose Hemisphere",
                              choices = c("left","right","both"),
                              selected = "both",inline = TRUE
                 ),
                 
                 checkboxInput("col","Color and Value",FALSE),
                 
                 conditionalPanel(condition="input.col==1",
                                  colourInput("color_obergrenze", "Please select the color of the upper bound", "red"),
                                  numericInput(inputId = 'wert_obergrenze',
                                               label = 'Please choose a value for the upper bound',
                                               4.2),
                                  
                                  colourInput("color_untergrenze", "Please select the color of the lower bound", "blue"),
                                  numericInput(inputId = 'wert_untergrenze',
                                               label = 'Please choose a value for the lower bound',
                                               1.5),
                                  
                                  
                                  
                                  actionButton("add_mitte", "Add new values and colors"),
                                  hr(),
                                  actionButton("remove_mitte","Remove"),
                                  hr()
                 ),
                 checkboxInput("com","composity",value = TRUE),
                 checkboxInput("check_download","Download",FALSE),
                 
                 actionButton("ab","Brain Map"),
                 conditionalPanel(
                   condition = "input.check_download==1",
                   textInput(inputId = "name",
                             "file name"
                   ),
                   
                   selectInput(inputId = "format",
                               "file format",
                               choices = c("svg","pdf","png"),
                               "pdf"
                   ),
                   div(style="height:100px;",
                       checkboxInput("down_filter","With Filer Label",value = FALSE)
                       
                   ),
                   actionButton("download","Download image"),
                   hr()
                 )
               )
      ),
      menuItem("Linear Regression",expandedName = "ss",icon=icon("chart-line"),
               conditionalPanel(
                 condition="output.dataFileLoad==true",
                 uiOutput("fil_ss"),
                 uiOutput("ss_kon"),
                 actionButton("rp","Regression Plots")
               )
               
               ),
      
      menuItem("Lasso Regression",expandedName = "ls",icon=icon("chart-line"),
               conditionalPanel(
                 condition="output.dataFileLoad==true",
                 uiOutput("fil_ls"),
                 uiOutput("ls_kon"),
                 actionButton("lp","TableLasso generation")
               )
               
      ),
      
      tags$button("Restart", 
                  id="restart", 
                  type="button", class="btn btn-danger action-button", onclick="history.go(0)"),
      id="sideM"
      
    )
    
  ),  
  
  
  dashboardBody(
    conditionalPanel(
      condition="output.dataFileLoad==true && input.sidebarItemExpanded=='qc'",
      tabsetPanel(type="tabs",id="qc_tab",
                  tabPanel("Table",DT::dataTableOutput("qc_table")),
                  tabPanel("Quality Raincloud",plotOutput("quality",height = "7500px",width = "1300px"))
                  
      )
      
    ),
    
    
    conditionalPanel(
      condition="output.dataFileLoad==true && input.sidebarItemExpanded=='ds'",
      tabsetPanel(
        type="tabs",id="ds_tab",
        tabPanel("Table",DT::dataTableOutput("ds_table")),
        tabPanel("Statistics",DT::dataTableOutput("ds_composity")),
        tabPanel("3D",plotlyOutput("ggseg3d",height = "700px"))
      ),
    ),
    
    conditionalPanel(
      condition="output.dataFileLoad==true && input.sidebarItemExpanded=='ss'",
      tabsetPanel(
        type="tabs",id="ss_tab",
        tabPanel("Table",DT::dataTableOutput("ss_table")),
        tabPanel("Regression Plots",plotOutput("regression",height ="40000px",width = "1000px"))
      ),
    ),
    
    conditionalPanel(
      condition="output.dataFileLoad==true && input.sidebarItemExpanded=='ls'",
      tabsetPanel(
        type="tabs",id="ls_tab",
        tabPanel("Table",DT::dataTableOutput("ls_table")),
        tabPanel("Lasso tabel",DT::dataTableOutput("lasso_table"))
      ),
    )
  )
)
