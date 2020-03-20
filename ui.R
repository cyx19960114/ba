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

## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]
u_age <- sort(as.numeric(unique(id_sex_age$age)))
u_IDs <- id_sex_age$ID
u_sex <- c("F","M")
u_region<-c(paste("L_Region",1:74), paste("R_Region",1:74))
u_hemisphere<-c("left","right")
u_format<-c("svg","pdf","png")

fluidPage(
  titlePanel("ggseg3d"),
  sidebarLayout(
    sidebarPanel(
      
      ######################################################
      #################single person auswahl################
      ######################################################
      
      conditionalPanel(
        condition = "input.com==0",
        selectInput("fil",label = "Filter",
                    choices = names(OASIS),multiple = TRUE),
      ),
      conditionalPanel(
        condition = "input.com==1",
        selectInput("fil_com",label = "Filtern",
                    choices = names(OASIS)[-1],multiple = TRUE),
      ),
      uiOutput("kon"),
      
      
      ######################################################
      #################single person auswahl################
      ######################################################
      conditionalPanel(
        condition = "input.com==0",
        checkboxInput("single_region","single region",FALSE),
        
        conditionalPanel(
          condition = "input.single_region == 1",
          selectInput(inputId = "region",
                      label = "Please select a single region",
                      choices = u_region)
        )
      ),
      
      
      
      ######################################################
      #######################Checkbox#######################
      ######################################################
      checkboxInput(inputId="com",label="composite display",value=FALSE),
      checkboxInput("farbe_wert","color and value",FALSE),
      checkboxInput("hemisphere","hemisphere",FALSE),
      checkboxInput("checkbox_downlaod","Download options",FALSE),
      
      
      
      ######################################################
      #####################Hemisphere#######################
      ######################################################
      conditionalPanel(
        condition = "input.hemisphere == 1",
        selectInput(inputId = "select_hemisphere",
                    label = "Choose Hemisphere",
                    choices = c("All",u_hemisphere),
                    "All"
        ),
      ),
      
      
      
      
      ######################################################
      ######Choose colors and values for 3d brain maps######
      ######################################################
      conditionalPanel(
        condition = "input.farbe_wert == 1",
        # selectInput(inputId = "color_obergrenze",
        #            label = "waehlen color_obergrenze",
        #           choices = u_color_obergrenze,
        #          "red"),
        colourInput("color_obergrenze", "Please select the color of the upper bound", "red"),
        numericInput(inputId = 'wert_obergrenze',
                     label = 'Please choose a value for the upper bound',
                     4.2),
        # selectInput(inputId = "color_untergrenze",
        #       label = "waehlen color_untergrenz",
        #     choices = u_color_untergrenze,
        #    "blue"),
        colourInput("color_untergrenze", "Please select the color of the lower bound", "blue"),
        numericInput(inputId = 'wert_untergrenze',
                     label = 'Please choose a value for the lower bound',
                     1.5),
        actionButton("add_mitte", "Add new values and colors"),
        hr(),
        actionButton("remove_mitte","Remove"),
        hr()
        
      ),
      
      
      conditionalPanel(
        condition = "input.checkbox_downlaod==1",
        textInput(inputId = "name",
                  "file name"
        ),
        
        selectInput(inputId = "format",
                    "file format",
                    choices = u_format,
                    "pdf"
        ),
        
        actionButton("download","Download image"),
        hr()
      ),
      
      
      
      ######################################################
      ########Reset button and generate image button########
      ######################################################
      actionButton("ab","Generate brain map"),
      tags$button("Restart", id="restart", type="button", class="btn btn-danger action-button", onclick="history.go(0)"),
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",id = "tab",
                  tabPanel("Table",DT::dataTableOutput("table")),
                  tabPanel("3D",plotlyOutput("ggseg3d",height = "700px")),
                  tabPanel("DistributionPlot",plotOutput("distributionPlot")))
    )
    
    
  )
)
