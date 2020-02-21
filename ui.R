library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(ggplot2)
library(readxl)
library(plotly)
library(colourpicker)

## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]
u_age <- sort(as.numeric(unique(id_sex_age$age)))
u_IDs <- id_sex_age$ID
u_sex <- c("F","M")
u_region<-c(paste("L_Region",1:74), paste("R_Region",1:74))
u_color_obergrenze<-c("white","green","red","blue","yellow","cyan","purple")
u_color_mitte<-c("white","green","red","blue","yellow","cyan","purple")
u_color_untergrenze<-c("white","green","red","blue","yellow","cyan","purple")

fluidPage(
  titlePanel("ggseg3d"),
  sidebarLayout(
    sidebarPanel(
      
      ## single person auswahl
      #################################
      selectInput("fil",label = "Filtern",
                  choices = names(OASIS),multiple = TRUE),
      # conditionalPanel(
        # condition = "input.com==0",
        uiOutput("kon"),
      # ),
      
      
      #############################
      ##composite display
      #############################
      # conditionalPanel(
      #   condition = "input.com==1",
      #   uiOutput("kon_com"),
      #   selectizeInput(inputId = "com_sex",
      #                  label = "waehlen Geschlecht:",
      #                  choices = c("All",u_sex)
      #   ),
      #   sliderInput("age_range","Age Range",
      #               min = min(u_age),max=max(u_age),
      #               value = c(min(u_age),max(u_age))),
      #   radioButtons("com_way","median or mean",choices = c("median","mean"),
      #                selected = "median",inline = TRUE)
      # ),
      
      
      
      
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
      
      checkboxInput(inputId="com",label="composite display",value=FALSE),
      
      
      
      #############################
      ##composite display
      #############################
      
      
      
      
      
      
      
      
      checkboxInput("farbe_wert","Farbe und Wert",FALSE),
      ##waehlen color und wert(grenze)
      ######################################################
      conditionalPanel(
        condition = "input.farbe_wert == 1",
        # selectInput(inputId = "color_obergrenze",
        #            label = "waehlen color_obergrenze",
        #           choices = u_color_obergrenze,
        #          "red"),
        colourInput("color_obergrenze", "waehlen color_obergrenze", "red"),
        numericInput(inputId = 'wert_obergrenze',
                     label = 'wert_obergrenze',
                     4.2),
        # selectInput(inputId = "color_untergrenze",
        #       label = "waehlen color_untergrenz",
        #     choices = u_color_untergrenze,
        #    "blue"),
        colourInput("color_untergrenze", "waehlen color_untergrenze", "blue"),
        numericInput(inputId = 'wert_untergrenze',
                     label = 'wert_untergrenze',
                     2),
        actionButton("add_mitte", "Add wert_mitte"),
        actionButton("remove_mitte","Remove"),
        hr()
        
      ),
      #########################################################
      
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
