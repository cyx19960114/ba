library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)

shinyUI(pageWithSidebar(
  headerPanel("ggseg3d"),
  sidebarPanel(
    selectInput(inputId = "dataset",
                label = "waehlen Dataset:",
                choices = c("Herr","Frau")),
    selectInput(inputId = "dataset1",
                label = "waehlen Dataset:",
                choices = c("Herr","Frau")),
  ),
  mainPanel(
    plotlyOutput("ggseg3d"),
    plotlyOutput("ggseg3d1")
    
    
  )
))