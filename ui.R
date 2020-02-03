library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(ggplot2)

## read OASIS
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]
u_age <- sort(as.numeric(unique(id_sex_age$age)))
u_IDs <- id_sex_age$ID
u_sex <- c("F","M")

fluidPage(
  titlePanel("ggseg3d"),

fluidRow(
  column(4,
    selectizeInput(inputId = "sex",
                label = "waehlen Geschlecht:",
                choices = c("All",u_sex))
  ),
  column(4,
    selectInput(inputId = "age",
                label = "waehlen Alt:",
                choices = c("All",u_age))
  ),
  column(4,
    selectInput(inputId = "id",
                label = "waehlen ID",
                choices = c("All",u_IDs))
  )
    ),
    DT::dataTableOutput("table"),
    plotlyOutput("ggseg3d")
  )
