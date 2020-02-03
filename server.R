library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(plotly)
library(ggplot2)
library(readxl)


OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
id_sex_age <- OASIS[,1:3]

server<-function(input, output,session) {
  
  
  #update sex and id choices when age changes
  #######################################################
  observeEvent(input$age,{
    s_age <- input$age
    if(s_age!=""){
      if(input$sex==""){
        updateSelectInput(session,"sex",
                          choices = c("age"="",sort(unique(OASIS[OASIS$age==s_age,]$sex)))
        )
        updateSelectInput(session,"id",
                          choices = c("id"="",sort(OASIS[OASIS$age==s_age,]$ID)))
      }else{
        updateSelectInput(session,"id",
                          choices = c("id"="",sort(OASIS[OASIS$age==s_age & OASIS$sex==input$sex,]$ID)))
      }
      
      
    } })
  
  
  
  ##update age and id choices when sex changes
  #######################################################
  observeEvent(input$sex,{
    s_sex <- input$sex
    if(s_sex!=""){
      if(input$age==""){
        updateSelectInput(session,"age",
                          choices = c("age"="",sort(as.numeric(unique(OASIS[OASIS$sex==s_sex,]$age))))
        )
        updateSelectInput(session,"id",
                          choices = c("id"="",sort(OASIS[OASIS$sex==s_sex,]$ID)))
      }else{
        updateSelectInput(session,"id",
                          choices = c("id"="",sort((OASIS[OASIS$sex==s_sex & OASIS$age==input$age,]$ID)))
        )
      }
    } })
  
  
  
  # when the id is detemined, update the sex and age value
  #######################################################
  observeEvent(input$id,{
    s_id <- input$id
    if (s_id!="") {
      if(input$sex ==""){
        updateSelectizeInput(session,"sex",
                             options = list(
                               placeholder = OASIS[OASIS$ID==s_id,]$sex,
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      }
      if(input$age ==""){
        updateSelectizeInput(session,"age",
                             options = list(
                               placeholder = OASIS[OASIS$ID==s_id,]$age,
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      }
    }
  })
  

  
  ## output OASIS table
  #######################################################
  output$table<- DT::renderDataTable(DT::datatable({
    if(input$sex!=""){
      OASIS<-OASIS[OASIS$sex==input$sex,]
    }
    if(input$age!=""){
      OASIS<-OASIS[OASIS$age==input$age,]
    }
    if(input$id!=""){
      OASIS<-OASIS[OASIS$ID==input$id,]
    }
    OASIS
  }))
  
  
  datasetInput<-reactive({
    switch(input$dataset,
           "Herr"= Herr,"Frau"= Frau)
  })
  
  datasetInput1<-reactive({
    switch(input$dataset1,
           "Herr"= Herr,"Frau"= Frau)
  })
  
  
  
  # output$ggseg3d<- renderPlotly({  ggseg3d(.data = example1Data,
  #                                          atlas = desterieux_neu,
  #                                          colour = "wert", text = "beschreibung",
  #                                          surface = "LCBC",
  #                                          palette = c("red" = 1, "yellow" = 2, "blue" = 3),
  #                                          hemisphere = c("left","right"),
  #                                          na.alpha= .5) %>%
  #     pan_camera("left lateral") %>%
  #     remove_axes()})
  
  
}
