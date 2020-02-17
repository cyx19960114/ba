library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(plotly)
library(ggplot2)
library(readxl)
library(colourpicker)


OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
OASIS[-1:-3] <- apply(OASIS[-1:-3],1,as.numeric)
id_sex_age <- OASIS[,1:3]
u_color_mitte<-c("white","green","red","blue","yellow","cyan","purple")

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
    } 
  })
  
  
  
  observe(print(input$single_region))
  
  ##update age and id choices when sex changes
  #######################################################
  observeEvent(input$sex,{
    s_sex <- input$sex
    if(s_sex!=""){
      if(input$age==""){
        age_sex <- sort(as.numeric(unique(OASIS[OASIS$sex==s_sex,]$age)))
        updateSelectInput(session,"age",
                          choices = c("age"="",age_sex)
        )
        updateSelectInput(session,"id",
                          choices = c("id"="",sort(OASIS[OASIS$sex==s_sex,]$ID)))
        updateSliderInput(session, "age_range",
                          min = min(age_sex),max = max(age_sex),
                          value = c(min(age_sex),max(age_sex)))
      }else{
        updateSelectInput(session,"id",
                          choices = c("id"="",sort((OASIS[OASIS$sex==s_sex & OASIS$age==input$age,]$ID)))
        )
      }
    } 
  })
  
  
  
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
  
  ## update the tabs when single_region selected
  ######################################################
  observeEvent(input$single_region,{
    if(input$single_region==0)
      hideTab(inputId ="tab",target = "DistributionPlot")
    
    if(input$single_region==01)
      showTab(inputId ="tab",target = "DistributionPlot")
  }
  )
  
  
  ## output OASIS table
  #######################################################
  output$table<- DT::renderDataTable({
    input$ab
    if(input$ab==0)
      return()
    
    isolate({
      if (input$sex=="" & input$age=="" & input$id=="" & input$com==0) {
        return()
      }
      
      out_Oasis_table <- OASIS
      if(input$com==0){ ## when single person data
        if(input$sex!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$sex==input$sex,]
        }
        if(input$age!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$age==input$age,]
        }
        if(input$id!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$ID==input$id,]
        }
      }else{ ## when composite data
        if(input$sex!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$sex==input$sex,]
        }
        out_Oasis_table <- filter(out_Oasis_table,age<=max(input$age_range),age>=min(input$age_range))
      }
    })
    DT::datatable(out_Oasis_table)
  })
  
  
  
  ## make the names pass to left and right brain
  #######################################################
  
  desterieux_neu<-desterieux_3d # load desterieux_3d
  for (j in 1:6) {
    if (desterieux_neu[[3]][[j]] == "left") {
      for (i in 1:82) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("L_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]]))
      }
      for (i in 84:149) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("L_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1)
      }
    }else{
      for (i in 1:82) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("R_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]]))
      }
      for (i in 84:149) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("R_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1)
      }
    }
  }
  
  ## transform the names of OASIS
  #######################################################
  oasis_data <- OASIS
  region_names <- names(oasis_data)[-1:-3]
  names(oasis_data)[4:77] <-paste("L_Region",1:74) 
  names(oasis_data)[78:151] <-paste("R_Region",1:74)
  
  ######################add mitte color und wert####################################  
  index_selection <- reactiveVal(1)
  observeEvent(input$add_mitte, {
    insertUI(
      selector = "#add_mitte",
      where = "beforeBegin",
      ui = tagList(
       # selectInput(inputId = paste("color_mitte", index_selection(), sep = "_"),
             #       label = paste("color_mitte", index_selection(), sep = "_"),
             #       choices = u_color_mitte,
              #      " "),
        colourInput(inputId = paste("color_mitte", index_selection(), sep = "_"),
                    label = paste("color_mitte", index_selection(), sep = "_"),
                    "black"),
        numericInput(inputId = paste("wert_mitte", index_selection(), sep = "_"),
                     label = paste("wert_mitte", index_selection(), sep = "_"),
                     " ")
      )
    )
    new_index <- index_selection() + 1
    index_selection(new_index)
  })
  #############################################################
  
  
  ## make ggseg3d plot
  ######################################################
  output$ggseg3d<- renderPlotly({
    input$ab  
    if(input$ab==0)
      return()
    isolate({
      if (input$id =="" & input$com==0) {
        return()
      }
      if(input$id!=""){  ## when single person
        auswahl_id <- input$id
        auswahl_area <- oasis_data[oasis_data$ID==auswahl_id,]
        auswahl_area <- auswahl_area[-1:-3]
        if(input$single_region==1){ ## when only one region to display
          auswahl_region <- input$region
          save<-auswahl_area[[auswahl_region]]
          auswahl_area[1,]<-0.5
          auswahl_area[[auswahl_region]]<-save
        }
        auswahl_area <- t(auswahl_area)
        auswahl_data = data.frame(
          area = as.character(row.names(auswahl_area)),
          wert = as.numeric(auswahl_area[,1]),
          strings_As_Factors = FALSE
        )
        auswahl_data$beschreibung <- paste("Region Names: ",region_names,", Wert ist ",auswahl_data$wert)
      }
      
      if(input$com!=0){
        age_min = min(input$age_range)
        age_max = max(input$age_range)
        auswahl_area <- oasis_data[oasis_data$age<age_max & oasis_data$age>age_min,]
        if(input$com_sex!="All"){
          auswahl_area <- auswahl_area[auswahl_area$sex==input$com_sex,]
        }
        auswahl_area <- auswahl_area[-1:-3]
        
        if(input$com_way=="median"){
          auswahl_area[1,] <- apply(auswahl_area, 2, median)
          auswahl_area <- auswahl_area[1,]
        }
        
        if (input$com_way=="mean") {
          auswahl_area[1,] <- apply(auswahl_area, 2, mean)
          auswahl_area <- auswahl_area[1,]
        }
        auswahl_area <- t(auswahl_area)
        auswahl_data <- data.frame(
          area = as.character(row.names(auswahl_area)),
          wert = auswahl_area[,1],
          strings_As_Factors = FALSE
        )
        auswahl_data$beschreibung <- paste("Region Names: ",region_names,", Wert ist ",auswahl_data$wert)
      }
      
      ####waehlen color und wert(grenze)######################
      auswahl_wert<- c(input$wert_untergrenze,input$wert_mitte_1,input$wert_mitte_2,input$wert_mitte_3,input$wert_mitte_4,input$wert_mitte_5,input$wert_obergrenze)
      names(auswahl_wert)<-c(input$color_untergrenze, input$color_mitte_1,input$color_mitte_2,input$color_mitte_3,input$color_mitte_4,input$color_mitte_5,input$color_obergrenze)
      auswahl_color<-auswahl_wert
      ############################
      
      # ggseg
      ggseg3d(.data = auswahl_data,
              atlas = desterieux_neu,
              colour = "wert", text = "beschreibung",
              surface = "LCBC",
              palette = auswahl_color,
              hemisphere = c("left","right"),
              na.alpha= .5) %>%
        pan_camera("left lateral") %>%
        remove_axes()
    })
  })
  
  
  ##distributionPlot when single person
  ################################
  output$distributionPlot<-renderPlot({
    if(input$single_region==0)
      return()
    if(input$ab==0)
      return()
    isolate({
      region<-input$region
      auswahl_id <- input$id
      auswahl_area <- oasis_data[oasis_data$ID==auswahl_id,]
      auswahl_area <- auswahl_area[-1:-3]
      auswahl_region <- input$region
      selectedData <- oasis_data[auswahl_region]
      colnames(selectedData) <- c("distributionPlot")
      selectedData$distributionPlot<-as.numeric(selectedData$distributionPlot)
      
      save<-auswahl_area[[auswahl_region]]
      save<-as.numeric(save)
      
      ggplot(selectedData,
             aes(x = distributionPlot)
      ) + geom_density() + geom_point(aes(save,0),col="red", size=8)
    })
    
  })
  ##########################################
  
  # observe(for (variable in input$fil) {
  #   print(variable)
  # })
  get_fil <- reactive({
    input$fil
  })
  output$kon <- renderUI({
    x <- vector("list",length=length(get_fil()))
    for (ff in get_fil()) {
      x <- append(x,
      list(
        selectInput(inputId = paste0(ff),
                  label = as.character(ff),
                  choices = c(" "="",OASIS[[ff]])
      ))
      )
    }
    return(x)
  }
  )
  
  
  
  
  
  ##########################################
  # 暂时没用
  # # progress report
  # progress_load <- Progress$new(session,min = 1,max=15)
  # on.exit(progress_load$close())
  # progress_load$set(message = "3d Image loading")
  # for (i in 1:15) {
  #   progress_load$set(value = i)
  #   Sys.sleep(1)
  # }
  
  
}
