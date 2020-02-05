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
    } 
  })
  
  ########
  #observeEvent(input$region,{
  # s_region<-input$region
  
  #  })
  
  ########
  
  
  
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
  
  
  
  
  ## output OASIS table
  #######################################################
  output$table<- DT::renderDataTable(DT::datatable({
    if(input$ab==0)
      return()
    
    out_Oasis_table <- OASIS
    
    isolate({
      if(input$com==0){
        if(input$sex!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$sex==input$sex,]
        }
        if(input$age!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$age==input$age,]
        }
        if(input$id!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$ID==input$id,]
        }
      }else{
        if(input$sex!=""){
          out_Oasis_table<-out_Oasis_table[out_Oasis_table$sex==input$sex,]
        }
        out_Oasis_table <- filter(out_Oasis_table,age<=max(input$age_range),age>=min(input$age_range))
      }
      
    })
    
    out_Oasis_table
  }))
  
  
  
  
  ## make the names pass to left and right brain
  #######################################################
  
  desterieux_neu<-desterieux_3d # load desterieux_3d
  for (j in 1:6) {
    if (desterieux_neu[[3]][[j]] == "left") {
      for (i in 1:82) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Left_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]]))
      }
      for (i in 84:149) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Left_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1)
      }
    }else{
      for (i in 1:82) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Right_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]]))
      }
      for (i in 84:149) {
        desterieux_neu[[4]][[j]][[1]][[i]]<-paste("Right_Region",as.numeric(desterieux_neu[[4]][[j]][[5]][[i]])-1)
      }
    }
  }
  
  ## transform the names of OASIS
  oasis_data <- OASIS
  region_names <- names(oasis_data)[-1:-3]
  names(oasis_data)[4:77] <-paste("Left_Region",1:74) 
  names(oasis_data)[78:151] <-paste("Right_Region",1:74)
  
  
  
  ## make ggseg3d plot
  ######################################################
  output$ggseg3d<- renderPlotly({
    if(input$ab==0)
      return()

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

    isolate({
      # ggseg
      ggseg3d(.data = auswahl_data,
              atlas = desterieux_neu,
              colour = "wert", text = "beschreibung",
              surface = "LCBC",
              palette = c("yellow" = 1, "red" = 2, "green" = 2.5, "blue" = 3, "cyan" = 4, "white" = 4.2),
              hemisphere = c("left","right"),
              na.alpha= .5) %>%
        pan_camera("left lateral") %>%
        remove_axes()
    })
  })
  
  
    ##distributionPlot
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
