# install.packages("remotes")
# remotes::install_github("LCBC-UiO/ggseg", build_vignettes = TRUE)
# remotes::install_github("LCBC-UiO/ggseg3d", build_vignettes = TRUE)
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
OASIS[-1:-2] <- apply(OASIS[-1:-2],2,as.numeric)
id_sex_age <- OASIS[,1:3]
options(warn = -1)

server<-function(input, output,session) {
  
  
  ######################################data preprocess######################################### 
  
  
  
  #######################################################
  ######make the names pass to left and right brain######
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
  
  
  
  #######################################################
  #############transform the names of OASIS##############
  #######################################################
  oasis_data <- OASIS
  region_names <- names(oasis_data)[-1:-3]
  
  
  
  #######################################################
  ##########output the ui from the selected col##########
  #######################################################
  get_fil <- reactive({
    input$fil
  })
  
  u_oasis <- oasis_data
  
  get_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_fil()
    for (col in col_input) {
      if(input$com==0){
        v <- input[[col]]
        if(!(is.null(v)|| ""==v)){
          u_oasis <- u_oasis[u_oasis[[col]]==v,]
        }
      }else{
        if(!is.null(input[[paste0(col,"_range")]])){
          v_min <- input[[paste0(col,"_range")]]
          v_max <- input[[paste0(col,"_range")]]
          u_oasis <- u_oasis[u_oasis[[col]]<=as.numeric(v_max),]
          u_oasis <- u_oasis[u_oasis[[col]]>=as.numeric(v_min),]
        }
        
        if(as.character(col)=='sex'){
          u_oasis <- u_oasis[u_oasis$sex==input$sex,]
        }
        u_oasis
      }
    }
    return(u_oasis)
  })
  
  observeEvent(input$com,
               {
                 if(input$com!=0){
                   updateSelectInput(session,inputId = "fil",choices = names(OASIS)[-1])
                 }else{
                   updateSelectInput(session,inputId = "fil",choices = names(OASIS))
                 }
               })
  
  
  output$kon <- renderUI({     # ouput the select UI
    x <- vector("list",length=length(get_fil()))
    if(input$com==0){
      for (ff in get_fil()) {
        x <- append(x,list(
          selectInput(
            inputId = paste0(ff),
            label = as.character(ff),
            choices = c(" "="",sort(unique(get_choice()[[ff]]))),
            selected = {
              if (is.null(input[[ff]])||""==input[[ff]]){
                ""
              } else{
                input[[ff]]
              }
            }
          )
        ))}
    }else{
      for (ff in get_fil()) {
        x <- append(x,list(
          if(as.character(ff)=="sex"){
            selectInput("sex",label = "sex",choices = c("M","F"))
          }else{
            sliderInput(paste0(ff,"_range"),paste(ff,"Range"),
                        min = min(OASIS[[ff]]),max=max(OASIS[[ff]]),
                        value = c(min(OASIS[[ff]]),max(OASIS[[ff]])))
          }
        ))
       
      }
      x <- append(x,list( radioButtons("com_way","median or mean",choices = c("median","mean"),
                                       selected = "median",inline = TRUE)))
    }
    return(x)
  })
  
  
  
  #######################################################
  ##################output OASIS table###################
  #######################################################
  output$table<- DT::renderDataTable({
    if(is.null(get_fil())
       ||(!(TRUE%in%lapply(get_fil(), function(x){
         input[[x]]!="" ||
           input[[paste0(x,"_range")]]!=0
       }))))
      return()
    isolate({
      ##change the color boundary automatically
      ausgewaehlte_daten <- get_choice()
      wert<-ausgewaehlte_daten[-1:-3]
      max_wert<-max(wert)
      min_wert<-min(wert)
      updateNumericInput(session, "wert_obergrenze", value = max_wert)
      updateNumericInput(session, "wert_untergrenze", value = min_wert)
      ##output table
      DT::datatable(ausgewaehlte_daten)
    })
  })
  
  observe({
    if ("age_range"%in%names(input)) {
      print(input$age_range)
    }
  })
  
 
  
  #######################################################
  ######update the tabs when single_region selected######
  ####################################################### 
  observeEvent(input$single_region,{
    if(input$single_region==0)
      hideTab(inputId ="tab",target = "DistributionPlot")
    
    if(input$single_region==01)
      showTab(inputId ="tab",target = "DistributionPlot")
  })
  
  
  
  #######################################################
  ########Add new values and colors(UI)##################
  #######################################################
  index_selection <- reactiveVal(1)
  observeEvent(input$add_mitte, {
    insertUI(
      selector = "#add_mitte",
      where = "beforeBegin",
      ui = tagList(column(
        12,
        colourInput(inputId = paste("color_mitte", index_selection(), sep = "_"),
                    label = paste("new colors", index_selection()),
                    "black"),
        numericInput(inputId = paste("wert_mitte", index_selection(), sep = "_"),
                     label = paste("new values ", index_selection()),
                     " ")
      )
      ))
    new_index <- index_selection() + 1
    index_selection(new_index)
  })
  
  observeEvent(input$remove_mitte, {
    removeUI(selector = paste0(".col-sm-12:has(#wert_mitte_", index_selection()-1, ")"))
    removeUI(selector = paste0(".col-sm-12:has(#color_mitte_", index_selection()-1, ")"))
    index_selection(index_selection()-1)
    
  })
  
  
  #######################################################
  ###############Automatic update values#################
  #######################################################
  
  # observeEvent(input$ID, {
  #   ausgewaehlte_daten <- get_choice()
  #   wert<-ausgewaehlte_daten[-1:-3]
  #   max_wert<-max(wert)
  #   min_wert<-min(wert)
  #   updateNumericInput(session, "wert_obergrenze", value = max_wert)
  #   updateNumericInput(session, "wert_untergrenze", value = min_wert)
  # })
  

  
  
  #######################################################
  ################Generate 3D brain map##################
  #######################################################
  output$ggseg3d<- renderPlotly({
    input$ab  
    if(input$ab==0)
      return()
    isolate({
      # if (input$ID =="" & input$com==0) {
      #   return()
      # }
      
      if (nrow(get_choice())==1) {
        auswahl_area <- get_choice()
        names(auswahl_area)[4:77] <- paste("L_Region",1:74)
        names(auswahl_area)[78:151] <- paste("R_Region",1:74)
        auswahl_area <- auswahl_area[-1:-3]
        
        if(input$single_region==1){ ## when only one region to display
          auswahl_region <- input$region
          save<-auswahl_area[[auswahl_region]]
          auswahl_area[1,]<-0.5
          auswahl_area[[auswahl_region]]<-save
        }
        auswahl_area <- t(auswahl_area)
        print(auswahl_area)
        auswahl_data = data.frame(
          area = as.character(row.names(auswahl_area)),
          wert = as.numeric(auswahl_area[,1]),
          strings_As_Factors = FALSE
        )
        
        auswahl_data$beschreibung <- paste("Region Names: ",region_names,", Wert ist ",auswahl_data$wert)
      }else if(input$com==0){
        showModal(modalDialog(title = "INPUT ERROR",
                              "The inputed date should be one line or composite display selected"))
        return()
      }
        else{
        auswahl_area <- get_choice()
        names(auswahl_area)[4:77] <- paste("L_Region",1:74)
        names(auswahl_area)[78:151] <- paste("R_Region",1:74)
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
      

      
      
      ###################################
      #######new values and colors#######
      ###################################

      auswahl_wert <- c(input$wert_obergrenze,input$wert_untergrenze)
      auswahl_color <- c(input$color_obergrenze,input$color_untergrenze)
      print(input[[paste("wert_mitte", 1, sep = "_")]])
      if (1<index_selection()) {
        print("aaaa")
        print(index_selection())
        for (i in 1:(index_selection()-1)) {
          auswahl_wert[i+2] <- input[[paste("wert_mitte", i, sep = "_")]]
          auswahl_color[i+2] <- input[[paste("color_mitte", i, sep = "_")]]
        }
      }
      names(auswahl_wert) <- auswahl_color
      print(auswahl_wert)
      
      
      
      ###################################
      ########select_hemisphere##########
      ###################################
      auswahl_hemisphere<-input$select_hemisphere
    
      
      
      ###################################
      #############ggseg3d###############
      ###################################
      ggseg3d(.data = auswahl_data,
              atlas = desterieux_neu,
              colour = "wert", text = "beschreibung",
              surface = "LCBC",
              palette = sort(auswahl_wert),
              hemisphere = auswahl_hemisphere,
              na.alpha= .5) %>%
        pan_camera("left lateral") %>%
        remove_axes()
      
      
    })
  })
  
  
  
  
  ###########################################
  #####distributionPlot when single person###
  ###########################################
  output$distributionPlot<-renderPlot({
    if(input$single_region==0)
      return()
    if(input$ab==0)
      return()
    isolate({
      region<-input$region
      auswahl_area <- get_choice()
      print(nrow(get_choice()))
      if(nrow(get_choice())!=1){
        return()
      }
      print(nrow(get_choice()))
      names(auswahl_area)[4:77] <- paste("L_Region",1:74)
      names(auswahl_area)[78:151] <- paste("R_Region",1:74)
      auswahl_area <- auswahl_area[-1:-3]
      
      names(oasis_data)[4:77] <- paste("L_Region",1:74)
      names(oasis_data)[78:151] <- paste("R_Region",1:74)
      oasis_data <- oasis_data[-1:-3]
      
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
  
}
