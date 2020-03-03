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
library(scales)
source("title_fun.R")


OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
OASIS[-1:-2] <- apply(OASIS[-1:-2],2,as.numeric)
id_sex_age <- OASIS[,1:3]

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
  
  
  
  #######################################################
  ##########output the ui from the selected col##########
  #######################################################
  get_fil <- reactive({
    input$fil
  })
  
  get_fil_com <- reactive({
    input$fil_com
  })
  
  
  
  get_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_fil()
    col_com_input <- get_fil_com()
    u_oasis <- oasis_data
    if(input$select_hemisphere=="left"){
      u_oasis <- u_oasis[-78:-151]
      region_names <- names(u_oasis[-1:-3])
    }else if(input$select_hemisphere=="right"){
      u_oasis <- u_oasis[-4:-77]
      region_names <- names(u_oasis[-1:-3])
    }
    
    if(input$com==0){
      for (col in col_input) {
        v <- input[[col]]
        if(!(is.null(v)|| ""==v)){
          if(as.character(col)=='sex'){
            if(input$sex!="All"){
              u_oasis <- u_oasis[u_oasis$sex==input$sex,]
            }}
          else{
            u_oasis <- u_oasis[u_oasis[[col]]==v,]
          }}
      }
    }else{
      for (col in col_com_input) {
        if(as.character(col)=='sex'){
          if((!is.null(input$sex_range))&&input$sex_range!="All"){
            u_oasis <- u_oasis[u_oasis$sex==input$sex_range,]
          }
        }else if(!is.null(input[[paste0(col,"_range")]])){
          v_min <- min(input[[paste0(col,"_range")]])
          v_max <- max(input[[paste0(col,"_range")]])
          u_oasis <- u_oasis[u_oasis[[col]]<=as.numeric(v_max),]
          u_oasis <- u_oasis[u_oasis[[col]]>=as.numeric(v_min),]}
        
      }
    }
    return(u_oasis)
  })
  
  get_region_names <- reactive({
    region_names <- names(get_choice())
    if(input$select_hemisphere=="left"){
      region_names <- region_names[-78:-151]
    }else if(input$select_hemisphere=="right"){
      region_names <- region_names[-4:-77]
    }
    region_names <- region_names[-1:-3]
    return(region_names)
  })
  
  
  output$kon <- renderUI({     # ouput the select UI
    x <- vector("list",length=length(get_fil()))
    if(input$com==0){
      for (ff in get_fil()) {
        x <- append(x,list(
          if(as.character(ff)=="sex"){
            selectInput("sex",
                        label = "sex",
                        choices = c("All","M","F"),
                        selected = {
                          if (is.null(input[[ff]])||""==input[[ff]]){
                            "All"
                          } else{
                            input[[ff]]
                          }
                        })
          }
          else{
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
            )}
        ))}
    }else{
      for (ff in get_fil_com()) {
        x <- append(x,list(
          if(as.character(ff)=="ID"){}
          else if(as.character(ff)=="sex"){
            selectInput("sex_range",
                        label = "sex",
                        choices = c("All","M","F"),
                        selected = {
                          if (is.null(input[["sex_range"]])||""==input[["sex_range"]]){
                            "All"
                          } else{
                            input[["sex_range"]]
                          }
                        })
          }else{
            sliderInput(paste0(ff,"_range"),paste(ff,"Range"),
                        min = min(oasis_data[[ff]]),max=max(oasis_data[[ff]]),
                        value = {if(!is.null(input[[paste0(ff,"_range")]])){
                          input[[paste0(ff,"_range")]]
                        }else{
                          c(min(oasis_data[[ff]]),max(oasis_data[[ff]]))
                        }
                        }
            )
          }
        ))
      }
      x <- append(x,list( radioButtons("com_way","Formula Mode",
                                       choices = c("median","mean","SD"),
                                       selected = {
                                         if(is.null(input$com_way)){"median"}
                                         else{input[["com_way"]]}
                                       },
                                       inline = TRUE)))
    }
    return(x)
  })
  
  aus_daten <- reactive({  ## get the composite way 
    if(!is.null(input$com_way)){
      switch (as.vector(input$com_way[[1]]),
              "median" = "median",
              "mean" = "mean",
              "SD"= "sd"
      )
    }
  })
  
  
  
  
  
  
  #######################################################
  ##################output OASIS table###################
  #######################################################
  output$table<- DT::renderDataTable({
    if(is.null(get_fil())&&is.null(get_fil_com())){return()}
    lapply(get_fil(), function(x){input[[x]]!=""})
    lapply(get_fil_com(), function(x){input[[paste0(x,"_range")]]!=0})
    if (input$com) {}
    if(is.null(input$com_way)){}
    input$select_hemisphere
    isolate({
      ##change the color boundary automatically
      ausgewaehlte_daten <- get_choice()
      wert<-ausgewaehlte_daten[-1:-3]
      DT::datatable(ausgewaehlte_daten)
    })
  })
  
  
  #######################################################
  ###################color update auto###################
  #######################################################  
  observeEvent({
    input$fil
    input$fil_com
    is.null(input$com_way)
    lapply(get_fil(), function(x){input[[x]]})
    lapply(get_fil_com(), function(x){input[[paste0(x,"_range")]]})
    input$select_hemisphere
  },{
    ausgewaehlte_daten <- get_choice()
    wert<-ausgewaehlte_daten[-1:-3]
    if (!is.null(input$com_way)) {
      wert[1,] <- apply(wert, 2, aus_daten())
      wert <- wert[1,]
    }
    max_wert<-max(wert)
    min_wert<-min(wert)
    updateNumericInput(session, "wert_obergrenze", value = max_wert)
    updateNumericInput(session, "wert_untergrenze", value = min_wert)
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
  
  
  get_auswahl_data <- reactive({
    auswahl_area <- get_choice()
    if(input$select_hemisphere=="left"){
      names(auswahl_area)[4:77] <- paste("L_Region",1:74)
    }else if(input$select_hemisphere=="right"){
      names(auswahl_area)[4:77] <- paste("R_Region",1:74)
    }else{
      names(auswahl_area)[4:77] <- paste("L_Region",1:74)
      names(auswahl_area)[78:151] <- paste("R_Region",1:74)
    }
    auswahl_area <- auswahl_area[-1:-3]
    if (nrow(get_choice())==1) {
      if(input$single_region==1){ ## when only one region to display
        auswahl_region <- input$region
        save<-auswahl_area[[auswahl_region]]
        auswahl_area[1,]<-0.5
        auswahl_area[[auswahl_region]]<-save
      }
    }else if(input$com==0){
      showModal(modalDialog(title = "INPUT ERROR",
                            "The inputed date should be one line or composite display selected",
                            easyClose = TRUE))
      return(NULL)
    }
    else{
      auswahl_area[1,] <- apply(auswahl_area, 2, aus_daten())
      auswahl_area <- auswahl_area[1,]
    }
    
    auswahl_area <- t(auswahl_area)
    auswahl_data <- data.frame(
      area = as.character(row.names(auswahl_area)),
      wert = auswahl_area[,1],
      stringsAsFactors = FALSE
    )
    auswahl_data$beschreibung <- paste("Region Names: ",get_region_names(),", Wert ist ",auswahl_data$wert)
    
    
    return(auswahl_data)
  })
  
  
  
  
  #######################################################
  ################Generate 3D brain map##################
  #######################################################
  output$ggseg3d<- renderPlotly({
    input$ab  
    if(input$ab==0)
      return()
    isolate({
      # if (input$com==0 & input$ID =="") {
      #   return()
      # }
      auswahl_data <- get_auswahl_data()
      
      if (is.null(auswahl_data)) {
        return()
      }
      
      ###################################
      #######new values and colors#######
      ###################################
      
      auswahl_wert <- c(input$wert_obergrenze,input$wert_untergrenze)
      auswahl_color <- c(input$color_obergrenze,input$color_untergrenze)
      if (1<index_selection()) {
        for (i in 1:(index_selection()-1)) {
          auswahl_wert[i+2] <- input[[paste("wert_mitte", i, sep = "_")]]
          auswahl_color[i+2] <- input[[paste("color_mitte", i, sep = "_")]]
        }
      }
      names(auswahl_wert) <- auswahl_color
      
      
      
      ###################################
      ########select_hemisphere##########
      ###################################
      auswahl_hemisphere<-input$select_hemisphere
      if(auswahl_hemisphere=="All"){auswahl_hemisphere=c("left","right")}
      
      
      ###################################
      #############ggseg3d###############
      ###################################
      
      gg <- ggseg3d(.data = auswahl_data,
                    atlas = desterieux_neu,
                    colour = "wert", text = "beschreibung",
                    surface = "LCBC",
                    palette = sort(auswahl_wert),
                    hemisphere = auswahl_hemisphere,
                    na.alpha= .5,
                    show.legend = TRUE,
                    options.legend = (colorbar=list(title=list(text="mm")))) %>%
        pan_camera("left lateral") %>%
        remove_axes()
      
      gg
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
      if(nrow(get_choice())!=1){
        return()
      }
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