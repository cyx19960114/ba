# install.packages("remotes")
# remotes::install_github("LCBC-UiO/ggseg", build_vignettes = TRUE)
# remotes::install_github("LCBC-UiO/ggseg3d", build_vignettes = TRUE)

library(shiny)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(plotly)
library(ggplot2)
library(readxl)
library(colourpicker)
library(scales)
library(processx)
library(reshape2)
library(cowplot)
library(ggpubr)
library("XLConnect")
library(glmnet)
library(gplots)

# source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
source("geom_flat_violin.R")
source("lm_function_74.R")
# source("title_fun.R")
file.source=list.files("ggseg3d//R",pattern="*.R",full.names = TRUE)
lapply(file.source, source,.GlobalEnv)
load("desterieux_3d.rda")
sem <- function(x){
  return(sd(x)/sqrt(length(x)))
}




server<-function(input, output,session) {
  OASIS <<- NULL
  
  
  
  get_data_file <- reactive({
    input$data_table
    if(!is.null(input$data_table)){
      OASIS <<- read_excel(input$data_table[["datapath"]])
      return(TRUE)
    }else{
      # OASIS <<- read_excel("OASIS_behavioral.xlsx")
      return(FALSE)
      # return(TRUE)
      
    }
  })
  
  output$dataFileLoad <- reactive({
    return(get_data_file())
  })
  
  outputOptions(output,'dataFileLoad',suspendWhenHidden=FALSE)
  
  
  
  
  ######################################data preprocess######################################### 
  
  
  
  #######################################################
  #############transform the names of OASIS##############
  #######################################################
  
  
  get_oasis <- reactive({
    is.null(input$data_table)
    if(!is.null(input$name_file)){
      area <- read_excel(input$name_file[["datapath"]],col_names = FALSE)
      cols <- ncol(OASIS)
      
      # the to be changed names
      oasis_r <- OASIS[(cols-147):cols]
      n <- names(oasis_r)
      nt <- sub("lh","",n)
      nt <- sub("rh","",nt)
      nt <- sub("thickness","",nt)
      nt <- gsub("_","",nt)
      nt <- gsub("-","",nt)
      
      
      o_table <- tibble(names(oasis_r),nt,seq(1:length(names(oasis_r))))
      names(o_table) <- c("o_names","pattern","name_seq")
      
      #the replacement names
      an <- area[[2]]
      an <- gsub("_","",an)
      an <- gsub("-","",an)
      an <- sub("and","",an)
      area[[2]] <- an
      names(area) <- c("num","pattern","replacement")
      pattern <- merge(o_table,area)
      
      pattern <- pattern[c("o_names","name_seq","replacement")]%>%arrange(name_seq)
      
      get_name <- function(x){
        if(grepl("lh",x["o_names"])){
          x["o_names"] <- paste("L",x["replacement"],sep = " ")
        }else if(grepl("rh",x["o_names"])){
          x["o_names"] <- paste("R",x["replacement"],sep = " ")
        }
      }
      
      pattern <- apply(pattern,1,get_name)
      
      names(OASIS)[(cols-147):cols] <- pattern
      
      oasis_data <- OASIS
      updateSelectInput(session,"qc_fil",choices = c("sex","age",pattern))
      updateSelectInput(session,"fil",choices = c("ID","sex","age",pattern))
      updateSelectInput(session,"fil_com",choices = c("sex","age",pattern))
      
      print("change")
      return(oasis_data)
    }else{
      oasis_data <- OASIS
      return(oasis_data)
    }
    
  })
  
  
  
  
  get_fil <- reactive({
    input$fil
  })
  
  get_fil_com <- reactive({
    input$fil_com
  })
  
  
  get_qc_fil <- reactive({
    input$qc_fil
  })
  
  get_ss_fil <- reactive({
    input$ss_fil
  })
  
  get_ls_fil <- reactive({
    input$ls_fil
  })
  
  
  
  #######################################################
  ######make the names pass to left and right brain######
  #######################################################
  # load desterieux_3d
  get_altes <- reactive({
    desterieux_neu<-desterieux_3d
    oa <- get_oasis()
    cols <- ncol(oa)
    t_name <- names(oa[(cols-147):cols])
    for (j in 1:6) {
      if (desterieux_neu[[3]][[j]] == "left") {
        for (i in 1:82) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+1)/2)]
        }
        for (i in 84:149) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc(i/2)]
        }
      }else{
        for (i in 1:82) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+149)/2)]
        }
        for (i in 84:149) {
          desterieux_neu[[4]][[j]][[1]][[i]]<-t_name[trunc((i+148)/2)]
        }
      }
    }
    
    return(desterieux_neu)
  })
  
  
  #######################################################
  ##############output the select choices################
  #######################################################
  
  output$fil_ui <- renderUI({
    if(input$com==0){
      tagList(
        selectInput("fil",label = "Filter",
                    choices = names(OASIS),multiple = TRUE)
      )
    }else{
      tagList(
        selectInput("fil_com",label = "Filter",
                    choices = names(OASIS)[-1],multiple = TRUE)
      )
    }
  })
  
  output$fil_qc <- renderUI({
    tagList(
      selectInput("qc_fil",label = "Filter",choices = names(OASIS)[-1],multiple = TRUE)
    )
  })
  
  
  
  
  ## get the explan names
  get_explan_names <- reactive({
    cols <- ncol(OASIS)
    explans <- dplyr::select(OASIS,-(cols-147):-cols)
    names_explan <- names(explans)
    to_dellte_name <- which(names_explan==c("ID","sex"))
    names_explan <- names_explan[-to_dellte_name]
    
    return(names_explan)
  })  
  
  output$fil_ss <- renderUI({
    tagList(
      selectInput("ss_fil",label = "Filter",choices = names(OASIS)[-1],multiple = TRUE)
    )
  })
  
  output$fil_ls <- renderUI({
    tagList(
      selectInput("ls_fil",label = "Filter",choices = names(OASIS)[-1],multiple = TRUE)
    )
  })
  
  
  
  
  
  #######################################################
  #filter the data from according to selected condition##
  #######################################################
  get_choice <- reactive({    # get the select col and return the selected date
    input$data_table
    col_input <- get_fil()
    col_com_input <- get_fil_com()
    u_oasis <- get_oasis()
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
  
  
  
  get_qc_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_qc_fil()
    u_oasis <- get_oasis()
    for (col in col_input) {
      if(as.character(col)=='sex'){
        if((!is.null(input$sex_qc))&&input$sex_qc!="All"){
          u_oasis <- u_oasis[u_oasis$sex==input$sex_qc,]
        }
      }else if(!is.null(input[[paste0(col,"_qc")]])){
        v_min <- min(input[[paste0(col,"_qc")]])
        v_max <- max(input[[paste0(col,"_qc")]])
        u_oasis <- u_oasis[u_oasis[[col]]<=as.numeric(v_max),]
        u_oasis <- u_oasis[u_oasis[[col]]>=as.numeric(v_min),]}
      
    }
    
    return(u_oasis)
  })
  
  get_ss_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_ss_fil()
    u_oasis <- get_oasis()
    for (col in col_input) {
      if(as.character(col)=='sex'){
        if((!is.null(input$sex_qc))&&input$sex_qc!="All"){
          u_oasis <- u_oasis[u_oasis$sex==input$sex_qc,]
        }
      }else if(!is.null(input[[paste0(col,"_qc")]])){
        v_min <- min(input[[paste0(col,"_qc")]])
        v_max <- max(input[[paste0(col,"_qc")]])
        u_oasis <- u_oasis[u_oasis[[col]]<=as.numeric(v_max),]
        u_oasis <- u_oasis[u_oasis[[col]]>=as.numeric(v_min),]}
      
    }
    
    return(u_oasis)
  })
  
  get_ls_choice <- reactive({    # get the select col and return the selected date
    col_input <- get_ls_fil()
    u_oasis <- get_oasis()
    for (col in col_input) {
      if(as.character(col)=='sex'){
        if((!is.null(input$sex_qc))&&input$sex_qc!="All"){
          u_oasis <- u_oasis[u_oasis$sex==input$sex_qc,]
        }
      }else if(!is.null(input[[paste0(col,"_qc")]])){
        v_min <- min(input[[paste0(col,"_qc")]])
        v_max <- max(input[[paste0(col,"_qc")]])
        u_oasis <- u_oasis[u_oasis[[col]]<=as.numeric(v_max),]
        u_oasis <- u_oasis[u_oasis[[col]]>=as.numeric(v_min),]}
      
    }
    
    return(u_oasis)
  })
  
  
  
  
  
  
  
  

  
  
  #######################################################
  ##########output the ui from the selected col##########
  #######################################################
  
  
  output$ds_kon <- renderUI({     # ouput the select UI
    x <- vector("list",length=length(get_fil()))
    if(input$com==0||is.null(input$com)){
      for (ff in get_fil()) {
        x <- append(x,list(
          if(as.character(ff)=="sex"){
            selectInput("sex",
                        label = "sex",
                        choices = unique(append("All",get_choice()[["sex"]])),
                        selected = {
                          if (is.null(input[[ff]])||"All"==input[[ff]]){
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
                        choices = unique(append("All",c("F","M"))),
                        selected = {
                          if (is.null(input[["sex_range"]])||"All"==input[["sex_range"]]){
                            "All"
                          } else{
                            input[["sex_range"]]
                          }
                        })
          }else{
            sliderInput(paste0(ff,"_range"),paste(ff),
                        min = min(get_oasis()[[ff]]),max=max(get_oasis()[[ff]]),
                        value = {if(!is.null(input[[paste0(ff,"_range")]])){
                          input[[paste0(ff,"_range")]]
                        }else{
                          c(min(get_oasis()[[ff]]),max(get_oasis()[[ff]]))
                        }
                        }
            )
          }
        ))
      }
      
      
    }
    return(x)
  })
  
  
  ## Quality Control
  output$qc_kon <- renderUI({
    x <- NULL
    for (ff in get_qc_fil()) {
      x <- append(x,list(
        if(as.character(ff)=="sex"){
          selectInput("sex_qc",
                      label = "sex",
                      choices = unique(append("All",c("F","M"))),
                      selected = {
                        if (is.null(input[["sex_qc"]])||"All"==input[["sex_qc"]]){
                          "All"
                        } else{
                          input[["sex_qc"]]
                        }
                      })
        }else{
          sliderInput(paste0(ff,"_qc"),paste(ff),
                      min = min(get_oasis()[[ff]]),max=max(get_oasis()[[ff]]),
                      value = {if(!is.null(input[[paste0(ff,"_qc")]])){
                        input[[paste0(ff,"_qc")]]
                      }else{
                        c(min(get_oasis()[[ff]]),max(get_oasis()[[ff]]))
                      }
                      }
          )
        }
      ))
    }
    return(x)
  })
  
  
  
  
  ## Stastics
  output$ss_kon <- renderUI({
    x <- NULL
    for (ff in get_ss_fil()) {
      x <- append(x,list(
        if(as.character(ff)=="sex"){
          selectInput("sex_qc",
                      label = "sex",
                      choices = unique(append("All",c("F","M"))),
                      selected = {
                        if (is.null(input[["sex_qc"]])||"All"==input[["sex_qc"]]){
                          "All"
                        } else{
                          input[["sex_qc"]]
                        }
                      })
        }else{
          sliderInput(paste0(ff,"_qc"),paste(ff),
                      min = min(get_oasis()[[ff]]),max=max(get_oasis()[[ff]]),
                      value = {if(!is.null(input[[paste0(ff,"_qc")]])){
                        input[[paste0(ff,"_qc")]]
                      }else{
                        c(min(get_oasis()[[ff]]),max(get_oasis()[[ff]]))
                      }
                      }
          )
        }
      ))
    }
    
    x <- append(x,list(selectInput("explan",label="Explanatory variable",choices = get_explan_names())))
    return(x)
  })
  
  output$ls_kon <- renderUI({
    x <- NULL
    for (ff in get_ls_fil()) {
      x <- append(x,list(
        if(as.character(ff)=="sex"){
          selectInput("sex_qc",
                      label = "sex",
                      choices = unique(append("All",c("F","M"))),
                      selected = {
                        if (is.null(input[["sex_qc"]])||"All"==input[["sex_qc"]]){
                          "All"
                        } else{
                          input[["sex_qc"]]
                        }
                      })
        }else{
          sliderInput(paste0(ff,"_qc"),paste(ff),
                      min = min(get_oasis()[[ff]]),max=max(get_oasis()[[ff]]),
                      value = {if(!is.null(input[[paste0(ff,"_qc")]])){
                        input[[paste0(ff,"_qc")]]
                      }else{
                        c(min(get_oasis()[[ff]]),max(get_oasis()[[ff]]))
                      }
                      }
          )
        }
      ))
    }
    
    x <- append(x,list(selectInput("explan",label="Explanatory variable",choices = get_explan_names())))
    return(x)
  })
  
  
  ## get the composite way 
  output$com_cd <- renderUI({
    tagList(
      radioButtons("com_way_c",label = "Central tendency",choices = c("mean","median"),inline = TRUE),
      radioButtons("com_way_d",label = "Dispersion",choices = c("SD","SEM"),selected = character(0),inline = TRUE)
    )
  })
  
  
  observeEvent(input$com_way_c,{
    output$com_cd <- renderUI({
      tagList(
        radioButtons("com_way_c",label = "Central tendency",choices = c("mean","median"),selected = input$com_way_c,inline = TRUE),
        radioButtons("com_way_d",label = "Dispersion",choices = c("SD","SEM"),selected = character(0),inline = TRUE)
      )
    })
    com_cd <<- input$com_way_c
  })
  
  observeEvent(input$com_way_d,{
    output$com_cd <- renderUI({
      tagList(
        radioButtons("com_way_c",label = "Central tendency",choices = c("mean","median"),selected = character(0),inline = TRUE),
        radioButtons("com_way_d",label = "Dispersion",choices = c("SD","SEM"),selected = input$com_way_d,inline = TRUE)
      )
    })
    com_cd <<- input$com_way_d
  })
  

  
  
  
  com_cd <<-"mean"
  aus_daten <- reactive({
    input$com_way_c
    input$com_way_d
    switch (as.vector(com_cd),
            "median" = "median",
            "mean" = "mean",
            "SD" = "sd",
            "SEM" =  "sem")
  })
  
  
  
  

  
  
  
  
  #######################################################
  ##################output OASIS table###################
  #######################################################
  output$qc_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_qc_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  output$ds_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })

  
  output$ds_composity <- DT::renderDataTable({
    data <- get_choice()
    cols <- ncol(data)
    data <- data[(cols-147):cols]
    data_mean <-apply(data, 2, mean)
    
    data_median <- apply(data, 2, median)
    data_sd <- apply(data, 2, sd)
    data_sem <- apply(data, 2, sem)
    
    frame_statistics <- tibble("thickness"=names(data),"mean"=data_mean,"median"=data_median,
                                   "SD"=data_sd,"SEM"=data_sem)
    frame_statistics[2:4] <- frame_statistics[2:4]%>%mutate_if(is.numeric,round,2)
    frame_statistics[5] <- frame_statistics[5]%>%mutate_if(is.numeric,round,4)
    
    
    DT::datatable(frame_statistics,options = list(scrolly=TRUE))
  })
  
  
  output$ss_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_ss_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  output$ls_table<- DT::renderDataTable({
    ausgewaehlte_daten <- get_ls_choice()
    DT::datatable(ausgewaehlte_daten,class = "display nowrap",options = list(scrollX=TRUE))
  })
  
  
  
  #######################################################
  ###################color update auto###################
  #######################################################  
  observeEvent({
    input$com_way_c
    input$com_way_d
    is.null(input$com_way)
    input$sidebarItemExpanded
    lapply(get_fil(), function(x){input[[x]]})
    lapply(get_fil_com(), function(x){input[[paste0(x,"_range")]]})
    input$select_hemisphere
    
  },{
    if(is.null(OASIS)){return()}  
    
    ausgewaehlte_daten <- get_choice()
    cols <- ncol(ausgewaehlte_daten)
    wert<-ausgewaehlte_daten[(cols-147):cols]
    wert <- tibble(apply(wert, 2, aus_daten()))
    
    if(aus_daten()!="sem"){
      wert <- wert%>%mutate_if(is.numeric,round,2)
    }else{
      wert <- wert%>%mutate_if(is.numeric,round,4)
    }
    
    
    max_wert<-max(wert)
    min_wert<-min(wert)
    updateNumericInput(session,"wert_obergrenze",value = max_wert)
    updateNumericInput(session, inputId = "wert_untergrenze", value = min_wert)
    
  }
)
  
  
  
  
  #######################################################
  ###################control panel tab###################
  ####################################################### 
  observeEvent(input$single_region,{
    if(input$single_region==0){
      hideTab(inputId ="tab",target = "DistributionPlot")
      
    }
    if(input$single_region==1){
      showTab(inputId ="tab",target = "DistributionPlot")
    }
  })
  
  observeEvent(input$com,{
    if(input$com==0){
      hideTab(inputId ="tab",target="Quality Control")
    }
    
    if(input$com==1){
      showTab(inputId ="tab",target="Quality Control")
    }
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
    cols <- ncol(auswahl_area)
    if(nrow(auswahl_area)==0){
      showModal(modalDialog(title = "INPUT ERROR",
                            "The inputed date is empty",
                            easyClose = TRUE))
      return(NULL)
    }
    
    
    auswahl_area <- auswahl_area[(cols-147):cols]
    if (nrow(get_choice())==1) {
      
    }else if(input$com==0){
      showModal(modalDialog(title = "INPUT ERROR",
                            "The inputed date should be one line or composite display selected",
                            easyClose = TRUE))
      return(NULL)
    }
    else{
      auswahl_area <- apply(auswahl_area, 2, aus_daten())
      # auswahl_area <- auswahl_area[1,]
      # View(auswahl_area)s
    }
    
    auswahl_data <- tibble(
      area = as.character(names(auswahl_area)),
      wert = as.numeric(auswahl_area),
      stringsAsFactors = FALSE
    )
    if(aus_daten()!="sem"){
      auswahl_data <- auswahl_data%>%mutate_if(is.numeric,round,2)
      
    }else{
      auswahl_data <- auswahl_data%>%mutate_if(is.numeric,round,4)
    }
    # auswahl_area <- t(auswahl_area)
    
    # View(auswahl_data)
    auswahl_data[[" "]] <- paste(auswahl_data$area,", Wert ist ",auswahl_data$wert)
    
    
    return(auswahl_data)
  })
  
  
  
  
  #######################################################
  ################Generate 3D brain map##################
  #######################################################
  output$ggseg3d<- renderPlotly({
    input$ab  
    if(input$ab==0)
      return()
    # isolate({
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
      if(auswahl_hemisphere=="both"){auswahl_hemisphere=c("left","right")}
      
      
      ###################################
      #############ggseg3d###############
      ###################################
      
      gg <<- ggseg3d(.data = auswahl_data,
                     atlas = get_altes(),
                     colour = "wert", text = " ",
                     surface = "LCBC",
                     palette = sort(auswahl_wert),
                     hemisphere = auswahl_hemisphere,
                     na.alpha= .5,
                     show.legend = TRUE,
                     options.legend = (colorbar=list(title=list(text="mm")))) %>%
        pan_camera("left lateral") %>% 
        event_register("plotly_relayout") %>% 
        layout(scene=list(camera=list(center=list(z=-0.4))))%>%
        remove_axes()
      
      
      filter_data <<- NULL
      if(input$com==0){
        for (i in input$fil) {
          filter_data <<- paste(filter_data,i,":",input[[i]],"\n",sep=" ")
        }
      }else{
        for (i in input$fil_com) {
          i_range=paste(i,"_range",sep="")
          if(i_range=="sex_range"){
            filter_data <<- paste(filter_data,"sex: ",input[[i_range]],"\n",sep = "")
          }else{
            
            filter_data <<- paste(filter_data,i,": ","[",min(input[[i_range]]),",",max(input[[i_range]]),"]","\n",sep="")
          }
        }
      }
      if(is.null(filter_data)){
        gg
      }else{
        gg%>%layout(annotations=list(visible=TRUE,
                                     text=filter_data,
                                     showarrow=FALSE,
                                     x=0,y=1,
                                     align="left",
                                     font=list(family="Arial",size=13)))
        
      }
      
    # })
  })
  
  
  
  observeEvent(input$ab,{
    updateTabsetPanel(session,"ds_tab",selected = "3D")
  })
  
  ###########################################
  #####################orca##################
  ###########################################
  get_eye <- reactive({
    d <- event_data("plotly_relayout")
    if(input$ab==0||is.null(d)){
      return(NULL)
    }else {
      return(d$scene.camera[["eye"]])
    }
  })
  
  
  
  
  #p<-output$ggseg3d
  observeEvent(input$download,{
    if(is.null(get_eye())){
      if(isFALSE(input$down_filter)){
        orca(gg, paste(input$name,input$format, sep = ".", collapse = NULL))
      }else{
        orca(gg%>%layout(annotations=list(visible=TRUE,text=filter_data,showarrow=FALSE,x=0,y=1,align="left",font=list(family="Arial",size=13))),
             paste(input$name,input$format, sep = ".", collapse = NULL))
      }
    }else{
      eye <- get_eye()
      scene=list(camera=list(eye=list(x=eye$x,y=eye$y,z=eye$z)))
      # gg_eye <- gg%>%layout(scene=scene)
      if(isFALSE(input$down_filter)){
        orca(gg%>%layout(scene=scene),paste(input$name,input$format, sep = ".", collapse = NULL))
      }else{
        gg_eye <- gg%>%layout(scene=scene,annotations=list(visible=TRUE,text=filter_data,showarrow=FALSE,x=0,y=1,align="left",font=list(family="Arial",size=13)))
        orca(gg_eye,paste(input$name,input$format, sep = ".", collapse = NULL))
      }
    }
  })
  
  
  
  
  
  ###########################################
  #####distributionPlot when single person###
  ###########################################
  output$distributionPlot<-renderPlot({
    if(input$single_region==0)
      return()
    # if(input$ab==0)
    #   return()
    if(is.null(input[["ID"]])){}
    isolate({
      region<-input$region
      auswahl_area <- get_choice()
      if(nrow(get_choice())!=1){
        return()
      }
      
      auswahl_region <- input$region
      oasis_data <- get_oasis()[-1:-3]
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
  
  
  ###########################################
  ##########ouput Quality Control############
  ###########################################
  
  observeEvent(input$dp,{
    updateTabsetPanel(session,"qc_tab","Quality Raincloud")
  })
  
  observeEvent(input$rp,{
    updateTabsetPanel(session,"ss_tab","Regression Plots")
  })
  
  
  
  
  output$quality <- renderPlot({
    if(is.null(input$dp) || input$dp==0){return(NULL)}
    
    isolate({
      data <- get_qc_choice()
      cols <- ncol(data)
      data <- data[(cols-147):cols]
      name_level <- names(data)
      data <- melt(data)
      names(data) <- c("area","thickness")
      data$lr <- substr(data$area,1,1)
      data[which(data$lr=='l'|data$lr=="L"),]$lr <- "left"
      data[which(data$lr=='r'|data$lr=="R"),]$lr <- "right"
      data$lr <- as.factor(data$lr)
      
      ##format the thickness area names the rearrange the factor levels
      if(!is.null(input$name_file)){
        name_level <- rev(unique(substring(name_level,2)))
        data$area <- factor(x=substring(data$area,2),levels=name_level,ordered = TRUE)
      }else{
        name_level <- rev(unique(substring(name_level,4)))
        data$area <- factor(x=substring(data$area,4),levels=name_level,ordered = TRUE)
      }
      
      p <- ggplot(data,aes(x=area,y=thickness,fill=area))+
        geom_flat_violin(position=position_nudge(x=0.2,y=0),adjust=1,trim = TRUE)+
        geom_point(position = position_jitter(width=.1),size=.2,aes(color=area),show.legend = FALSE)+
        geom_boxplot(aes(x=as.numeric(area)+0.2,y=thickness),outlier.shape = NA,alpha=0.3,width=0.1,color="BLACK")+
        coord_flip()+
        facet_wrap(~lr)+
        theme_cowplot()+
        guides(fill=FALSE)
      p
    })
  })
  
  
  output$regression <- renderPlot({
      data <- get_ss_choice()
      cols <- ncol(data)
      var_explan <- as.character(input$explan)
      p <- add_lm_trace(data,var_explan)
      
  })
  
  
  lasso_training_results <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),alpha=1,normalise=T){
    taco <- target.column
    if (!is.numeric(taco)){
      taco <- grep(target.column,colnames(dat))
    }
    x_vars <- as.matrix(dat[,-taco])
    y_var <- dat[,taco]
    y.mean <- 0
    y.sd <- 1
    if (normalise){
      x_vars <- scale(x_vars)
      y.mean <- mean(y_var,na.rm=T)
      y.sd <- sd(y_var,na.rm=T)
      y_var <- (y_var - y.mean)/y.sd
    }
    
    cv_output <- cv.glmnet(x_vars,y_var,alpha=alpha,lambda=lambda_seq)
    best_lam <- cv_output$lambda.min
    lasso_best <- glmnet(x_vars,y_var,alpha=alpha,lambda=best_lam)
    predictions <- predict(lasso_best,s=best_lam,newx=x_vars)
    if (normalise){
      predictions <- y.mean + y.sd*predictions
    }
    return(list(lambda=best_lam,coefficients=coef(lasso_best)[,1],predictions=predictions))
  }
  lasso_bootstrap <- function(dat,target.column,lambda_seq=10^seq(2,-2,by = -.1),alpha=1,normalise=T,n.bootstrap=10){
    
    lambda <- rep(0,n.bootstrap)
    coefficient.matrix <- matrix(0,nrow=n.bootstrap,ncol=ncol(dat)+1)
    colnames(coefficient.matrix) <- c("lambda","intercept",colnames(dat)[!names(dat) %in% target.column,drop=F])
    for (i in 1:n.bootstrap){
      inds <- sample(nrow(dat),nrow(dat),replace=T)
      lassi <- lasso_training_results(dat[inds,],target.column,lambda_seq=lambda_seq,alpha=alpha,normalise=normalise)
      coefficient.matrix[i,] <- c(lassi$lambda,lassi$coefficients)
    }
    return(coefficient.matrix)
  }
  prop.nonzero <- function(x){
    return(sum(!is.na(x) & x!=0)/sum(!is.na(x)))
  }
  get.proportion.of.nonzero.coeffcients <- function(coefficient.matrix){
    return(apply(coefficient.matrix,2,prop.nonzero))
  }
  sign.consistency <- function(x){
    return(sum(!is.na(x) & x>=0)==sum(!is.na(x)) | sum(!is.na(x) & x<=0)==sum(!is.na(x)))
  }
  get.sign.consistency <- function(coefficient.matrix){
    return(apply(coefficient.matrix,2,sign.consistency))
  }
  
  
  
  
  
}