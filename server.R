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
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
# source("title_fun.R")
file.source=list.files("ggseg3d\\R",pattern="*.R",full.names = TRUE)
lapply(file.source, source,.GlobalEnv)




server<-function(input, output,session) {
  OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
  OASIS[-1:-2] <- apply(OASIS[-1:-2],2,as.numeric)
  id_sex_age <- OASIS[,1:3]
  
  
  
  
  ######################################data preprocess######################################### 
  
  
  
  
  
  #######################################################
  #############transform the names of OASIS##############
  #######################################################
  
  
  get_oasis <- reactive({
    if(!is.null(input$name_file)){
      area <- read_excel(input$name_file[["datapath"]],col_names = FALSE)
      oasis_r <- OASIS[-1:-3]
      n <- names(oasis_r)
      
      nt <- sub("lh","",n)
      nt <- sub("rh","",nt)
      nt <- sub("thickness","",nt)
      nt <- gsub("_","",nt)
      nt <- gsub("-","",nt)
      o_table <- tibble(names(oasis_r),nt,seq(1:length(names(oasis_r))))
      names(o_table) <- c("o_names","pattern","name_seq")
      
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
      
      names(OASIS)[-1:-3] <- pattern
      
      oasis_data <- OASIS
      
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
  
  
  
  
  #######################################################
  ######make the names pass to left and right brain######
  #######################################################
  # if(is.null(desterieux_neu)){ }
  # load desterieux_3d
  get_altes <- reactive({
    desterieux_neu<-desterieux_3d
    t_name <- names(get_oasis()[-1:-3])
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
  #filter the data from according to selected condition##
  #######################################################
  get_choice <- reactive({    # get the select col and return the selected date
    aa <- input$name_file
    col_input <- get_fil()
    col_com_input <- get_fil_com()
    u_oasis <- get_oasis()
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
  
  
  ## get the region names after filter
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
  
  
  #######################################################
  ##########output the ui from the selected col##########
  #######################################################
  output$kon <- renderUI({     # ouput the select UI
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
      x <- append(x,list(actionButton("dp","Distribution Plots")))
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
            sliderInput(paste0(ff,"_range"),paste(ff,"Range"),
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
      x <- append(x,list(radioButtons("com_way","Descriptive Statistics",
                                       choices = c("median","mean","SD"),
                                       selected = {
                                         if(is.null(input$com_way)){"median"}
                                         else{input[["com_way"]]}
                                       },
                                       inline = TRUE)))
      
      
    }
    return(x)
  })
  
  ## get the composite way 
  aus_daten <- reactive({  
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
    if(!is.null(input$name_file)){}
    lapply(get_fil(), function(x){input[[x]]!=""})
    lapply(get_fil_com(), function(x){input[[paste0(x,"_range")]]!=0})
    if (input$com) {}
    if(is.null(input$com_way)){}
    input$select_hemisphere
    if(is.null(get_fil())&&is.null(get_fil_com())){return()}
    isolate({
      
      ausgewaehlte_daten <- get_choice()
      DT::datatable(ausgewaehlte_daten,class = "display nowrap")
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
    ##change the color boundary automatically
    ausgewaehlte_daten <- get_choice()
    wert<-ausgewaehlte_daten[-1:-3]
    if (!is.null(input$com_way)) {
      wert[1,] <- apply(wert, 2, aus_daten())
    }
    wert <- wert%>%mutate_if(is.numeric,round,2)
    max_wert<-max(wert)
    min_wert<-min(wert)
    updateNumericInput(session, "wert_obergrenze", value = max_wert)
    updateNumericInput(session, "wert_untergrenze", value = min_wert)
  })
  
  
  
  
  
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
    if(nrow(auswahl_area)==0){
      showModal(modalDialog(title = "INPUT ERROR",
                            "The inputed date is empty",
                            easyClose = TRUE))
      return(NULL)
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
    auswahl_area <- auswahl_area%>%mutate_if(is.numeric,round,2)
    
    auswahl_area <- t(auswahl_area)
    auswahl_data <- data.frame(
      area = as.character(row.names(auswahl_area)),
      wert = auswahl_area[,1],
      stringsAsFactors = FALSE
    )
    auswahl_data[[" "]] <- paste(get_region_names(),", Wert ist ",auswahl_data$wert)
    
    
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
      
    })
  })
  
  
  
  observeEvent(input$ab,{
    updateTabsetPanel(session,"tab",selected = "3D")
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
    updateTabsetPanel(session,"tab","Quality Control")
  })
  
  
  

  output$quality <- renderPlot({
    if(is.null(input$dp) || input$dp==0){return(NULL)}
    
    isolate({
      
      data <- get_choice()
      data <- data[-1:-3]
      
      data <- melt(data)
      names(data) <- c("area","thickness")
      data$lr <- substr(data$area,1,1)
      data[which(data$lr=='l'|data$lr=="L"),]$lr <- "left"
      data[which(data$lr=='r'|data$lr=="R"),]$lr <- "right"
      data$lr <- as.factor(data$lr)
      if(!is.null(input$name_file)){
        data$area <- ordered(substring(data$area,2),rev(unique(substring(data$area,2))))
      }else{
        data$area <- ordered(substring(data$area,4),rev(unique(substring(data$area,4))))
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
  
  
  
  
  
}