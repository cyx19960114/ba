library(shiny)
library(ggseg)
library(dplyr)
library(tidyr)
library(ggseg3d)
library(ggsegExtra)
library(plotly)

server<-function(input, output) {
  datasetInput<-reactive({
    switch(input$dataset,
           "Herr"= Herr,"Frau"= Frau)
  })
  
  datasetInput1<-reactive({
    switch(input$dataset1,
           "Herr"= Herr,"Frau"= Frau)
  })

  
  
  output$ggseg3d<- renderPlotly({  ggseg3d(datasetInput(),
                                   atlas = desterieux_3d,
                                   colour = "wert", text = "wert",
                                   palette = c("#ff0000", "#00ff00", "#0000ff"),
                                   hemisphere = "left",
                                   na.alpha= .5)})
  
  output$ggseg3d1<- renderPlotly({ ggseg3d(datasetInput1(),
                                   atlas = desterieux_3d,
                                   colour = "wert", text = "wert",
                                   palette = c("#ff0000", "#00ff00", "#0000ff"),
                                   hemisphere = "left",
                                   na.alpha= .5)})

  
}
