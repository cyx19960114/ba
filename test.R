library(shiny)
library(readxl)
OASIS <- read_excel("OASIS.xlsx",col_types = c("text"))
OASIS[-1:-2] <- apply(OASIS[-1:-2],2,as.numeric)

if (interactive()) {
  # Define UI
  ui <- fluidPage(
    selectInput("fil",label = "Filtern",
                choices = names(OASIS),multiple = TRUE),
    
    uiOutput("kon"),
    
    DT::dataTableOutput("table") 
    
  )
  
  # Server logic
  server <- function(input, output, session) {
    get_fil <- reactive({
      input$fil
    })
    
    u_oasis <- OASIS
    
    get_choice <- reactive({
      col_input <- get_fil()
      for (col in col_input) {
        v <- input[[col]]
        if(!(is.null(v)|| ""==v)){
          u_oasis <- u_oasis[u_oasis[[col]]==v,]
          
        }
      }
      return(u_oasis)
    })
    
    observe(print(get_fil()))
    
    output$kon <- renderUI({
      x <- vector("list",length=length(get_fil()))
      for (ff in get_fil()) {
        print(min(get_choice()[[ff]]))
        x <- append(x,
                    list(
                      selectInput(inputId = paste0(ff),
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
                    )
        )
        
      }
      return(x)
    }
    )
    
    output$table <- DT::renderDataTable({
      if(is.null(get_fil())||!(TRUE%in%lapply(get_fil(), function(x){
        input[[x]]!=""
      })))
        return()
      isolate({
        DT::datatable(get_choice())
      })
    })
    
    
  }
  
  shinyApp(ui, server)
}
