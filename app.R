library(shiny)
library(ggplot2)
datasolidegroei <- read.csv("SOLIDEGROEI_WAARDEN.csv", sep=';',header=TRUE)
datasolidegroei[is.na(datasolidegroei)] <- 0
num_col <- ncol(datasolidegroei)
waarden <- names(datasolidegroei[,2:(num_col-1)])
labnummers <- datasolidegroei[1]
maximum <- max(as.numeric(datasolidegroei$Aantal.records), na.rm=TRUE)
ui <- fluidPage(
  titlePanel("Spiegelinfo"),
  sidebarLayout(sidebarPanel(selectInput(inputId = 'waarde', "Waarden solide groei",waarden),
                             selectInput(inputId = 'lab', "Labs", labnummers),
                             sliderInput(inputId ='records','Minimaal aantal records',
                                         min=1,max=maximum,value=0, animate=TRUE ),
                             fluidRow(column(width=6, 
                                             h4("Geselecteerd lab"),
                                             verbatimTextOutput('click_info'))),
                             width=8
  ),
  
  
  mainPanel(plotOutput('plot', click = "plot_click"),
            tableOutput('table')
            
  )
  
  
  ) 
  
)
library(shiny)
library(ggplot2)
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    datasolidegroei <- datasolidegroei[datasolidegroei$Aantal.records >= input$records,]
    datasolidegroei$"percentage" <- (datasolidegroei[,input$waarde]/datasolidegroei[,"Aantal.records"])
    datasolidegroei[,"mean"] <- sum(datasolidegroei[,input$waarde])/sum(datasolidegroei[,"Aantal.records"])
    datasolidegroei[,"standard error"] <-sqrt( (datasolidegroei[,"mean"] * (1-mean(datasolidegroei[,"mean"])) /datasolidegroei[,"Aantal.records"] ) ) 
    datasolidegroei[,"upper limit"] <- datasolidegroei[,"mean"] + 2 * datasolidegroei[,"standard error"]
    datasolidegroei[,"lower limit"] <- datasolidegroei[,"mean"] - 2 * datasolidegroei[,"standard error"]
    datasolidegroei <- datasolidegroei[order(datasolidegroei$Aantal.records,decreasing= FALSE),]
    datasolidegroei[,"lablabel"] <- ifelse(datasolidegroei$Labnummer==input$lab, "Uw lab", "Overige labs" ) 
    
    ggplot(datasolidegroei, aes(x=Aantal.records,y=percentage,color=lablabel)) + 
      xlab("Aantal.records")+
      ylab(paste("Percentage ", input$waarde))+
      geom_point() + 
      geom_hline(yintercept = datasolidegroei$mean)+
      geom_line(data=datasolidegroei, aes(x=Aantal.records,y=datasolidegroei$`upper limit`))+
      geom_line(data=datasolidegroei, aes(x=Aantal.records,y=datasolidegroei$`lower limit`))+
      labs(title="Spiegelinformatie item Solide groei tumor", color="Geselecteerd")
  }
  )
  
  output$click_info <- renderPrint({
    
    datasolidegroei <- datasolidegroei[datasolidegroei$Aantal.records >= input$records,]
    datasolidegroei$"percentage" <- (datasolidegroei[,input$waarde]/datasolidegroei[,"Aantal.records"])
    datasolidegroei[,"mean"] <- sum(datasolidegroei[,input$waarde])/sum(datasolidegroei[,"Aantal.records"])
    datasolidegroei[,"standard error"] <-sqrt( (datasolidegroei[,"mean"] * (1-mean(datasolidegroei[,"mean"])) /datasolidegroei[,"Aantal.records"] ) ) 
    datasolidegroei[,"upper limit"] <- datasolidegroei[,"mean"] + 2 * datasolidegroei[,"standard error"]
    datasolidegroei[,"lower limit"] <- datasolidegroei[,"mean"] - 2 * datasolidegroei[,"standard error"]
    datasolidegroei <- datasolidegroei[order(datasolidegroei$Aantal.records,decreasing= FALSE),]
    datasolidegroei[,"lablabel"] <- ifelse(datasolidegroei$Labnummer==input$lab, "Uw lab", "Overige labs" ) 
    
    datasolidegroei_info <- datasolidegroei[c("Labnummer","Aantal.records", "percentage")]
    
    nearPoints(datasolidegroei_info, input$plot_click)
  })
  
  output$table <- renderTable(
    datasolidegroei[datasolidegroei$Aantal.records >= input$records,],
    colnames = TRUE,
    digits = 0)
}

shinyApp(ui = ui, server = server)
