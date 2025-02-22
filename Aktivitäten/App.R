#bitte zuerst ausf�hren
library(readxl)
library(reshape2)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)
aktivit�ten <- read.csv("aktivit�ten.csv")
aktivit�ten$Date <- as.Date(aktivit�ten$Date)

########## Grafik Aktivit�ten ##########

#Zeile 15 - 48 markieren & ausf�hren

ui = fluidPage(
  titlePanel("Gesamtaktivit�ten pro Tag"),
  selectInput(inputId = "Z�hllinie",
              label = "W�hle Z�hllinie:",
              choices = c("Ost-S�d", "West-S�d", "Ost-Nord", "Ost-SBB", "Ost-VBZ", "West-Nord", "West-SBB", "West-VBZ"),
              selected = "Ost-S�d",
              multiple = TRUE
  ),
  plotOutput("plot"), width = 40,
  textOutput("text")
)



server = function(input, output) {
  
  selected <- reactive({
    aktivit�ten[aktivit�ten$Name == input$Z�hllinie,]
  })
  
  
  
  output$plot <- renderPlot({
    ggplot(selected(), aes(x= Date , y=sum, group = Jahr, colour = factor(Jahr))) +
      geom_line() + theme(axis.line = element_line(), text = element_text(size = 15), panel.background = element_rect(fill = "white"), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
      labs (x = "Monat", y = "Z�hllinie")
  }, width = "auto", height = "auto")
  
  
  output$text <- renderText({"Erkl�rung: In dieser Grafik wird die Aktivit�t an den Z�hllinien der VBZ Haltestelle Bahnhof Hardbr�cke dargestellt. Dabei wird die Aktivit�t als die Summe der Variablen 'In' & 'Out' definiert. Durch die Wahl der Z�hllinie im Feld wird der Vergleich der Gesamtaktivit�ten (pro Tag) in den Jahren 2020 und 2021 (bis Mitte Juni) an dieser Z�hllinie angestossen. Besonders auff�llig ist der Fall der Aktivit�ten an allen Z�hllinien im M�rz 2020, vermutlich verursacht durch die Erkl�rung der ausserordentlichen Lage durch den Bundesrat und den daher eingehenden Massnahmen."})
  
}

shinyApp(ui = ui, server)