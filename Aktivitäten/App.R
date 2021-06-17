#bitte zuerst ausführen
library(readxl)
library(reshape2)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)
aktivitäten <- read.csv("aktivitäten.csv")
aktivitäten$Date <- as.Date(aktivitäten$Date)

########## Grafik Aktivitäten ##########

#Zeile 15 - 48 markieren & ausführen

ui = fluidPage(
  titlePanel("Gesamtaktivitäten pro Tag"),
  selectInput(inputId = "Zähllinie",
              label = "Wähle Zähllinie:",
              choices = c("Ost-Süd", "West-Süd", "Ost-Nord", "Ost-SBB", "Ost-VBZ", "West-Nord", "West-SBB", "West-VBZ"),
              selected = "Ost-Süd",
              multiple = TRUE
  ),
  plotOutput("plot"), width = 40,
  textOutput("text")
)



server = function(input, output) {
  
  selected <- reactive({
    aktivitäten[aktivitäten$Name == input$Zähllinie,]
  })
  
  
  
  output$plot <- renderPlot({
    ggplot(selected(), aes(x= Date , y=sum, group = Jahr, colour = factor(Jahr))) +
      geom_line() + theme(axis.line = element_line(), text = element_text(size = 15), panel.background = element_rect(fill = "white"), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
      labs (x = "Monat", y = "Zähllinie")
  }, width = "auto", height = "auto")
  
  
  output$text <- renderText({"Erklärung: In dieser Grafik wird die Aktivität an den Zähllinien der VBZ Haltestelle Bahnhof Hardbrücke dargestellt. Dabei wird die Aktivität als die Summe der Variablen 'In' & 'Out' definiert. Durch die Wahl der Zähllinie im Feld wird der Vergleich der Gesamtaktivitäten (pro Tag) in den Jahren 2020 und 2021 (bis Mitte Juni) an dieser Zähllinie angestossen. Besonders auffällig ist der Fall der Aktivitäten an allen Zähllinien im März 2020, vermutlich verursacht durch die Erklärung der ausserordentlichen Lage durch den Bundesrat und den daher eingehenden Massnahmen."})
  
}

shinyApp(ui = ui, server)