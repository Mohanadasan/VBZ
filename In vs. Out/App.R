#bitte zuerst ausf�hren
library(readxl)
library(reshape2)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)
In_Out_reshape <- read.csv("In_Out_reshape.csv")

########## Grafik In vs. Out ##########

#Zeile 15 - 57 markieren & ausf�hren
#Datum vor 16.06.21 w�hlen & kurz warten

ui = fluidPage(
  titlePanel("Flaschenhals identifizieren: In vs. Out"),
  selectInput(inputId = "Z�hllinie",
              label = "W�hle Z�hllinie:",
              choices = c("Ost-S�d", "West-S�d", "Ost-Nord", "Ost-SBB", "Ost-VBZ", "West-Nord", "West-SBB", "West-VBZ"),
              selected = "Ost-S�d",
              multiple = TRUE
  ), 
  dateInput(inputId = "Datum",
            label = "W�hle Datum (vor 16.06.21):",
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 0, 
            language = "de"),
  plotOutput("plot"), width = 40,
  textOutput("text"))




server = function(input, output) {
  
  selected <- reactive({
    In_Out_reshape <- In_Out_reshape[In_Out_reshape$Name == input$Z�hllinie,]
    In_Out_reshape <- In_Out_reshape[In_Out_reshape$Date == input$Datum,]
    
    
    
  })
  
  
  
  output$plot <- renderPlot({
    ggplot(selected()) + geom_line(aes(x = Time_num, y = value, colour = variable)) + scale_fill_discrete(labels = c("In", "Out")) + scale_x_continuous(breaks = seq(0,24,by=2)) + theme(axis.line = element_line(), text = element_text(size = 15), panel.background = element_rect(fill = "white"), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + labs (x = "Uhrzeit", y = "In vs. Out")
    
    
  })
  
  output$text <- renderText({"Erkl�rung: In dieser Grafik wird der Vergleich der Variablen 'In' & 'Out' f�r die Z�hllinien der VBZ Haltestelle Bahnhof Hardbr�cke an einem bestimmten Tag dargestellt. Besonders zu Stosszeiten an Wochentagen ist ersichtlich, dass der In-Flow h�her als der Out-Flow ist und somit ein Flaschenhals entstehen kann."})
  
}  

shinyApp(ui = ui, server)








