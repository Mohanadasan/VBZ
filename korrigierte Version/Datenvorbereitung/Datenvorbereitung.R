library(readxl)
library(reshape2)
library(dplyr)
library(shiny)
library(ggplot2)
library(lubridate)
frequenzen_2020 <- read.csv("frequenzen_hardbruecke_2020.csv")
View(frequenzen_2020)

frequenzen_2021 <- read.csv("frequenzen_hardbruecke_2021.csv")
View(frequenzen_2021)


#Daten vorbereiten: ü richtig formatieren
frequenzen_2020$Name <- recode_factor(frequenzen_2020$Name, "Ost-SÃ¼d total"="Ost-Süd total", "West-SÃ¼d total"="West-Süd total")
levels(frequenzen_2020$Name)
frequenzen_2021$Name <- recode_factor(frequenzen_2021$Name, "Ost-SÃ¼d total"="Ost-Süd total", "West-SÃ¼d total"="West-Süd total")
levels(frequenzen_2021$Name)

#Daten vorbereiten: total raus 
frequenzen_2020$Name <- recode_factor(frequenzen_2020$Name, "Ost-Süd total"="Ost-Süd", "West-Süd total"="West-Süd", "Ost-Nord total"="Ost-Nord", "Ost-SBB total" = "Ost-SBB", "Ost-VBZ Total" = "Ost-VBZ", "West-Nord total" = "West-Nord", "West-SBB total" = "West-SBB", "West-VBZ total" = "West-VBZ")
levels(frequenzen_2020$Name)
frequenzen_2021$Name <- recode_factor(frequenzen_2021$Name, "Ost-Süd total"="Ost-Süd", "West-Süd total"="West-Süd", "Ost-Nord total"="Ost-Nord", "Ost-SBB total" = "Ost-SBB", "Ost-VBZ Total" = "Ost-VBZ", "West-Nord total" = "West-Nord", "West-SBB total" = "West-SBB", "West-VBZ total" = "West-VBZ")
levels(frequenzen_2021$Name)

#Spalte Timestamp trennen
frequenzen_2020$Timestamp <- colsplit(frequenzen_2020$Timestamp, "T", c("Date", "Time"))
head(frequenzen_2020)
frequenzen_2020 <- cbind(frequenzen_2020, frequenzen_2020$Timestamp)
frequenzen_2020$Timestamp <- NULL
frequenzen_2021$Timestamp <- colsplit(frequenzen_2021$Timestamp, "T", c("Date", "Time"))
head(frequenzen_2021)
frequenzen_2021 <- cbind(frequenzen_2021, frequenzen_2021$Timestamp)
frequenzen_2021$Timestamp <- NULL


########## Transformation für Grafik Aktivitäten ##########

#Transformation: Aktivität = In + Out
aktivitäten_2020 <- frequenzen_2020
aktivitäten_2021 <- frequenzen_2021

aktivitäten_2020$Aktivität <- aktivitäten_2020$In + aktivitäten_2020$Out
head(aktivitäten_2020)
aktivitäten_2021$Aktivität <- aktivitäten_2021$In + aktivitäten_2021$Out
head(aktivitäten_2021)

#Summe der Aktivität pro Tag, gruppiere per Zähllinie
aktivitäten_2020_sum <- aktivitäten_2020 %>% group_by(aktivitäten_2020$Name, aktivitäten_2020$Date) %>% summarise(sum = sum(Aktivität))
head(aktivitäten_2020_sum)
aktivitäten_2021_sum <- aktivitäten_2021 %>% group_by(aktivitäten_2021$Name, aktivitäten_2021$Date) %>% summarise(sum = sum(Aktivität))
head(aktivitäten_2021_sum)

aktivitäten_2020_sum$Name <- aktivitäten_2020_sum$`aktivitäten_2020$Name`
aktivitäten_2020_sum$`aktivitäten_2020$Name` <- NULL
aktivitäten_2020_sum$Date <- aktivitäten_2020_sum$`aktivitäten_2020$Date`
aktivitäten_2020_sum$`aktivitäten_2020$Date` <- NULL
aktivitäten_2021_sum$Name <- aktivitäten_2021_sum$`aktivitäten_2021$Name`
aktivitäten_2021_sum$`aktivitäten_2021$Name` <- NULL
aktivitäten_2021_sum$Date <- aktivitäten_2021_sum$`aktivitäten_2021$Date`
aktivitäten_2021_sum$`aktivitäten_2021$Date` <- NULL

#Datum transformieren 
aktivitäten_2020_sum$Date <- ymd(aktivitäten_2020_sum$Date)
str(aktivitäten_2020_sum$Date)
aktivitäten_2021_sum$Date <- ymd(aktivitäten_2021_sum$Date)
head(aktivitäten_2021_sum)

#Jahr extrahieren
aktivitäten_2020_sum$Year <- year(aktivitäten_2020_sum$Date)
aktivitäten_2021_sum$Year <- year(aktivitäten_2021_sum$Date)

#beide Datensätze vereinen
aktivitäten <- rbind(aktivitäten_2020_sum, aktivitäten_2021_sum)
head(aktivitäten)

#Jahr überall auf 2020 setzen, damit auf x-Achse nur Daten abgebildet sind ohne Jahre
year(aktivitäten$Date) <- 2020
aktivitäten$Jahr <- aktivitäten$Year
aktivitäten$Year <- NULL

#Daten speichern
write.csv(aktivitäten,"aktivitäten.csv", row.names = FALSE)


########## Transformation für Grafik In vs. Out ##########

In_Out_2020 <- frequenzen_2020
In_Out_2021 <- frequenzen_2021

#Datum transformieren 
In_Out_2020$Date <- ymd(In_Out_2020$Date)
str(In_Out_2020$Date)
In_Out_2021$Date <- ymd(In_Out_2021$Date)
head(In_Out_2021)

#beide Datensätze vereinen
In_Out <- rbind(In_Out_2020, In_Out_2021)
head(In_Out)

#Time als Nummer formatieren
In_Out$Time_num <- as.character(In_Out$Time)
In_Out$Time_num <- gsub(":",".",In_Out$Time_num)
head(In_Out)
In_Out$Time_num <- substr(In_Out$Time_num,1,5)
In_Out$Time_num <- as.numeric(In_Out$Time_num)
str(In_Out$Time_num)
head(In_Out)
In_Out$Time <- NULL

#In und Out in eine Kolonne transformieren
In_Out_reshape <- melt(In_Out, id=c("Name", "Date", "Time_num"))
head(In_Out_reshape)

#Daten speichern
write.csv(In_Out_reshape,"In_Out_reshape.csv", row.names = FALSE)



########## Grafik Aktivitäten ##########

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


########## Grafik In vs. Out ##########

ui = fluidPage(
  titlePanel("Flaschenhals identifizieren: In vs. Out"),
  selectInput(inputId = "Zähllinie",
                  label = "Wähle Zähllinie:",
                  choices = c("Ost-Süd", "West-Süd", "Ost-Nord", "Ost-SBB", "Ost-VBZ", "West-Nord", "West-SBB", "West-VBZ"),
                  selected = "Ost-Süd",
                  multiple = TRUE
      ), 
      dateInput(inputId = "Datum",
                label = "Wähle Datum (vor 16.06.21):",
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0, 
                language = "de"),
      plotOutput("plot"), width = 40,
  textOutput("text"))




server = function(input, output) {
  
  selected <- reactive({
    In_Out_reshape <- In_Out_reshape[In_Out_reshape$Name == input$Zähllinie,]
    In_Out_reshape <- In_Out_reshape[In_Out_reshape$Date == input$Datum,]
    
    
    
  })
  
  
  
  output$plot <- renderPlot({
    ggplot(selected()) + geom_line(aes(x = Time_num, y = value, colour = variable)) + scale_fill_discrete(labels = c("In", "Out")) + scale_x_continuous(breaks = seq(0,24,by=2)) + theme(axis.line = element_line(), text = element_text(size = 15), panel.background = element_rect(fill = "white"), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) + labs (x = "Uhrzeit", y = "In vs. Out")
    
    
  })
  
  output$text <- renderText({"Erklärung: In dieser Grafik wird der Vergleich der Variablen 'In' & 'Out' für die Zähllinien der VBZ Haltestelle Bahnhof Hardbrücke an einem bestimmten Tag dargestellt. Besonders zu Stosszeiten an Wochentagen ist ersichtlich, dass der In-Flow höher als der Out-Flow ist und somit ein Flaschenhals entstehen kann."})
  
}  

shinyApp(ui = ui, server)








