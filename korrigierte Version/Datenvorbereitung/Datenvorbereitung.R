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


#Daten vorbereiten: � richtig formatieren
frequenzen_2020$Name <- recode_factor(frequenzen_2020$Name, "Ost-Süd total"="Ost-S�d total", "West-Süd total"="West-S�d total")
levels(frequenzen_2020$Name)
frequenzen_2021$Name <- recode_factor(frequenzen_2021$Name, "Ost-Süd total"="Ost-S�d total", "West-Süd total"="West-S�d total")
levels(frequenzen_2021$Name)

#Daten vorbereiten: total raus 
frequenzen_2020$Name <- recode_factor(frequenzen_2020$Name, "Ost-S�d total"="Ost-S�d", "West-S�d total"="West-S�d", "Ost-Nord total"="Ost-Nord", "Ost-SBB total" = "Ost-SBB", "Ost-VBZ Total" = "Ost-VBZ", "West-Nord total" = "West-Nord", "West-SBB total" = "West-SBB", "West-VBZ total" = "West-VBZ")
levels(frequenzen_2020$Name)
frequenzen_2021$Name <- recode_factor(frequenzen_2021$Name, "Ost-S�d total"="Ost-S�d", "West-S�d total"="West-S�d", "Ost-Nord total"="Ost-Nord", "Ost-SBB total" = "Ost-SBB", "Ost-VBZ Total" = "Ost-VBZ", "West-Nord total" = "West-Nord", "West-SBB total" = "West-SBB", "West-VBZ total" = "West-VBZ")
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


########## Transformation f�r Grafik Aktivit�ten ##########

#Transformation: Aktivit�t = In + Out
aktivit�ten_2020 <- frequenzen_2020
aktivit�ten_2021 <- frequenzen_2021

aktivit�ten_2020$Aktivit�t <- aktivit�ten_2020$In + aktivit�ten_2020$Out
head(aktivit�ten_2020)
aktivit�ten_2021$Aktivit�t <- aktivit�ten_2021$In + aktivit�ten_2021$Out
head(aktivit�ten_2021)

#Summe der Aktivit�t pro Tag, gruppiere per Z�hllinie
aktivit�ten_2020_sum <- aktivit�ten_2020 %>% group_by(aktivit�ten_2020$Name, aktivit�ten_2020$Date) %>% summarise(sum = sum(Aktivit�t))
head(aktivit�ten_2020_sum)
aktivit�ten_2021_sum <- aktivit�ten_2021 %>% group_by(aktivit�ten_2021$Name, aktivit�ten_2021$Date) %>% summarise(sum = sum(Aktivit�t))
head(aktivit�ten_2021_sum)

aktivit�ten_2020_sum$Name <- aktivit�ten_2020_sum$`aktivit�ten_2020$Name`
aktivit�ten_2020_sum$`aktivit�ten_2020$Name` <- NULL
aktivit�ten_2020_sum$Date <- aktivit�ten_2020_sum$`aktivit�ten_2020$Date`
aktivit�ten_2020_sum$`aktivit�ten_2020$Date` <- NULL
aktivit�ten_2021_sum$Name <- aktivit�ten_2021_sum$`aktivit�ten_2021$Name`
aktivit�ten_2021_sum$`aktivit�ten_2021$Name` <- NULL
aktivit�ten_2021_sum$Date <- aktivit�ten_2021_sum$`aktivit�ten_2021$Date`
aktivit�ten_2021_sum$`aktivit�ten_2021$Date` <- NULL

#Datum transformieren 
aktivit�ten_2020_sum$Date <- ymd(aktivit�ten_2020_sum$Date)
str(aktivit�ten_2020_sum$Date)
aktivit�ten_2021_sum$Date <- ymd(aktivit�ten_2021_sum$Date)
head(aktivit�ten_2021_sum)

#Jahr extrahieren
aktivit�ten_2020_sum$Year <- year(aktivit�ten_2020_sum$Date)
aktivit�ten_2021_sum$Year <- year(aktivit�ten_2021_sum$Date)

#beide Datens�tze vereinen
aktivit�ten <- rbind(aktivit�ten_2020_sum, aktivit�ten_2021_sum)
head(aktivit�ten)

#Jahr �berall auf 2020 setzen, damit auf x-Achse nur Daten abgebildet sind ohne Jahre
year(aktivit�ten$Date) <- 2020
aktivit�ten$Jahr <- aktivit�ten$Year
aktivit�ten$Year <- NULL

#Daten speichern
write.csv(aktivit�ten,"aktivit�ten.csv", row.names = FALSE)


########## Transformation f�r Grafik In vs. Out ##########

In_Out_2020 <- frequenzen_2020
In_Out_2021 <- frequenzen_2021

#Datum transformieren 
In_Out_2020$Date <- ymd(In_Out_2020$Date)
str(In_Out_2020$Date)
In_Out_2021$Date <- ymd(In_Out_2021$Date)
head(In_Out_2021)

#beide Datens�tze vereinen
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



########## Grafik Aktivit�ten ##########

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


########## Grafik In vs. Out ##########

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








