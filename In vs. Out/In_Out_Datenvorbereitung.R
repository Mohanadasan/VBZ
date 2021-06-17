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