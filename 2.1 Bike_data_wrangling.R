# Stappen te zetten voor wat betreft data:

# Combineer alle maand,- en kwartaalbestanden tot een groot bestand;
# Check data op volledigheid en integriteit (wel NA/missing values, geen outliers);
# Voeg weerdata, feestdagen en (eventueel) events toe aan bestand;
# Maak een snapshot/sample van gecombineerde en verrijkte bestand om snel analyses op los te laten (voor testen)

# 0. Libraries ---------------------------------------------------------------
#Libraries <- c("readr", "tidyverse", "lubridate", "dplyr", "ggmap", "stringr", "stringi", "geosphere", "tidyquant", "knitr", "readxl")

Libraries <- c("readr", "tidyverse", "lubridate", "dplyr", "stringr", "stringi", "geosphere", "knitr", "readxl", "data.table")
lapply(Libraries, require, character.only = TRUE)

for (p in Libraries){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# 1. Uitpakken & inlezen Data ------------------------------------------------

setwd("~/QI Courses/Assignment/Bike/Data")

# Het uitpakken van de data is nog 'handmatig' gebeurt, vanaf hier zal worden getracht alles vanuit R te doen.

# De filenames zijn nogal lang met veel redundantie en zullen in eerste instantie hernoemd worden:
old_filenames <- list.files(path="~/QI Courses/Assignment/Bike/Data/Original Data", full.names=TRUE)
new_filenames <- paste0(path="~/QI Courses/Assignment/Bike/Data/Renamed Data/Bike_File_",1:length(old_filenames),".csv")
file.copy(from = old_filenames, to = new_filenames)

# Bij de eerste poging om alle data in een bestand te krijgen bokte de functie op een van de files die (waarschijnlijk)
# een iets andere opzet had, we doen daarom eerst een check of alle files hetzelfde format hebben
# Update 1, functie gevonden die alle bestanden samenvoegt plus voortgang checked en waarschuwt bij bestand wat bokt.
# Update 2, bestand wordt enorm groot bij combineren van alle data, voor nu besloten om per kwartaal de data te verzamelen in een bestand (2012 t/m 2017)

require(data.table)

# Onderstaande regels voor Q1 t/m Q4 gedaan:

setwd("~/QI Courses/Assignment/Bike/Data/Data per Q/Q4")
file_list <- list.files(path="~/QI Courses/Assignment/Bike/Data/Data per Q/Q4")
dataset <- data.frame()

for (i in 1:length(file_list)){
  temp_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  dataset <- rbindlist(list(dataset, temp_data), use.names = T, fill=TRUE) #for each iteration, bind the new data to the building dataset
}
write.csv(dataset, "datacombiQ4.csv") 
##

datacombiQ1 <- read_csv("~/QI Courses/Assignment/Bike/Data/Data per Q/Combined Q's/datacombiQ1.csv")
datacombiQ2 <- read_csv("~/QI Courses/Assignment/Bike/Data/Data per Q/Combined Q's/datacombiQ2.csv")
datacombiQ3 <- read_csv("~/QI Courses/Assignment/Bike/Data/Data per Q/Combined Q's/datacombiQ3.csv")
datacombiQ4 <- read_csv("~/QI Courses/Assignment/Bike/Data/Data per Q/Combined Q's/datacombiQ4.csv")
#datacombiAll <- rbind(datacombiQ1, datacombiQ2, datacombiQ3, datacombiQ4)
#write.csv(datacombiAll, "~/QI Courses/Assignment/Bike/Data/Data per Q/Combined Q's/datacombiAll.csv")
# De gecombineerde file is te groot voor mijn laptop, helaas.

# 2. Eerste (cijfermatige) indruk Data ---------------------------------------------------

#setwd("~/Documenten/Assignment/Bike/Data/Data per Q/Combined Q's") # USB stick


datacombiQ1 <- read_csv("datacombiQ1.csv")
datacombiQ2 <- read_csv("datacombiQ2.csv")
datacombiQ3 <- read_csv("datacombiQ3.csv")
datacombiQ4 <- read_csv("datacombiQ4.csv")
#datacombiAll <- read_csv("datacombiAll.csv")

str(datacombiQ1)
str(datacombiQ2)
str(datacombiQ3)
str(datacombiQ4)
#str(datacombiAll)

summary(datacombiQ1$Duration)
summary(datacombiQ2$Duration)
summary(datacombiQ3$Duration)
summary(datacombiQ4$Duration)
#summary(datacombiQAll$Duration)

head(datacombiQ1)
head(datacombiQ2)
head(datacombiQ3)
head(datacombiQ4)
#head(datacombiAll)

tail(datacombiQ1)
tail(datacombiQ2)
tail(datacombiQ3)
tail(datacombiQ4)
#tail(datacombiAll)

# Er lijken geen lege data velden en/of NA's te zijn, scheelt weer, waarschijnlijk al reeds in eerder stadium eruit gehaald

# 3. Sample aanmaken ---------------------------------------------------------

# Voor het testen van visualisaties & technieken etc. zal ik een sample maken zodat (test-)analyses snel kunnen worden gedaan:
# Onderstaande regels voor Q1 t/m Q4 gedaan:
datacombiQ4 <- read_csv("datacombiQ4.csv")
Sampleq4 <- sample(nrow(datacombiQ4),100000)
Samplesq4 <- datacombiQ4[Sampleq4,]
write.csv(Samplesq4, "Samplesq4.csv", row.names = FALSE) 

# 4. Feature engineering; Data uitbreiden & verrijken ---------------------------------------------

# Voor wat betreft de files voor nu wil ik de datum/tijd-kolommen graag (intern) aanvullen met:
# doordeweekse-dag (D) of weekend dag (W);
# alsmede dagdeel, ochtend (O), middag (M) of avond (A)
# van zowel de start date alsmede de end-date.

Samplesq1 <- read_csv("Samplesq4.csv")
View(Samplesq1)
attributes(Samplesq1$`Start date`)
WdaysS <- lubridate::wday(Samplesq1$`Start date`, week_start = getOption("lubridate.week.start", 1)) # dag van de weken toegevoegd  met maandag = 1, weekend 6 & 7. 
WdaysE <- lubridate::wday(Samplesq1$`End date`, week_start = getOption("lubridate.week.start", 1))   # dag van de weken toegevoegd  met maandag = 1, weekend 6 & 7.
Samplesq1 <- mutate(Samplesq1, 'Weekday Start' = WdaysS)
Samplesq1 <- mutate(Samplesq1, 'Weekday End' = WdaysE)
View(Samplesq1)
head(Samplesq1)

# Enkele 'one-hot coding' variabelen (dummy) aanmaken vanuit de data om sommige analyses (beter mogelijk) te maken
Samplesq1$'Weekend Start' <- ifelse(Samplesq1$'Weekday Start' < 6, 0, 1) # onderscheid maken tussen werkdagen en weekenddagen bij start dag
Samplesq1$'Weekend End' <- ifelse(Samplesq1$'Weekday End' < 6, 0, 1) # onderscheid maken tussen werkdagen en weekenddagen bij eind dag
Samplesq1$'Member' <- ifelse(Samplesq1$`Member type` == "Member", 1, 0) # geeft 1 als iemand member is, anders 0

DateStart <- as.Date(Samplesq1$`Start date`)
DateEnd <- as.Date(Samplesq1$`End date`)
Samplesq1$'Samedayback' <-  ifelse(DateStart  == DateEnd, 1,0) # geeft aan wanneer fiets zelfde dag nog wordt ingeleverd

# Indelen van de dag in 3 delen (ochtend, middag en avond)
Samplesq1$'Start time' <- as.ITime(Samplesq1$`Start date`)
Samplesq1$'End time' <- as.ITime(Samplesq1$`End date`)

Samplesq1$'Day part Start' <- format(as.POSIXct(Samplesq1$'Start time', tz = "" , format = "%H: %M"), "%H")
time2 <- as.numeric(Samplesq1$'Day part Start')
Samplesq1$'Day part Start' <- ifelse(time2 >= 05 & time2 <= 12, "Morning",
                           ifelse(time2 > 12 & time2 <= 18, "Afternoon",
                                  ifelse(time2 > 18 & time2 <= 23, "Evening", "Night")))

Samplesq1$'Day part End' <- format(as.POSIXct(Samplesq1$'End time', tz = "" , format = "%H: %M"), "%H")
time2 <- as.numeric(Samplesq1$'Day part End')
Samplesq1$'Day part End' <- ifelse(time2 >= 05 & time2 <= 12, "Morning",
                                     ifelse(time2 > 12 & time2 <= 18, "Afternoon",
                                            ifelse(time2 > 18 & time2 <= 23, "Evening", "Night")))

# Duration is totaal # seconden, zal het hieronder in een leesbaarder format gieten
RentTime <- seconds_to_period(Samplesq1$Duration)
Samplesq1 <- mutate(Samplesq1, 'RentTime' = RentTime) 
#str(Samplesq1$RentTime)
V#iew(Samplesq1)

# Toevoegen van de maand vanuit de datum
#Samplesq1 <- read_csv("Samplesq1Enriched.csv")
Maand <- month(Samplesq1$'Start date')
Samplesq1 <- mutate(Samplesq1, 'Month' = Maand)
#View(Samplesq1)
#write.csv(Samplesq1, "Samplesq1Enriched.csv")

# Zelfde station terug
#Samplesq1 <- read_csv("Samplesq1Enriched.csv")
ZelfdeStation <- ifelse(Samplesq1$'Start station number'== Samplesq1$'End station number',1,0)
Samplesq1 <- mutate(Samplesq1, 'Zelfde station' = ZelfdeStation)
#write.csv(Samplesq1, "Samplesq1Enriched.csv")

# Toevoegen van weeknummer
#Samplesq1 <- read_csv("Samplesq1EnrichedCleaned.csv")
Week <- strftime(Samplesq1$'Start date',format="%W")
Week <- as.numeric(Week)
Samplesq1 <- mutate(Samplesq1, 'WeekNr' = Week)
#View(Samplesq1)
#Samplesq1 <- Samplesq1[,-1]
#write.csv(Samplesq1, "Samplesq1EnrichedCleaned.csv")

# Dagdeel vernummeren
#Samplesq1 <- read_csv("Samplesq1EnrichedCleaned.csv")
#View(Samplesq1)
Samplesq1$`Day part Start`[Samplesq1$`Day part Start`=='Morning'] <- 1
Samplesq1$`Day part Start`[Samplesq1$`Day part Start`=='Afternoon'] <- 2
Samplesq1$`Day part Start`[Samplesq1$`Day part Start`=='Evening'] <- 3
Samplesq1$`Day part Start`[Samplesq1$`Day part Start`=='Night'] <- 0
Samplesq1$`Day part Start` <- as.numeric(Samplesq1$`Day part Start`)

Samplesq1$`Day part End`[Samplesq1$`Day part End`=='Morning'] <- 1
Samplesq1$`Day part End`[Samplesq1$`Day part End`=='Afternoon'] <- 2
Samplesq1$`Day part End`[Samplesq1$`Day part End`=='Evening'] <- 3
Samplesq1$`Day part End`[Samplesq1$`Day part End`=='Night'] <- 0
Samplesq1$`Day part End` <- as.numeric(Samplesq1$`Day part End`)
#Samplesq1 <- Samplesq1[,-1]
#write.csv(Samplesq1, "Samplesq1EnrichedCleaned.csv")

# Opschonen & ordenen van Data
#Samplesq1Enriched <- read_csv("Samplesq1Enriched.csv")
Samplesq1EnrichedCleaned <- Samplesq1[,-c(1,9)]
#View(Samplesq1EnrichedCleaned)
#write.csv(Samplesq1EnrichedCleaned, "Samplesq1EnrichedCleaned.csv")

# Hernoemen variabelen (kolommen) naar handzamere namen
#Samplesq1Enriched <- read_csv("Samplesq1EnrichedCleaned.csv")
#Samplesq1EnrichedCleaned <- Samplesq1Enriched[,-1]
Samplesq1EnrichedRenamed <- Samplesq1EnrichedCleaned %>% rename_all(function(x) gsub(" ", "_", x))
colnames(Samplesq1EnrichedRenamed) <- c(paste0(colnames(Samplesq1EnrichedRenamed),'_',1:ncol(Samplesq1EnrichedRenamed)))
colnames(Samplesq1EnrichedRenamed)
names(Samplesq1EnrichedRenamed)
write.csv(Samplesq1EnrichedRenamed, "Samplesq4EnrichedCleanedRenamed.csv")
# rm(list = ls())

# 5.Bestanden hernoemen en samenvatten --------------------------------------

BikeData1 <- read_csv("Samplesq1EnrichedCleanedRenamed.csv")
BikeData1 <- BikeData1[,-1]
BikeData2 <- read_csv("Samplesq2EnrichedCleanedRenamed.csv")
BikeData2 <- BikeData2[,-1]
BikeData3 <- read_csv("Samplesq3EnrichedCleanedRenamed.csv")
BikeData3 <- BikeData3[,-1]
BikeData4 <- read_csv("Samplesq4EnrichedCleanedRenamed.csv")
BikeData4 <- BikeData4[,-1]

BikeData <- rbind(BikeData1,BikeData2, BikeData3, BikeData4)

BikeData2012 <- filter(BikeData, year(Start_date_2) == 2012)
BikeData2013 <- filter(BikeData, year(Start_date_2) == 2013)
BikeData2014 <- filter(BikeData, year(Start_date_2) == 2014)
BikeData2015 <- filter(BikeData, year(Start_date_2) == 2015)
BikeData2016 <- filter(BikeData, year(Start_date_2) == 2016)
BikeData2017 <- filter(BikeData, year(Start_date_2) == 2017)

#write all, to be analysed files to file 
write.csv(BikeData, "Bikedata.csv")
write.csv(BikeData2012, "Bikedata2012.csv")
write.csv(BikeData2013, "Bikedata2013.csv")
write.csv(BikeData2014, "Bikedata2014.csv")
write.csv(BikeData2015, "Bikedata2015.csv")
write.csv(BikeData2016, "Bikedata2016.csv")
write.csv(BikeData2017, "Bikedata2017.csv")
