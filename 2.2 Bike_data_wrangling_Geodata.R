# 0. Libraries ---------------------------------------------------------------
Libraries <- c("readr", "tidyverse", "lubridate", "dplyr", "ggmap", "stringr", "stringi", "geosphere", "tidyquant", "knitr", "readxl","purr")
lapply(Libraries, require, character.only = TRUE)


# 1. Geo-Data van stations toevoegen vanuit Google & hadmatig------------------------------------------

# Idee is om via formule de GPS coordinaten op te halen van de individuele adressen.
# Hierna kunnen we met een tweede formule de afstand bepalen tussen begin en eindstation.
# Tevens Zal de adres data worden gecatoriseerd in enkele buurten om te zien hoe het inter-buurt verloop is

BikeStations <- unique(select(Samplesq1, 'Start station'))
BikeStations <- BikeStations %>% rename(addresses = 'Start station')
nrow(BikeStations)
View(BikeStations)
write.csv(BikeStations, 'Bikestations.csv')

#BikeStationsNumber <- unique(Samplesq1$`Start station number`)
#(BikeStationsNumber)
#View(BikeStationsNumber)
#write.csv(BikeStations, "BikeStations.csv")

# BikeStationsE <- unique(select(Samplesq1, 'End station'))
# nrow(BikeStationsE)

# BikeStationsNumberE <- unique(Samplesq1$`End station number`)
# length(BikeStationsNumberE)
# View(BikeStationsNumberE)

## Iets minder eindstations, voor het maken van de Geo-data data-file zal ik gebruik maken van de 'beginstations'

# Adding GPS coordinates via Google Cloud Platform

# Select the file from the file chooser
# BikeStations <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
# BikeStations <- read.csv('Bikestations.csv', stringsAsFactors = FALSE)

# Initialize the data frame
# geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
# register_google(key = "#######################################")

#for(i in 1:nrow(BikeStations))
#{
# Print("Working...")
#  result <- geocode(BikeStations$addresses[i], output = "latlona", source = "google")
#  BikeStations$lon[i] <- as.numeric(result[1])
#  BikeStations$lat[i] <- as.numeric(result[2])
#  BikeStations$geoAddress[i] <- as.character(result[3])
#}
View(BikeStations)
#write.csv(BikeStations, 'Bikestations.csv', row.names=FALSE)

## Na deze excercitie eindelijk aan de praat te hebben gekregen blijkt slechts de helft van de coordinaten min of meer goed te staan. 
## Voornaamste reden hiervoor is dat de meeste adressen zeer summier zijn, vaak alleen straat/kruising van straten.
## In eerste instantie dacht ik om het hierbij te laten voor nu maar via de coordinaten zijn de goede GPS-nummers vrij makkelijk eruit te filteren.
## Idee is ome overgebleven adressen 'washington, dc, usa' toe te voegen. Dit is niet helemaal ideaal aangezien een deel van de adressen buiten de DC-area liggen.
## In de staten Maryland en Virginia om precies te zijn maar dit is een klein gedeelte.


## Om te kijken hoeveel er buiten de goede range vallen
## Met een blik op de electronische kaart wordt snel duidelijk dat de stations tussen lon (longitude) -77.3 en -76.7 moeten liggen 
## en tussen lat (latitude) 38.7 en 39.17

BikeStationslon <- arrange(BikeStations, desc(lon)) 
View(BikeStationslon)
BikeStationslat <- arrange(BikeStations, desc(lat))
View(BikeStationslat)

Bikestationsout <- filter(BikeStations, lon < -77.3 | lon > -76.7 | lat < 38.7 | lat > 39.17)
BikeStationsin <- filter(BikeStations, lon > -77.3 & lon < -76.7 & lat > 38.7 & lat < 39.17)
View(Bikestationsout)
nrow(Bikestationsout)
View(BikeStationsin)
nrow(BikeStationsin)
str(Bikestationsout)
write.csv(BikeStationsin, 'Bikestationsin.csv')

## Koppelen van ", washington, dc, usa" aan 'Bikestationsout' en daarna (wederom) opzoeken via Google cloud platform/Google Maps
mis <- " Washington DC"
Washington <- as.character(rep(mis,nrow(Bikestationsout)))
str(Washington)
Bikestationsout <- cbind(Bikestationsout, Washington)
Bikestationsout[,3:5] <- NULL 
View(Bikestationsout)
Bikestationsout$new <- stri_join(Bikestationsout$addresses,Bikestationsout$Washington)
Bikestationsout[,2:3] <- NULL
colnames(Bikestationsout)[2] <-"addresses"
View(Bikestationsout)
str(Bikestationsout)
write.csv(Bikestationsout, 'Bikestationsout.csv')

# Adding GPS coordinates for Bikestationsout
# Select the file from the file chooser
# Bikestationsout <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
Bikestationsout <- read.csv("Bikestationsout.csv", stringsAsFactors = FALSE)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

register_google(key = "########################################")

for(i in 1:nrow(Bikestationsout))
{
  result <- geocode(Bikestationsout$addresses[i], output = "latlona", source = "google")
  Bikestationsout$lon[i] <- as.numeric(result[1])
  Bikestationsout$lat[i] <- as.numeric(result[2])
  Bikestationsout$geoAddress[i] <- as.character(result[3])
}

View(Bikestationsout)
Bikestationsout2 <- filter(BikeStations, lon < -77.3 | lon > -76.7 | lat < 38.7 | lat > 39.17)
nrow(Bikestationsout2)
#write.csv(Bikestationsout, 'Bikestationsout.csv', row.names=FALSE)

# Het toevoegen van de suffix 'Wahington DC' aan de adresgegevens van straten die foutief elders zijn ge-geocodeerd heeft helaas niet
# tot het gewenste resultaat geleid. 
# Aangezien de analyse wel erg mager zou worden op een minderheid van de data is gekozen om de rest van de coordinaten met de hand op te zoeken.
# Opmerkelijk hierbij is dat in het grootste deel van de gevallen het goed werkte met de 'Washington DC' toevoeging,
# bij een beperkt aantal niet en na het weghalen van deze suffix bleek meestal dat de stations net in de staten Maryland of Virginia lagen.
# Deze handmatige data entry is via Excel gebeurt.

BikestationsGeocoded <- read_excel("QI Courses/Assignment/Bike/Data/Data per Q/Combined Q's/Bikestationsallgeocoded3.xlsx")
View(BikestationsGeocoded)


# Toevoegen van de geografische coordinaten opgezocht in vorige sectie aan de dataframe

Bikestationsin <- read.csv('Bikestationsin.csv', header = FALSE)
View(Bikestationsin)
Bikestationshand <- read.csv('Bikestationsallgeocoded.csv', header = FALSE)
View(Bikestationshand)

# 2. Samenvoegen opgezochte resultaten in bestaande files----------------------------------------


# Het bleek uitermate lastig om de opgezochte lon/lat coordinaten goed in de files te krijgen.
# Probleem was het matchen van de addressen met de geo-data, hierdoor ontstond zeer veel verlies aan data.
# Om toch enigszins een geo-analyse (ook) uit te voeren zal er een aparte Bikedata-file gemaakt worden met de geo-data erbij
BikeData <- read_csv("Bikedata.csv")
Geodata <- read_excel("BikestationsFinalShortS.xlsx")
Geodata <- select(Geodata, addresses, Start_station_5, lon, lat)

Geodata <- read_excel("BikestationsFinalShortE.xlsx")
GeodataE <- select(Geodata, addresses, End_station_7, lonE, latE)

#GeodataF <- read_excel("BikestationsFinalShortS.xlsx")
#GeodataF <- rename(GeodataF, addresses=End_station_7)
#GeodataF <- rename(GeodataF, lonF=lon)
#GeodataF <- rename(GeodataF, latF=lat)
#GeodataF <- mutate(GeodataF, End_station_7 = substr(addresses,1,25))
#GeodataF <- select(GeodataF, addresses,End_station_7, everything())
#GeodataF <- select(GeodataF, addresses, End_station_7, lonF, latF)

#BikeData$Start_station_5 <- substr(BikeData$Start_station_5,1,25)
#BikeData$End_station_7 <- substr(BikeData$End_station_7,1,25)

BikeData <- merge(BikeData, Geodata[,c("Start_station_5", "lon", "lat")], by="Start_station_5")
BikeData <- merge(BikeData, GeodataE[,c("End_station_7", "lonE", "latE")], by="End_station_7")

#BikeDataG <- rbind(BikeDataS, BikeDataF)

#BikeDataG <- unique()

#write to be analysed files to file 
write.csv(BikeData, "BikedataGeoCoded.csv")
# Uiteindelijk is het dan toch gelukt om met een zeer beperkt verlies aan data (5%) een (aparte) geo-file te maken waarmee 
# ook een geografische analyse gemaakt kan worden!
# De aangepaste adresvelden dienden eerste weer in de ude vorm teruggezet te worden waarna het 'mergen' uiteindelijk goed ging 



# 3. Afstand bepalen tussen stations -----------------------------------------

setwd("~/QI Courses/Assignment/Bike/R code")
BikeGeodata <- read.csv("BikedataGeoCoded.csv")
BikeGeodata <- filter(BikeGeodata, Zelfde_station_21==0) #voor deze analyse alleen (natuurlijk) trips gebruiken met een ander eind-station dan beginstation

#View(GPSStations)
#distance <- distm(c(BikeGeodata$lon, BikeGeodata$lat), c(BikeGeodata$lonE,BikeGeodata$latE), fun = distHaversine) 
# werkt met een rij...
#lontats <- mutate(lontats, distance = apply(lontats,c(c(lon, lat), c(lonE,latE)),distm))
#lontats <- mutate(lontats, distance = apply(distHaversine(c(lon, lat), c(lonE,latE)),r=6378137))        #distHaversine(c(c(lon, lat), c(lonE,latE))),FUN = distHaversine
#lontats <- mutate(lontats, distance = apply(lontats,c(c(lon, lat), c(lonE,latE)),distHaversine))
#distance <- apply(distm(lontats,c(BikeGeodata$lon, BikeGeodata$lat), c(BikeGeodata$lonE,BikeGeodata$latE), fun = distHaversine))
#distance <- apply(BikeGeodata, 24, sum)
#map_int(BikeGeodata,c(BikeGeodata$lon, BikeGeodata$lat), c(BikeGeodata$lonE,BikeGeodata$latE), distm, fun = distHaversine)
#BikeGeodata <- mutate(BikeGeodata, distance = distm(c(BikeGeodata$lon, BikeGeodata$lat), c(BikeGeodata$lonE,BikeGeodata$latE), fun = distHaversine))

# met deze functie (distm) lukte het maar niet om het over het hele bestand te krijgen, uiteindelijk wel met functie direct toepassen:
#GPS_Stations <- select(BikeGeodata, lon,lat,lonE,latE)
#GPS_Stations <- mutate(GPS_Stations, distance = 0)

hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}
#source function : https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula/23095329#23095329
#GPS_Stations$distance[1:nrow(GPS_Stations)] <- hav.dist(GPS_Stations$lon[1:nrow(GPS_Stations)], GPS_Stations$lat[1:nrow(GPS_Stations)], GPS_Stations$lonE[1:nrow(GPS_Stations)],GPS_Stations$latE[1:nrow(GPS_Stations)])

BikeGeodata$distance[1:nrow(BikeGeodata)] <- hav.dist(BikeGeodata$lon[1:nrow(BikeGeodata)], BikeGeodata$lat[1:nrow(BikeGeodata)], BikeGeodata$lonE[1:nrow(BikeGeodata)],BikeGeodata$latE[1:nrow(BikeGeodata)])

write.csv(BikeGeodata, "BikedataGeoCodedDist.csv")




# 4. Sector bepalen van begin,-& eindstation ------------------------------

# Voor deze additionele data-verrijking zal aan de hand van de GPS coordinaten van zowel begin,- alsmede eindstation een sector worden aangewezen.
# De 'kaart' wordt in eerste instantie ingedeeld in bovenste stuk (North) en onderste stuk (South) met alles wat overblijft in het midden (Center)


setwd("~/QI Courses/Assignment/Bike/R code")
BikeGeodata <- read.csv("BikedataGeoCodedDist.csv")

lonStart <- as.numeric(BikeGeodata$'lon')
latStart <- as.numeric(BikeGeodata$'lat')
lonEnd <- as.numeric(BikeGeodata$'lonE')
latEnd <- as.numeric(BikeGeodata$'latE')

#GPSStart <- cbind(lonStart,latStart)
#GPSEnd <- cbind(lonEnd,latEnd)

BikeGeodata <- mutate(BikeGeodata, StartSector= 'WS', EndSector = 'WE')

Centerlat = 38.90012
Centerlon = -77.00969
Northborder = 38.92896
Southborder = 38.87339
Eastborder = -76.98738
Westborder = -77.0457

# Aangezien in het centrum (Center) van de stad de meeste stations zitten en dus veruit de grootste sector is zal deze eerst worden onderverdeeld in 4 kwartielen. 
# In eerste instantie is onderzocht of dit, vanwege de schuinlopende rivieren, via schuine-lijn-vergelijkingen kan maar dit wordt wat teveel voor nu.
# Center zal, enigszins kunstmatig,in horizontale en verticale lijnen worden onderverdeeld.
# Hierna zal nog ten noorden en zuiden van de stad worden bepaald, alsmede oost en west.
# De GPS coordinaten zijn enigszins 'getweaked' om te zorgen dat iedere sector een minimaal aantal starts/ends heeft, min 5% van aantal
  
BikeGeodata$StartSector <- ifelse(latStart >= Centerlat & lonStart >= Centerlon & latStart <= Northborder & lonStart <= Eastborder, "NorthEastCenter",
                                  ifelse(latStart <= Centerlat & lonStart >= Centerlon & latStart >= Southborder & lonStart <= Eastborder, "SouthEastCenter",
                                         ifelse(latStart <= Centerlat & lonStart <= Centerlon & latStart >= Southborder & lonStart >= Westborder, "SouthWestCenter",
                                                ifelse(latStart >= Centerlat & lonStart <= Centerlon & latStart <= Northborder & lonStart >= Westborder, "NorthWestCenter",  
                                                       ifelse(latStart > Northborder,"NorthDC",
                                                              ifelse(latStart < Southborder, "SouthDC",
                                                                     ifelse(lonStart > Eastborder, "EasthDC",
                                                                            ifelse(lonStart < Westborder, "WestDC", "NA"))))))))
                                                      
BikeGeodata$EndSector <- ifelse(latEnd >= Centerlat & lonEnd >= Centerlon & latEnd <= Northborder & lonEnd <= Eastborder, "NorthEastCenter",
                                ifelse(latEnd <= Centerlat & lonEnd >= Centerlon & latEnd >= Southborder & lonEnd <= Eastborder, "SouthEastCenter",
                                       ifelse(latEnd <= Centerlat & lonEnd <= Centerlon & latEnd >= Southborder & lonEnd >= Westborder, "SouthWestCenter",
                                              ifelse(latEnd >= Centerlat & lonEnd <= Centerlon & latEnd <= Northborder & lonEnd >= Westborder, "NorthWestCenter",  
                                                     ifelse(latEnd > Northborder,"NorthDC",
                                                            ifelse(latEnd < Southborder, "SouthDC",
                                                                   ifelse(lonEnd > Eastborder, "EasthDC",
                                                                          ifelse(lonEnd < Westborder, "WestDC", "NA"))))))))


table(BikeGeodata$StartSector)
table(BikeGeodata$EndSector)

SectorStart <- BikeGeodata$StartSector
SectorEnd <- BikeGeodata$EndSector
BikeGeodata$'Samesector' <-  ifelse(SectorStart  == SectorEnd, 1,0) # geeft aan of fiets in zelfde sector blijft


write.csv(BikeGeodata, "BikedataGeoCodedDistSector.csv")


