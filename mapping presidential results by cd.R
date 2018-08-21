library(readr)
library(tigris)
library(sf)
library(ggplot2)
library(leaflet)
library(tmap)
library(tmaptools)
library(dplyr)
library(readr)
library(RColorBrewer)
library(htmlwidgets)
options(tigris_class = "sf")
tmap_mode("view")


prez_by_cd_orig <- read_csv("dkos_results_all.csv", skip = 1)
View(prez_by_cd_orig)
prez_by_cd <- prez_by_cd_orig

head(prez_by_cd)
names(prez_by_cd)
# View(prez_by_cd)

#rename columns
colnames(prez_by_cd) <- c("cd", "incumbent", "party", "clinton.2016", "trump.2016", "obama.2012", "romney.2012", "obama.2008", "mccain.2008")

#add winner columns
prez_by_cd$winner.2016 <- ifelse(prez_by_cd$clinton.2016 > prez_by_cd$trump.2016, "clinton", "trump")
prez_by_cd$winner.2012 <- ifelse(prez_by_cd$obama.2012 > prez_by_cd$romney.2012, "obama", "romney")
prez_by_cd$winner.2008 <- ifelse(prez_by_cd$obama.2008 > prez_by_cd$mccain.2008, "obama", "mccain")

#add margin of victory (pct points) columns
prez_by_cd$winner.2016.margin <- sqrt((prez_by_cd$clinton.2016 - prez_by_cd$trump.2016)^2)
prez_by_cd$winner.2012.margin <- sqrt((prez_by_cd$obama.2012 - prez_by_cd$romney.2012)^2)
prez_by_cd$winner.2008.margin <- sqrt((prez_by_cd$obama.2008 - prez_by_cd$mccain.2008)^2)

#eyeball new columns
prez_by_cd[,10:15]
write.csv(prez_by_cd, "prez_by_cd.csv")

View(prez_by_cd)


#-----BRINGING IN MAPPING FILES and building an interactive map



#for national map, 20m likely better here. For single states, 5m:
cd <- congressional_districts(cb = TRUE, resolution = "5m")
names(cd)
head(cd)

cd$STATEFP.numeric <- as.numeric(cd$STATEFP)

# filter(cd, STATEFP.numeric == 56)


statefipslookup <- read_csv("statefipslookup.csv")
# head(statefipslookup)

#join fips lookup to geospatial file ***revisit this to check if it's best way
cd_joined1 <- append_data(cd, statefipslookup, key.shp = "STATEFP.numeric", key.data = "fips")

# head(cd_joined1)
names(cd_joined1)

# filter(cd_joined1, STATEFP.numeric == 56)

#create unique district ID to match election results data
cd_joined1$cong.district <- paste(cd_joined1$state_code, cd_joined1$CD115FP, sep = "-")



#----  JOIN THE ELECTION RESULTS DATA TO GEOSPATIAL
cd_joined2 <- append_data(cd_joined1, prez_by_cd, key.shp = "cong.district", key.data = "cd")

# head(cd_joined2)

#remove AK, HI, PR

a <- filter(cd_joined2, STATEFP != "02")
b <- filter(a, STATEFP != "15")
c <- filter(b, STATEFP != "72")

mymap <- c
rm(a,b,c)

# mymap_test <-  tm_shape(mymap) +
#   tm_polygons(c("winner.2016"), id = "cong.district")
# # mymap_test

mymap_test <-  tm_shape(mymap) +
  tm_polygons(id = "cong.district") 
# mymap_test

#turn into leaflet object
mymap_leaflet <- tmap_leaflet(mymap_test)

# use leaflet package to zoom and center
mymap_leaflet <- mymap_leaflet %>%
  setView(-96, 37.8, zoom=4)
# mymap_leaflet


####let's try winners of the 2016 race

mymap_test <-  tm_shape(mymap) +
  tm_polygons("winner.2016", id = "cong.district",
              popup.vars=c("Winner" = "winner.2016",
                           "Trump Pct" = "trump.2016",
                           "Clinton Pct" = "clinton.2016"
                           )) 

mymap_test

#turn into leaflet object
mymap_leaflet <- tmap_leaflet(mymap_test)

# use leaflet package to zoom and center
mymap_leaflet <- mymap_leaflet %>%
  setView(-96, 37.8, zoom=4)

mymap_leaflet


saveWidget(mymap_leaflet, file="bycd-winner2016.html")



###trying out some popup options
#let's try pulling out just Trump winner and shading by pct

onlytrump <- filter(mymap, trump.2016>50)

mymap_test <-  tm_shape(onlytrump) +
  tm_polygons("trump.2016", 
              id = "cong.district", 
              title="Trump Vote Pct",
              popup.vars=c("Winner" = "winner.2016",
                           "Trump Pct" = "trump.2016",
                           "Clinton Pct" = "clinton.2016",
                           "Margin" = "winner.2016.margin")
              )
# mymap_test

#turn into leaflet object
mymap_leaflet <- tmap_leaflet(mymap_test)
# use leaflet package to zoom and center
mymap_leaflet <- mymap_leaflet %>%
  setView(-96, 37.8, zoom=4)
# mymap_leaflet

saveWidget(mymap_leaflet, file="bycd-trump-percentages.html")


##----------


##Clinton won where Romney also won?
clinton_romneydists <- filter(cd_joined2, winner.2016 == "clinton" & winner.2012 == "romney")
names(clinton_romneydists)

mymap_test <-  tm_shape(clinton_romneydists) +
  tm_polygons("winner.2016", 
              id = "cong.district",
              popup.vars=c("Winner" = "winner.2016", "Margin of Victory" = "winner.2016.margin"))
# mymap_test

#turn into leaflet object
mymap_leaflet <- tmap_leaflet(mymap_test)

# use leaflet package to zoom and center
mymap_leaflet <- mymap_leaflet %>%
  setView(-96, 37.8, zoom=4)
# mymap_leaflet

#-------------
###An ALTERNATE way of getting Clinton districts Romney won


##Clinton won where Romney also won?
cd_joined_temp <- cd_joined2
names(cd_joined_temp)  

#create new logical column on the fly for clinton/romney districts
cd_joined_temp$clinton.romney.win <- (cd_joined_temp$winner.2016=="clinton"&cd_joined_temp$winner.2012=="romney")

#remove AK, HI, PR
temp_a <- filter(cd_joined_temp, STATEFP.numeric != 2)
temp_b <- filter(temp_a, STATEFP.numeric != 15)
temp_c <- filter(temp_b, STATEFP.numeric != 72)

clinton_romneydists2 <- temp_c
rm(temp_a, temp_b, temp_c)

names(clinton_romneydists2)

#tmap object

#HOW TO SET COLOR PALETTES in TMAP
# display.brewer.all() 
# display.brewer.pal(9, "PuRd") 
# mypalette <- RColorBrewer::brewer.pal(9, "PuRd")[c(1,7)]

#can manually set hex values of palette too if need be:
mypalette <- c("#F7F9F9", "#2ECC71")

#BUILD THE MAP - TMAP OBJECT
mymap_test <-  tm_shape(clinton_romneydists2) +
  tm_polygons("clinton.romney.win", 
              id = "cong.district",
              popup.vars=c("Winner" = "winner.2016",
                           "Trump Pct" = "trump.2016",
                           "Clinton Pct" = "clinton.2016",
                           "Margin" = "winner.2016.margin"),
              palette = mypalette)
# mymap_test

#turn into LEAFLET OBJECT 
mymap_leaflet <- tmap_leaflet(mymap_test)
mymap_leaflet <- mymap_leaflet %>%
  setView(-96, 37.8, zoom=4)

mymap_leaflet

saveWidget(mymap_leaflet, file="bycd-clinton-romney.html")

