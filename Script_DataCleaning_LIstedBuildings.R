####################################################
#                                                  #
#  Cleaning and Augmenting Listed Building Data    #
#                                                  #
#      ClementineCttn - 10 March 2016              #
#                                                  #
####################################################




####################################
# Packages, Functions and Directory settings

setwd("~/Documents/BuildingAgeUK")

library(ggplot2)
library(RColorBrewer)
library(plyr)
library(shiny)
library(rgeos)
library(rgdal)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



####################################
# Initial File Upload

# London Wards
LDN = readOGR(dsn="LB_London/London_Ward_CityMerged.shp",
              layer = "London_Ward_CityMerged", encoding="utf8")
LDN=spTransform(LDN, CRS("+init=epsg:27700"))

# This file corresponds to the one originally sent by Historic England which has been projected with QGIS, and coordinates harmonised into the same reference system.
buildings_L = readOGR(dsn="LB_London/LB_Detailed_Reprojected.shp",
                      layer = "LB_Detailed_Reprojected", encoding="utf8")
buildings_L=spTransform(buildings_L, CRS("+init=epsg:27700"))





####################################
# Separating categories


# work with a dataframe rather than a shape file (lighter)
blg_L = buildings_L@data

# Making a vector of lists with all the categories to which each entry corresponds. Categories are separated by a comma in the original file.
blg_L$Monument.L = strsplit(as.character(blg_L$Monument.T), split = ", ")
head(blg_L$Monument.L, 10)

# store this version of the dataframe
blg_L_step1 = blg_L

# remove lines without info
blg_L = blg_L_step1[!is.na(blg_L_step1$Monument.L),]

# find the different categories used by Historic England
cats = c()
for (i in 1:dim(blg_L)[1]){
  d = length(cats)
  n = length(blg_L[i,"Monument.L"][[1]])
  for (j in 1:n){
    cats[d + j] =  blg_L[i,"Monument.L"][[1]][[j]]
    if(substrRight(cats[d + j], 1) == ",") cats[d + j] = substr(cats[d + j], 1, nchar(cats[d + j]) - 1)
  }
  print(i)}
categories = sort(unique(cats))

# To save this infomation :
#    tablecats = table(cats)
#    length(categories)
#    write.csv(tablecats, "LB_London/AllListedCategories.csv")





####################################
# From Monument Categories to Building Functions


# Import Polly's classification
tablePolly = read.csv("AllListedCategoriesPolly.csv", sep=",", dec=".")
catsPolly = unique(as.character(tablePolly$CategoryPolly))
catsPolly

# Key :
#
# ECC = ecclesiastical
# AGR = agricultural
# MIL = Military
# TRANS = Tranport
# MAN = Manufacturing and light industrial
# PUB/INST = Public/Institutional
# COM = Commercial
# COMM = Communication
# ENT = Entertainment
# GAR = Garden/parkland buildings which we could omit
# ZOO = Animal related buildings
# HOUSING = All housing types
# EDU = Education
# SPORT = Sports venue
# IND = Heavy Industrial/Energy related
# Omit = non building
# Unknown = unknown


# Create boolean variables for each category and function
blg_L[,c(as.character(categories), as.character(catsPolly))] = NA
head(blg_L)
for (i in 1:dim(blg_L)[1]){
  for (cat in categories){
    catPolly = as.character(tablePolly[tablePolly$cats == cat,"CategoryPolly"])
    if(paste0(cat,",") %in% blg_L[i,"Monument.L"][[1]] == "TRUE" ||
       cat %in% blg_L[i,"Monument.L"][[1]] == "TRUE" ||
       blg_L[i,"Monument.T"] == paste0(cat,",")) {
      blg_L[i,as.character(cat)] =  1
      blg_L[i,as.character(catPolly)] =  1
    }
  }
  print(i)}


# store and save this version of the dataframe
blg_L_step2 = blg_L
blg_L[,"Monument.L"] = NULL
write.csv(blg_L, "LB_London/LB_Detailed_Reprojected_All+PollyCategories.csv")

# store and save the dataframe with functions only
blg_L_step3 = blg_L[,c(1:32, 1288:1304)]
write.csv(blg_L_step3, "LB_London/LB_Detailed_Reprojected_PollyOnlyCategories.csv")
blg_L = blg_L_step3


# Summary of Listed Buildings by Function
colSums(blg_L[,33:49], na.rm = T)





####################################
# Mapping Building Functions


S_blg_L = buildings_L

# Join the dataframe with binary variable to the shape file
S_blg_L@data = data.frame(S_blg_L@data, blg_L[match(S_blg_L@data$List.Entry,blg_L$List.Entry),])


l = 0.1 # width of wards borders
par(mfrow=c(4,4), mar = c(0,0,1,0)) # margins and multiple window parameters
ClemSpringSummer2016Palette = c("#77c5ba", "#bbab61", "#ffced0", "#2db92d", "#9c636f", 
         "#ff9000", "#e1ff2f", "#505050", "#937f66", "#ff2313") # Colour palette


# Map it!
catsubset = S_blg_L[!is.na(S_blg_L@data$HOUSING),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="orange", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Houses"))

catsubset = S_blg_L[!is.na(S_blg_L@data$EDU),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="chartreuse3", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Education Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$PUBINST),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="goldenrod3", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Institutional Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$ECC),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="blue", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Ecclesiastical Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$MIL),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="olivedrab", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Military Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$TRANS),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="indianred", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Transport Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$IND),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="grey30", pch=19, cex=0.2, add=T)
title(paste0(n, " Heavy Manufacturing Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$MAN),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="grey50", pch=19, cex=0.2, add=T)
title(paste0(n, " Light Manufacturing Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$COM),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="deeppink2", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Commercial Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$COMM),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="mediumorchid4", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Communication Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$SPORT),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="lightseagreen", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Sports Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$ENT),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="orangered1", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Entertainment Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$GAR),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="forestgreen", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Garden and Parkland Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$AGR),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="yellow", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Agricultural Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$AGR) |
                      !is.na(S_blg_L@data$ECC) |
                      !is.na(S_blg_L@data$TRANS) |
                      !is.na(S_blg_L@data$MAN) |
                      !is.na(S_blg_L@data$PUBINST) |
                      !is.na(S_blg_L@data$COM) |
                      !is.na(S_blg_L@data$COMM) |
                      !is.na(S_blg_L@data$HOUSING) |
                      !is.na(S_blg_L@data$ENT) |
                      !is.na(S_blg_L@data$EDU) |
                      !is.na(S_blg_L@data$MIL) |
                      !is.na(S_blg_L@data$IND) |
                      !is.na(S_blg_L@data$GAR) |
                      !is.na(S_blg_L@data$SPORT) ,]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="burlywood4", pch=19, cex=0.2, add=T)
title(paste0("All ", n, " Buildings"))

catsubset = S_blg_L[!is.na(S_blg_L@data$List.Entry),]
n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="black", pch=19, cex=0.2, add=T)
title(paste0("All ", n, " Listed Entries"))






####################################
# Mapping non-Building Categories


S_blg_L = buildings_L
blg_L = blg_L_step2
blg_L[,"Monument.L"] = NULL

# Join the dataframe with binary variable to the shape file
S_blg_L@data = data.frame(S_blg_L@data, blg_L[match(S_blg_L@data$List.Entry,blg_L$List.Entry),])

# Select non-Buildings or unknown monuments
catsubset = S_blg_L[is.na(S_blg_L@data$AGR) &
                      is.na(S_blg_L@data$ECC) &
                      is.na(S_blg_L@data$TRANS) &
                      is.na(S_blg_L@data$MAN) &
                      is.na(S_blg_L@data$PUBINST) &
                      is.na(S_blg_L@data$COM) &
                      is.na(S_blg_L@data$COMM) &
                      is.na(S_blg_L@data$HOUSING) &
                      is.na(S_blg_L@data$ENT) &
                      is.na(S_blg_L@data$EDU) &
                    is.na(S_blg_L@data$MIL) &
                      is.na(S_blg_L@data$IND) &
                      is.na(S_blg_L@data$GAR) &
                      is.na(S_blg_L@data$SPORT) ,]

# Count the number of entries for each categories of non-buildings
sort(colSums(catsubset@data[,c(as.character(categories))], na.rm = T))
# NonBuildings = catsubset
# catsubset = NonBuildings

# Map 16 of them !
par(mfrow=c(4,4), mar = c(0,0,1,0))

n = dim(catsubset@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(catsubset, col ="black", pch=19, cex=0.2, add=T)
title(paste0("All ", n, " non-Buildings"))

subset2 = catsubset[!is.na(catsubset@data[,"WALL"]) |
                      !is.na(catsubset@data[,"GARDEN.WALL"]) |
                      !is.na(catsubset@data[,"BOUNDARY.WALL"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="grey50", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Walls"))

subset2 = catsubset[!is.na(catsubset@data[,"GATE"]) |
                      !is.na(catsubset@data[,"GATE.PIER"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="peru", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Gates and Gate Piers"))

subset2 = catsubset[!is.na(catsubset@data[,"RAILINGS"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="grey30", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Railings"))

subset2 = catsubset[!is.na(catsubset@data[,"TOMB"]) |
                      !is.na(catsubset@data[,"CHEST.TOMB"]) |
                      !is.na(catsubset@data[,"TABLE.TOMB"]) |
                      !is.na(catsubset@data[,"CEMETERY"])   ,]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[9], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Tombs, Stones & Cemeteries"))

subset2 = catsubset[!is.na(catsubset@data[,"COMMEMORATIVE.MONUMENT"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[1], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Commemorative Monuments"))

subset2 = catsubset[!is.na(catsubset@data[,"STATUE"]) |
                      !is.na(catsubset@data[,"SCULPTURE"]) ,]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[2], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Statues and Sculptures"))

subset2 = catsubset[!is.na(catsubset@data[,"LAMP.POST"]) |
                      !is.na(catsubset@data[,"STREET.LAMP"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[5], pch=19, cex=0.2, add=T)
title(paste0(n, " Street Lamps and Lamp Posts"))

subset2 = catsubset[!is.na(catsubset@data[,"TELEPHONE.BOX"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[10], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Telephone Boxes"))

subset2 = catsubset[!is.na(catsubset@data[,"WAR.MEMORIAL..FREESTANDING."]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[6], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed War Memorials"))

subset2 = catsubset[!is.na(catsubset@data[,"BRIDGE"]) |
                      !is.na(catsubset@data[,"ROAD.BRIDGE"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="blue", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Bridges"))

subset2 = catsubset[!is.na(catsubset@data[,"BOLLARD"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[4], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Bollards"))

subset2 = catsubset[!is.na(catsubset@data[,"MAUSOLEUM"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[8], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Mausoleums"))

subset2 = catsubset[!is.na(catsubset@data[,"ARCH"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="brown", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Arches"))

subset2 = catsubset[!is.na(catsubset@data[,"STEPS"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="mediumvioletred", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Steps"))
head(subset2@data[,1:33])

subset2 = catsubset[!is.na(catsubset@data[,"FORECOURT"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="mediumorchid4", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Forecourts"))

head(catsubset@data)
# Map another 16 of them !
par(mfrow=c(4,4), mar = c(0,0,1,0))

subset2 = catsubset[!is.na(catsubset@data[,"OBELISK"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="black", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Obelisks"))

subset2 = catsubset[!is.na(catsubset@data[,"MILESTONE"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[5], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Milestones"))

subset2 = catsubset[!is.na(catsubset@data[,"DRINKING.FOUNTAIN"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[1], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Drinking Fountains"))

subset2 = catsubset[!is.na(catsubset@data[,"FOUNTAIN"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="blue", pch=19, cex=0.2, add=T)
title(paste0(n, " other Listed Fountains"))

subset2 = catsubset[!is.na(catsubset@data[,"STABLE"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="peru", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Stables"))

subset2 = catsubset[!is.na(catsubset@data[,"PLAQUE"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="grey30", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Plaques"))

subset2 = catsubset[!is.na(catsubset@data[,"COAT.OF.ARMS"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[9], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Coats of Arms"))

subset2 = catsubset[!is.na(catsubset@data[,"BALUSTRADE"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[2], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Balustrades"))

subset2 = catsubset[!is.na(catsubset@data[,"CLOCK.TOWER"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[6], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Clock Towers"))

subset2 = catsubset[!is.na(catsubset@data[,"REVETMENT"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[8], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Revetements"))

subset2 = catsubset[!is.na(catsubset@data[,"COAL.DUTY.BOUNDARY.MARKER"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="blue", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Coal Duty Marker"))

subset2 = catsubset[!is.na(catsubset@data[,"BOUNDARY.MARKER"]) |
                      !is.na(catsubset@data[,"BOUNDARY.STONE"]) ,]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="blue", pch=19, cex=0.2, add=T)
title(paste0(n, " other Boundary Markers and Stones"))

subset2 = catsubset[!is.na(catsubset@data[,"COLUMN"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col =ClemSpringSummer2016Palette[5], pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Columns"))

subset2 = catsubset[!is.na(catsubset@data[,"SARCOPHAGUS"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="brown", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Sarcophaguses"))

subset2 = catsubset[!is.na(catsubset@data[,"LYCH.GATE"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="mediumvioletred", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Lych Gates"))

subset2 = catsubset[!is.na(catsubset@data[,"GROTTO"]),]
n = dim(subset2@data)[1]
plot(LDN, col = "lightgrey", border="white", lwd=l)
plot(subset2, col ="mediumorchid4", pch=19, cex=0.2, add=T)
title(paste0(n, " Listed Grottos"))





####################################
#
