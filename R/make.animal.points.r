
# make point shapefile grba appropriate for testing. 

grba <- readOGR("//LAR-FILE-SRV/Data/NPS/SDraw/inst/doc/Shapefiles","GRBA_IntUp_finite")

animalN <- round(rnorm(nrow(grba@data),1000,400),0)
animalN <- ifelse(animalN < 0,0,animalN)
animalLevel <- as.numeric(cut(animalN,7))

animalCat <- rep(NA,nrow(grba@data))
for(i in 1:length(animalLevel)){
  if(animalLevel[i] == 1){
    animalCat[i] <- "Very Low"
  } else if(animalLevel[i] == 2){
    animalCat[i] <- "Low"  
  } else if(animalLevel[i] == 3){
    animalCat[i] <- "Medium Low" 
  } else if(animalLevel[i] == 4){
    animalCat[i] <- "Medium" 
  } else if(animalLevel[i] == 5){
    animalCat[i] <- "Medium High" 
  } else if(animalLevel[i] == 6){
    animalCat[i] <- "High"  
  } else if(animalLevel[i] == 7){
    animalCat[i] <- "Very High"  
  }
}

grba@data$animalN <- animalN
grba@data$Category <- animalCat



tempN <- round(rnorm(nrow(grba@data),250,41),0)
tempN <- ifelse(tempN < 0,0,tempN)
tempLevel <- as.numeric(cut(tempN,7))

tempCat <- rep(NA,nrow(grba@data))
for(i in 1:length(tempLevel)){
  if(tempLevel[i] == 1){
    tempCat[i] <- "Savanna"
  } else if(tempLevel[i] == 2){
    tempCat[i] <- "Temperate Rain Forest"  
  } else if(tempLevel[i] == 3){
    tempCat[i] <- "Alpine Tundra" 
  } else if(tempLevel[i] == 4){
    tempCat[i] <- "Wetland" 
  } else if(tempLevel[i] == 5){
    tempCat[i] <- "Urban" 
  } 
}

grba@data$Strata <- tempCat
grba@data$SHAPE_LENG <- grba@data$SHAPE_AREA <- grba@data$POLYGON <- NULL


writeOGR(grba,"//LAR-FILE-SRV/Data/NPS/SDraw/inst/doc/Shapefiles",layer="animal_points",driver="ESRI Shapefile",overwrite_layer=TRUE)

