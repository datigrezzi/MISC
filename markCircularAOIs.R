# mark AOIs as distance from their centers ========================== CHECK NEW AOIS
# make function
data$aoi <- "out"
for(ia in 1:naoi){
data$aoi[sqrt((data$gazePointX-aoimat[1,1,ia])^2+(data$gazePointY-aoimat[1,2,ia])^2)<=aoicorr[1,ia]]<-aoinames[ia]
}
