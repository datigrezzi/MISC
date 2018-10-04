#
# A script to apply an I-VT fixation filter on tobii raw (combined) data
# input files: Tobii rawData files from directory 1 and a variables file
#
# script initiated on 23.01.2012
# updated 16.05.2013
# by Iyad Aldaqre
#
###########################################
# clear workspace
rm(list=ls())

library(zoo)
library(aspace)

# Setting Variables
source(paste(fundir,'setting variables.R',sep=''))

# files' and subjects' names	 ### CHANGE HERE ###

subjects <- dir(path = workdir1, pattern = '.tsv', all.files = FALSE,full.names = FALSE, recursive = FALSE,ignore.case = FALSE)
subject_names<- sub(".tsv", "", subjects, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
subject_names<- sub(experiment, "", subject_names, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

# Check if there are already some processed files
if(length(dir(workdir3))>0){
	appending<-1
	setwd(workdir3)
	processed<-sub("_preprocessed-data.txt","",dir())
	for(p in 1:length(processed)){
		subjects<-sub(processed[p],NA,subjects)
	}
	subjects<-subset(subjects,subset=is.na(subjects)!=T)
}else{appending<-0}

ValidityMat<-data.frame(NA,row.names=NULL,check.rows=FALSE,check.names=FALSE,stringsAsFactors=FALSE)
DistanceMat<-data.frame(NA,row.names=NULL,check.rows=FALSE,check.names=FALSE,stringsAsFactors=FALSE)

############################
## Getting events Details ##
############################
source(paste(fundir,'getting event details.R',sep=''))
print('event Details Saved - Remember to Check Segmented events!')

# reading events table
setwd(workdir2)
events<-read.table(eventsfile,sep="\t",header=T)

## Checking Segmented events ## TEMPORARY HERE, MOVE TO VARIABLES LATER

events[!(grepl('B',events$event)&grepl('Response',events$event))&!grepl('Intro',events$event)&!(grepl('A',events$event)&grepl('_R.',events$event)),'seg']<-1
stop('Check Segmented events First!') # if event to be segmented: events$seg<-1
######################
## Preparing Tables ##
######################
# i<-1
# for (i in 8:10){

for (i in 1:length(subjects)){

# set working directory
setwd(workdir1)

file <- subjects[i]								
data <- read.table(file, header = T, sep = "\t", dec = ",",fill = FALSE, na.strings="NA", blank.lines.skip = FALSE)
data<-data[,c('RecordingTimestamp','GazePointIndex','DistanceLeft','ValidityLeft','DistanceRight','ValidityRight','GazePointX..ADCSpx.','GazePointY..ADCSpx.','StudioEvent','StudioEventData','MediaName','X')]
names(data)<-c('timeStamp','Number','distanceLeft','validityLeft','distanceRight','validityRight','gazePointX','gazePointY','event','Descriptor','StimuliName','X')

data$X<-(data$validityLeft+data$validityRight)/2
validity<-(length(data$X[data$X<=2])/length(data$X))*100
ValidityMat[i,1]<-i
ValidityMat[i,2]<-subject_names[i]
ValidityMat[i,3]<-validity

# Calculating Distance from eye-tracker
data$distance<-(data$distanceLeft+data$distanceRight)/2
distance<-mean(data$distance,na.rm=T)
DistanceMat[i,1]<-i
DistanceMat[i,2]<-subject_names[i]
DistanceMat[i,3]<-distance

### Deacreasing Matrix Size ###
data$X<-0
data[data$StimuliName=='No media', 'X']<-1
for(p in 1:numintermedia){
	data[data$StimuliName==intermedia[p], 'X']<-1	
}
data[data$event=="MovieStart" | data$event=="MovieEnd" | data$event=="ImageStart" | data$event=="ImageEnd" | data$event=="LeftMouseClick" | data$event=="RightMouseClick" | data$event=="KeyPress","X"]<-1
data <- subset(data, subset = X != 1)

####################################
### Interpolating Missing Values ###
####################################
if(interpolation==1){
	source(paste(fundir,'interpolate missings.R',sep=''))
}

###############################
### Deleting Missing Values ### or what's left of them
###############################
data$X<-0
data[is.na(data$gazePointX)|is.na(data$gazePointY),'X']<-1
data <- subset(data, subset = X != 1)
# deleting out-of-screen fixations
data[data$gazePointX>resolution[1] | data$gazePointY>resolution[2],'X']<-1
data[data$gazePointX<0 | data$gazePointY<0,'X']<-1
data <- subset(data, subset = X != 1)
# adding subject's name
data[,13]<-subject_names[i]
data[,14]<-0
data<-data[,c(13,1,2,3,4,5,6,7,8,11,14,12)] # USE COLUMN NAMES
# Renaming rows and columns
names(data)<-c('subject','timeStamp','Number','distanceLeft','validityLeft','distanceRight','validityRight','gazePointX','gazePointY','event','trial','segment')
rownames(data) <- seq(length=nrow(data))

###################################
### marking trials and segments ###
###################################
eventMat<-events[events$subject==subject_names[i],]

if(sceneCond==1){ # each scene is treated differently (fam and test and then two scene timings are expected) e.g. AutoBox
	for(r in 1:nrow(eventMat)){
		if(r<=nfam){
			data$segment[data$timeStamp>=eventMat[r,"start"]+scenes[1,1] & data$timeStamp<=eventMat[r,"start"]+scenes[1,2]]<-1
		}else{
			data$segment[data$timeStamp>=eventMat[r,"start"]+scenes[2,1] & data$timeStamp<=eventMat[r,"start"]+scenes[2,2]]<-1
		}
		data$trial[data$timeStamp>=eventMat[r,"start"] & data$timeStamp<=eventMat[r,"end"]]<-r
	}
} else if(sceneCond==2){ # all scenes are to be treated the same, and scene timings will be treated as segments within each scene. e.g. wordLearning
	for(r in 1:nrow(eventMat)){ ###### MAKE SURE IT'S IN MS INSTEAD OF SECONDS (MULTIPLY BY 1000)
		if(eventMat$seg[r]==0){
			data$segment[data$timeStamp>=eventMat[r,"start"]+scenes[1,1] & data$timeStamp<=eventMat[r,"start"]+scenes[1,2]]<-1
		}else{
			for(k in 1:nseg){
				data$segment[data$timeStamp>=eventMat[r,"start"]+segments[k,1] & data$timeStamp<=eventMat[r,"start"]+segments[k,2]]<-k
			}
		}
		data$trial[data$timeStamp>=eventMat[r,"start"] & data$timeStamp<=eventMat[r,"end"]]<-r
	}
}

####################
### marking AOIs ### here for overall matrix (all gaze data) - copy code to fixations mat
####################
aoiType<-'polygon'

data$aoi<-'out'
if(aoiType=='polygon'){
	for(v in 1:naoi){data[point.in.polygon(data$gazePointX,data$gazePointY,aoimat[,1,v],aoimat[,2,v])>0,'aoi']<-aoinames[v]}
	} else if(aoiType=='circular'){
		for(v in 1:naoi){data$aoi[sqrt((data$gazePointX-aoimat[1,1,v])^2+(data$gazePointY-aoimat[1,2,v])^2)<=aoicorr[1,v]]<-aoinames[v]}
	}

##############################
### calculating velocities ###
##############################
source(paste(fundir,'simple i.vt.R',sep=''))

####################################
### marking and saving fixations ###
####################################
source(paste(fundir,'marking and saving fixations.R',sep=''))

## centroid check
## points(mean(temp$gazePointX),mean(temp$gazePointY),col='red')
## plot(temp$gazePointX,temp$gazePointY,col='blue')

print(paste(subject_names[i],i,", ",round(validity),'%'))

}

#########################
### Setting Threshold ### ???
#########################

# abline(velthresh,0)

################################################
### convert from pixel/ms to degree/s        ###
##  pixel to mm -> mm = (pixels * 25.4) / dpi ##
#   mm to pixel -> pixel = (mm * dpi) / 25.4   # MAKE VELTHRESH TO BE CALCULATED INDIVIDUALLY BASED ON DISTANCE, AND REPLACE VALUE IN VARIABLES TO DEG/S
#   degree = 2arctan(length/2distance)         #
##  degree = (radian * 180)/pi                ##
### size = tan(radian) * distance            ###
################################################
dpi<-96
distance<-mean(DistanceMat[,3],na.approxrm=T)
# distance<-650
mmthresh<-(velthresh*25.4)/dpi
degthresh<-2*atan_d(mmthresh/(2*distance))
degthresh # deg/ms
degthresh*1000 # deg/s
############################################
## Exporting Validity and Distance tables ##
############################################
setwd(workdir2)

validityfile<-paste(experiment,'_Validity.txt',sep='')
distancefile<-paste(experiment,'_Distance.txt',sep='')
if(appending==0){
	write.table(ValidityMat,validityfile, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=TRUE)
	write.table(DistanceMat,distancefile, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=TRUE)
	}else{
		write.table(ValidityMat,validityfile, append=T, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=FALSE)
		write.table(DistanceMat,distancefile, append=T, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=FALSE)
		}


################################################
### To check velocities from processed files ###
################################################
# datafile<-paste(subject_names[i],'_processed-data.txt',sep='')
# data<-read.table(datafile,header=T,sep="\t",fill = FALSE, na.strings="NA", blank.lines.skip = FALSE)
# plot(data$vel,type='p',col=i,cex=0.2)
# abline(velthresh,0)
