# light loggers pan KBay 2016 across all sites

library(reshape2)
library(plyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(plotrix)
library(devtools)
library(tools)

#clear work space
rm(list=ls())
ls()

getwd()

##########################################
###### 01.20.2018 Deployment ############
##########################################


#load in 1
read.csv("Light_logger/01.20.2018/2488_01202018.CSV", skip=8)



##### grab files in a list
PAR.files <- list.files(path="Light_logger/01.20.2018", pattern = "CSV$", full.names = T); PAR.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="Light_logger/01.20.2018", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(PAR.files))
{
  data<-read.csv(PAR.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-mdy(as.character(df$Date)) # corrects date format
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%d-%m %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2017-12-04 18:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2018-01-20 08:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
  # makes each df[i] as dataframe with specific file-name
  # write.csv(df.out, file=paste("trim",file.names[i])) # makes .csvs for output
}
# this is the end of the loop   
#WORKS
########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
files<-as.data.frame(mget(ls(pattern = "SN.*"))) # with SN as patterns in files from for loops
names(files) # see number of columns, and what these are
data_index<-c(1,(seq(2,17,2))) # these are the columns we will want: timestamp + raw data **change '18' to number of columns in your dataframe, specifying here to select 'every other column'

Jan20.18.PAR<-as.data.frame(c(files[, data_index])) # here is the data we want, now in df alone
names(June.PAR) = gsub(pattern = "_.*", replacement = "", x = names(June.PAR)) #strip name to SN only
colnames(June.PAR)[1]="timestamp" # rename the single column for time


#the above didnt work so do manually...
colnames(test)<-c("timestamp", "SN10956")
colnames(SN10957_01202018)<-c("timestamp", "SN10957")
colnames(SN10958_01202018)<-c("timestamp", "SN10958")
colnames(SN10959_01202018)<-c("timestamp", "SN10959")
colnames(SN2488_01202018)<-c("timestamp", "SN2488")
colnames(SN4803_01202018)<-c("timestamp", "SN4803")
colnames(SN6377_01202018)<-c("timestamp", "SN6377")
colnames(SN6378_01202018)<-c("timestamp", "SN6378")
colnames(SN6379_01202018)<-c("timestamp", "SN6379")
colnames(SN7272_01202018)<-c("timestamp", "SN7272")
colnames(SN7274_01202018)<-c("timestamp", "SN7274")
colnames(SN7276_01202018)<-c("timestamp", "SN7276")
colnames(SN7277_01202018)<-c("timestamp", "SN7277")
colnames(SN7278_01202018)<-c("timestamp", "SN7278")
colnames(SN7279_01202018)<-c("timestamp", "SN7279")
colnames(SN7280_01202018)<-c("timestamp", "SN7280")

#look for duplicated time stamps
library(move)
getDuplicatedTimestamps(x=as.factor(SN2488_01202018$Raw.PAR), 
                        timestamps=as.POSIXct(SN2488_01202018$timestamp, 
                                              format="%Y-%d-%m %H:%M:%S"))
                        




# above not working bc row nums not consitent. instead, merge using "Reduce" bc merge only does 2 dfs

Jan20.18.PAR<-Reduce(merge, list(SN10956_01202018,SN10957_01202018,SN10958_01202018,SN10959_01202018))
                                 
                                 
#merge not working                                 
merge(SN10956_01202018,SN10957_01202018,SN10958_01202018,SN10959_01202018, SN2488_01202018,SN4803_01202018,SN6377_01202018,SN6378_01202018, SN6379_01202018,
                    SN7272_01202018,SN7274_01202018,SN7276_01202018,SN7277_01202018,SN7278_01202018,
                    SN7279_01202018,SN7280_01202018)

vJan20.18.PAR<-merge(SN10956_01202018,SN10957_01202018,SN10958_01202018, by=c("timestamp"))

Test1b<-merge(SN10956_01202018,SN10957_01202018, by="timestamp")
Test1c<-merge(Test1b, SN10958_01202018, by="timestamp")

##use rbind to stack your diff dates for same logger

### apply data callibration for SN4322, SN4375, SN4805
June.PAR$SN4322<-(June.PAR$SN4322*0.09929) # Rf42
June.PAR$SN4375<-(June.PAR$SN4375*0.05762) # Rf44, starting 7/3 logger failing? maybe 7/21
June.PAR$SN4805<-(June.PAR$SN4805*0.07131) # HIMB

colnames(June.PAR)<-c("timestamp", "Rf42.mid", "Rf44.mid", "HIMB.mid")
June.PAR<-June.PAR[c("timestamp","Rf44.mid", "Rf42.mid", "HIMB.mid")] # reorder from N>S
June.PAR$Rf44.mid[June.PAR$timestamp >= "2016-07-21 00:00:00"] <- NA # set beyond this as NA

write.csv(June.PAR, "data/environmental/temp and light/Jun_DecPAR/all PAR/June.PAR.csv")

# Check the 15-min calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=June.PAR[,i]
  x=June.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(June.PAR)[i], xlab="Date-time")
}

############################
# calculate daily integrated light values for each logger
df<-June.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp) # change to DATE ONLY format to calulate range, means
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F")) # split df by date

df.dli<-aggregate(data.frame(Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf42.mid.DLI=df.split[[1]]$Rf42.mid*0.0864,
                             HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-06-09"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2016-08-02"), ]

colnames(df.dli)
# Check the dli calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=df.dli[,i]
  x=df.dli$Date
  plot(y~x, data=df.dli, ylab=colnames(df.dli)[i], xlab="Date-time", type="l")
}

write.csv(df.dli, "data/environmental/temp and light/Jun_DecPAR/all DLI/DLI.Jun2016.csv")


##########################################
######  August - October Deployment ######
##########################################
rm(list=ls())
ls()

##### grab files in a list
PAR.files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T12", pattern = "CSV$", full.names = T); PAR.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T12", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(PAR.files))
{
  data<-read.csv(PAR.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-mdy(as.character(df$Date)) # corrects date format
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2016-08-02 15:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2016-10-03 08:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
# merge the time of R44 with the data from R42 with NAs for blank data
SN4323_Rf42_T12<-merge(SN4375_Rf44_T12, SN4323_Rf42_T12, by="timestamp", all=T) 
SN4323_Rf42_T12<-SN4323_Rf42_T12[-2] # remove column 2 from R44 data and rename

# logger appears to have something on it reducing light? fell over? trim this data
SN4375_Rf44_T12$Raw.PAR[SN4375_Rf44_T12$timestamp >= "2016-08-28 00:00:00" & 
                           SN4375_Rf44_T12$timestamp < "2016-09-11 00:00:00" ] <- NA # set beyond this as NA

files<-as.data.frame(mget(ls(pattern = "SN.*")))
names(files) 

data_index<-c(1,(seq(2,6,2)))

Aug.PAR<-as.data.frame(c(files[, data_index])) 
names(Aug.PAR) = gsub(pattern = "_.*", replacement = "", x = names(Aug.PAR)) 
colnames(Aug.PAR)[1]="timestamp"

### apply data callibration for SN4323, SN4375, SN4805
Aug.PAR$SN4323<-(Aug.PAR$SN4323*0.104) # Rf42
Aug.PAR$SN4375<-(Aug.PAR$SN4375*0.05762) # Rf44 
Aug.PAR$SN4805<-(Aug.PAR$SN4805*0.07131) # HIMB

colnames(Aug.PAR)<-c("timestamp", "Rf42.mid", "Rf44.mid", "HIMB.mid")
Aug.PAR<-Aug.PAR[c("timestamp","Rf44.mid", "Rf42.mid", "HIMB.mid")]
Aug.PAR$Rf44.mid[Aug.PAR$timestamp >= "2016-10-03 00:00:00"] <- NA # set beyond this as NA

write.csv(Aug.PAR, "data/environmental/temp and light/Jun_DecPAR/all PAR/Aug.PAR.csv")

# Check the 15-min calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=Aug.PAR[,i]
  x=Aug.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(Aug.PAR)[i], xlab="Date-time")
}

############################
# calculate daily integrated light values for each logger
df<-Aug.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp)
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F"))

df.dli<-aggregate(data.frame(Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf42.mid.DLI=df.split[[1]]$Rf42.mid*0.0864,
                             HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-08-02"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2016-10-03"), ]

colnames(df.dli)

# Check the dli calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=df.dli[,i]
  x=df.dli$Date
  plot(y~x, data=df.dli, ylab=colnames(df.dli)[i], xlab="Date-time", type="l")
}

write.csv(df.dli, "data/environmental/temp and light/Jun_DecPAR/all DLI/DLI.Aug2016.csv")


###############################
###### October Deployment #####
###############################
rm(list=ls())
ls()

##### grab files in a list
PAR.files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T13", pattern = "CSV$", full.names = T); PAR.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T13", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(PAR.files))
{
  data<-read.csv(PAR.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-parse_date_time(df$Date, "dmy")
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2016-10-03 12:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2016-11-03 13:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
files<-as.data.frame(mget(ls(pattern = "SN.*")))
names(files)

data_index<-c(1,(seq(2,6,2)))

Oct.PAR<-as.data.frame(c(files[, data_index]))
names(Oct.PAR) = gsub(pattern = "_.*", replacement = "", x = names(Oct.PAR)) 
colnames(Oct.PAR)[1]="timestamp" 

### apply data callibration for SN4322, SN4375, SN4805
Oct.PAR$SN4322<-(Oct.PAR$SN4322*0.09929) # Rf42
Oct.PAR$SN4375<-(Oct.PAR$SN4375*0.05762) # Rf44
Oct.PAR$SN4805<-(Oct.PAR$SN4805*0.07131) # HIMB

colnames(Oct.PAR)<-c("timestamp", "Rf42.mid", "Rf44.mid", "HIMB.mid")
Oct.PAR<-Oct.PAR[c("timestamp","Rf44.mid", "Rf42.mid", "HIMB.mid")] # reorder from N>S

write.csv(Oct.PAR, "data/environmental/temp and light/Jun_DecPAR/all PAR/Oct.PAR.csv")

# Check the 15-min calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=Oct.PAR[,i]
  x=Oct.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(Oct.PAR)[i], xlab="Date-time")
}

############################
# calculate daily integrated light values for each logger
df<-Oct.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp)
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F")) # split df by date

df.dli<-aggregate(data.frame(Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf42.mid.DLI=df.split[[1]]$Rf42.mid*0.0864,
                             HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-10-03"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2016-11-03"), ]

colnames(df.dli)

# Check the dli calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=df.dli[,i]
  x=df.dli$Date
  plot(y~x, data=df.dli, ylab=colnames(df.dli)[i], xlab="Date-time", type="l")
}

write.csv(df.dli, "data/environmental/temp and light/Jun_DecPAR/all DLI/DLI.Oct2016.csv")



################################
###### November Deployment #####
################################
rm(list=ls())
ls()

##### grab files in a list
PAR.files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T14", pattern = "CSV$", full.names = T); PAR.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T14", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(PAR.files))
{
  data<-read.csv(PAR.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-parse_date_time(df$Date, "dmy")
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2016-11-08 12:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2016-11-24 17:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
files<-as.data.frame(mget(ls(pattern = "SN.*")))
names(files)

data_index<-c(1,(seq(2,6,2)))

Nov.PAR<-as.data.frame(c(files[, data_index])) 
names(Nov.PAR) = gsub(pattern = "_.*", replacement = "", x = names(Nov.PAR))
colnames(Nov.PAR)[1]="timestamp" 

### apply data callibration for SN4322, SN4375, SN4805
Nov.PAR$SN4322<-(Nov.PAR$SN4322*0.09929) # Rf42
Nov.PAR$SN4375<-(Nov.PAR$SN4375*0.05762) # Rf44
Nov.PAR$SN4805<-(Nov.PAR$SN4805*0.07131) # HIMB

colnames(Nov.PAR)<-c("timestamp", "Rf42.mid", "Rf44.mid", "HIMB.mid")
Nov.PAR<-Nov.PAR[c("timestamp","Rf44.mid", "Rf42.mid", "HIMB.mid")] # reorder from N>S

write.csv(Nov.PAR, "data/environmental/temp and light/Jun_DecPAR/all PAR/Nov.PAR.csv")

# Check the 15-min calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=Nov.PAR[,i]
  x=Nov.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(Nov.PAR)[i], xlab="Date-time")
}

############################
# calculate daily integrated light values for each logger
df<-Nov.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp) 
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F")) 

df.dli<-aggregate(data.frame(Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf42.mid.DLI=df.split[[1]]$Rf42.mid*0.0864,
                             HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-11-08"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2016-11-24"), ]

colnames(df.dli)

# Check the dli calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=df.dli[,i]
  x=df.dli$Date
  plot(y~x, data=df.dli, ylab=colnames(df.dli)[i], xlab="Date-time", type="l")
}

write.csv(df.dli, "data/environmental/temp and light/Jun_DecPAR/all DLI/DLI.Nov2016.csv")


################################
###### December Deployment #####
################################
rm(list=ls())
ls()

##### grab files in a list
PAR.files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T15", pattern = "CSV$", full.names = T); PAR.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/Jun_DecPAR/light_T15", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(PAR.files))
{
  data<-read.csv(PAR.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-parse_date_time(df$Date, "dmy")
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  df<-df[!(df$timestamp < "2016-12-09 14:00:00"),] # start at this time
  df<-df[!(df$timestamp >= "2017-01-13 10:00:00"),] # end at this time
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
files<-as.data.frame(mget(ls(pattern = "SN.*")))
names(files)
data_index<-c(1,(seq(2,6,2)))

Dec.PAR<-as.data.frame(c(files[, data_index])) 
names(Dec.PAR) = gsub(pattern = "_.*", replacement = "", x = names(Dec.PAR)) 
colnames(Dec.PAR)[1]="timestamp"

### apply data callibration for SN4322, SN4375, SN4805
Dec.PAR$SN4322<-(Dec.PAR$SN4322*0.09929) # Rf42
Dec.PAR$SN4375<-(Dec.PAR$SN4375*0.05762) # Rf44
Dec.PAR$SN4805<-(Dec.PAR$SN4805*0.07131) # HIMB

colnames(Dec.PAR)<-c("timestamp", "Rf42.mid", "Rf44.mid", "HIMB.mid")
Dec.PAR<-Dec.PAR[c("timestamp","Rf44.mid", "Rf42.mid", "HIMB.mid")] # reorder from N>S

write.csv(Dec.PAR, "data/environmental/temp and light/Jun_DecPAR/all PAR/Dec.PAR.csv")

# Check the 15-min calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=Dec.PAR[,i]
  x=Dec.PAR$timestamp
  plot(y~x, data=df, ylab=colnames(Dec.PAR)[i], xlab="Date-time")
}

############################
# calculate daily integrated light values for each logger
df<-Dec.PAR
df$timestamp<-strptime(df$timestamp, format="%Y-%m-%d %H:%M:%S")
df$timestamp<-as.Date(df$timestamp)
str(df)

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F"))

df.dli<-aggregate(data.frame(Rf44.mid.DLI=df.split[[1]]$Rf44.mid*0.0864,
                             Rf42.mid.DLI=df.split[[1]]$Rf42.mid*0.0864,
                             HIMB.mid.DLI=df.split[[1]]$HIMB.mid*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-12-09"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date >="2017-01-13"), ]

colnames(df.dli)

# Check the dli calibrated data
par(mfrow=c(2,2), mar=c(5,4,1,2))
for(i in 1:length(1:4)) {
  y=df.dli[,i]
  x=df.dli$Date
  plot(y~x, data=df.dli, ylab=colnames(df.dli)[i], xlab="Date-time", type="l")
}

write.csv(df.dli, "data/environmental/temp and light/Jun_DecPAR/all DLI/DLI.Dec2016.csv")

########################################
###### Reef 10 alone ###################
########################################
rm(list=ls())
ls()

##### grab files in a list
PAR.files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/R10", pattern = "CSV$", full.names = T); PAR.files

##### what are the file names, sans extensions
file.names<-file_path_sans_ext(list.files(path="data/environmental/temp and light/Jun_DecPAR/R10", pattern = "CSV$", full.names = F))

############ formatting all data in for loop
for(i in 1:length(PAR.files))
{
  data<-read.csv(PAR.files[i], sep=",", skip=8)
  df<-data[, c(-1,-5)] # removes "trash" columns
  colnames(df)<-c("Date", "Time", "Raw.PAR")
  df$Date<-parse_date_time(df$Date, "dmy")
  df$timestamp<-as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
  df<-df[ , c(4,3)] # timestamp and Raw value only
  make.names(assign(paste("SN",file.names[i], sep=""), df)) # put pattern at front of names
}
# this is the end of the loop

########## see the files you've made, as a list, then grab those that fill your pattern 
ls()
df<-rbind(SN2486_May.Aug_K4, SN2486_Aug.Oct_K4, SN2486_Oct_K4, SN2486_Nov.Jan_K4)
df$Rf10.mid<-df$Raw.PAR*0.08769 #calibrated

df<-df[-2] #remove Raw data
df<-df[!(df$timestamp < "2016-06-09 18:00:00"),] # start at this time
df<-df[!(df$timestamp >= "2017-01-13 10:00:00"),] # end at this time to match other sites

Rf10.mid<-df

# Check the 15-min calibrated data
par(mfrow=c(1,2), mar=c(5,4,1,2))
for(i in 1:length(1:2)) {
  y=Rf10.mid[,i]
  x=Rf10.mid$timestamp
  plot(y~x, data=df, ylab=colnames(Rf10.mid)[i], xlab="Date-time")
}

write.csv(Rf10.mid, "data/environmental/temp and light/Jun_DecPAR/all PAR/Reef 10/Rf10.PAR.csv")

############################
# calculate daily integrated light values for each logger
str(df)
df$timestamp<-as.Date(df$timestamp) # change to DATE ONLY format to calulate range, means

df.split <- split(df, f=df$timestamp
                  < as.Date("2014-10-07", format="%F")) # split df by date

df.dli<-aggregate(data.frame(Rf10.mid.DLI=df.split[[1]]$Rf10.mid*0.0864),
                  by=list(Date=df.split[[1]]$timestamp), FUN=mean)

df.dli<-df.dli[!(df.dli$Date=="2016-08-04"), ] # remove dates without full 24 h deployment
df.dli<-df.dli[!(df.dli$Date=="2016-10-11"), ]
df.dli<-df.dli[!(df.dli$Date=="2016-10-12"), ]
df.dli<-df.dli[!(df.dli$Date=="2016-10-25"), ]
df.dli<-df.dli[!(df.dli$Date=="2016-11-08"), ]
df.dli<-df.dli[!(df.dli$Date >="2017-01-13"), ]

colnames(df.dli)

# Check the dli calibrated data
par(mfrow=c(1,1), mar=c(5,4,1,2))
for(i in 1:length(1:2)) {
  y=df.dli[,i]
  x=df.dli$Date
  plot(y~x, data=df.dli, ylab=colnames(df.dli)[i], xlab="Date-time", type="l")
}

write.csv(df.dli, "data/environmental/temp and light/Jun_DecPAR/all DLI/Reef 10/DLI.Rf102016.csv")


########################################
###### Combine all PAR and all DLI #####
########################################
rm(list=ls())
ls()

##### 
##### all DLI for graphs
files <- list.files(path="data/environmental/temp and light/Jun_DecPAR/all DLI", pattern = "csv$", full.names = T)
tables <- lapply(files, read.csv, header = TRUE)
DLI<-do.call(rbind, tables)
DLI=DLI[-1] # remove junk column
DLI$Date<-as.Date(DLI$Date) # fix date
DLI<-DLI[order(DLI$Date),] # order by Date

# make a date sequence for entire study
all.date<-as.data.frame(seq(as.Date("2016-06-10"), as.Date("2017-01-12"), "days"))
colnames(all.date)="Date"

# merge the DLI data and the date sequence to make a complete df through time
DLI.3site<-merge(all.date, DLI, by="Date", all.x=T)
R10<-read.csv("data/environmental/temp and light/Jun_DecPAR/all DLI/Reef 10/DLI.Rf102016.csv")
R10<-R10[-1]; R10$Date<-as.Date(R10$Date)

DLI.4site<-merge(DLI.3site, R10, by="Date", all.x=T)
DLI.4site$month <- months(as.Date(DLI.4site$Date)) # makes a month column
DLI.4site<-DLI.4site[, c(1,6, 2:5)]

# determine monthly mean for PAR during deployments at depth
write.csv(DLI.4site, "data/environmental/temp and light/Jun_DecPAR/All.DLI.csv")


#########################
##### Figure
#########################
reefcols=c("mediumseagreen", "dodgerblue", "salmon", "orchid")
par(mar=c(2,3.6,1,1.3), mgp=c(2,0.5,0))
layout(matrix(c(1,2,3,4), nrow=2, byrow=TRUE))

plot(Rf44.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[1])
legend("topright", lty=1, col=reefcols[1], legend="Reef 44", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")

plot(Rf42.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[2])
legend("topright", lty=1, col=reefcols[2], legend="Reef 42", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")

plot(HIMB.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[3])
legend("topright", lty=1, col=reefcols[3], legend="HIMB", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")

plot(Rf10.mid.DLI~Date, type="l", All.DLI, ylab=(expression(paste("DLI mol photons", ~m^-2, ~d^-1, sep=""))), ylim=c(0, 30), xaxt="n", xlab="Time", col=reefcols[4])
legend("topright", lty=1, col=reefcols[4], legend="Reef 10", lwd=1.5, bty="n", cex=0.8, 
       x.intersp = 0.3, inset=c(-0.1, -0.05))
axis.Date(1, at=seq(min(All.DLI$Date), max(All.DLI$Date), by="2 mo"), format="%b '%y")


dev.copy(pdf, "figures/environmental/All.PAR.pdf", width=6.5, height=4)
dev.off()
