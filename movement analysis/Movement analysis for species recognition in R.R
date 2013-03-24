# code to extract movement characteristics from the results files of the ParticleTracker
# uses the adehabitatLT package to calculate movement metrics 
# 1. calculates summary statistics on net and gross movement
# 2. transforms the raw data (X and Y positions in to ltraj object to extract mean and SD of turning angles)
# 3. aggregates statistics on movement and stores data for further classification

rm(list=ls())

library(adehabitatLT)
library(grid)
library(plyr)
library(ggplot2)
library(sqldf)
library(reshape)

## Owen's paths
to.data.owen <- "/Users/owenpetchey/Desktop/hard.test/"

## Frank's paths
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## General folders
trajectory.data.folder <- "2 - trajectory data/"
merge.folder <- "merge morphology and trajectory/"

## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
  to.data <- to.data.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
  to.data <- to.data.owen}

# load trajectory data
trajectory.data <- read.table(paste0(to.data,trajectory.data.folder,"trajectory.data.txt"), header=TRUE, sep="\t")

# 0. prepare data for analysis with adehabitat package
# create unique ID consisting of trajectory ID and file
id <- paste(trajectory.data$file,trajectory.data$trajectory,sep="-")
trajectory.data.summary <- cbind(trajectory.data,id)

#calculate summary stats (count of frames) to perform trajectory selection
start_frame <- ddply(trajectory.data, .(id), .fun = function(a){a[which.min(a$frame), ]})
end_frame <- ddply(trajectory.data, .(id), .fun = function(a){a[which.max(a$frame), ]})
names(start_frame) <- c("id","start_frame","X_start","Y_start","file","trajectory")
names(end_frame) <- c("id","end_frame","X_end","Y_end","file","trajectory")
fixes_count <- ddply(trajectory.data, .(id), summarise, count = length(frame))

traject_features <- sqldf("select s.id, 
                          round(sqrt(power((e.X_end - s.X_start),2) + power((e.Y_end - s.Y_start),2))) as net_disp,
                          (e.end_frame - s.start_frame)+1 as frame_range
                          from start_frame as s, end_frame as e
                          where s.id=e.id")

traject_features <- merge(traject_features,fixes_count,by=c("id"))
traject_features$net_speed <- traject_features$net_disp/traject_features$frame_range

# detection rate quantifies how often an object was detected given the number of frames it spans (1 = 100%)
traject_features$detection_rate <- traject_features$count/traject_features$frame_range
traject_features$detection_rate <- traject_features$count/traject_features$frame_range

# filter very short trajectories out, otherwise problems when rediscretizing based on distance
select.trajectory.data.summary <- sqldf("select t.*
                from 'trajectory.data.summary' t, traject_features c
                where t.id=c.id AND count > 10 AND net_speed > 2 AND detection_rate > 0.9 AND net_disp > 10 AND file = 'Data34'")

# drop factor levels of id
trajectory.data.summary$id <- factor(trajectory.data.summary$id)

''' plot to calibrate trajectory exclusion
not.trajectory.data.summary <- sqldf("select t.*
                from 'trajectory.data.summary' t, traject_features c
                where t.id=c.id AND count < 10 AND net_speed < 2 AND detection_rate < 0.9 AND net_disp < 10 AND file = 'Data34'")

select.trajectory.data.plot <- subset(select.trajectory.data.summary, frame%%10==0)
not.trajectory.data.plot <- subset(not.trajectory.data.summary, frame%%2==0)

length(unique(not.trajectory.data.summary$id))

plot(select.trajectory.data.summary$Y, select.trajectory.data.summary$X+2048, xlim=c(0,2048), ylim=c(0,2048), col="#FFFF00", pch=1, cex=1, asp=1)
text(select.trajectory.data.plot$Y, select.trajectory.data.plot$X+2048-20,select.trajectory.data.plot$traject,cex=0.5,col="red")
par(new=T)
plot(not.trajectory.data.summary$Y, not.trajectory.data.summary$X+2048, xlim=c(0,2048), ylim=c(0,2048), col="green", pch=1, cex=1, asp=1)
text(not.trajectory.data.plot$Y, not.trajectory.data.plot$X+2048-20,not.trajectory.data.plot$traject,cex=0.5,col="red")
'''

# rename df with selected trajectories for further processing
trajectory.data.summary <- select.trajectory.data.summary

#*****************************************************************************************************************************
# only extract movement metrics for valid cell movements
# ***************************************************************************************************************************

# 1. convert frame into time class (each frame representing a time step of a second)
trajectory.data.summary$sec <- trajectory.data.summary$frame  
trajectory.data.summary$datetime <- as.POSIXct(trajectory.data.summary$sec, origin = "1900-01-01", format = "%OS")
trajectory.data.summary$sec <- NULL
options(digits.secs=2)

# 2. convert raw data in ltraj class (typeII=FALSE meaning that no time was recorded)
mvt_data <- as.ltraj(xy = trajectory.data.summary[,c("X","Y")], date = trajectory.data.summary$datetime, typeII=TRUE, id = trajectory.data.summary$id)

# 3. add positions where locations should have been measured (i.e. each second), but have not due to e.g. detection problems...
mvt_data <- setNA(mvt_data, date.ref = trajectory.data.summary$datetime, dt=1, units=c("sec"))

# optional: check whether adding positions rendered trajectory regular
# is.regular(mvt_data)

# 4. interpolate the positions which are missing (added as missing values before [see 3.]) by putting the time interval (dt) to 1 
mvt_data <- redisltraj(na.omit(mvt_data), 1, type="time")
mvt_summary <- ld(mvt_data)

# get gross & net displacement before rediscretizing the trajectories
gross_disp <- ddply(mvt_summary, .(id), summarise, gross_disp = sum(dist, na.rm=TRUE))
net_disp <-   sqldf("select id, sqrt(R2n) as net_disp
                  from mvt_summary 
                  group by id
                  having date = max(date)")
disp <- merge(net_disp,gross_disp,by=c('id'))


# 5. rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 5, nnew=8, type="time")

# plotting of selected trajectories
#plot(redis_space[[1]]$rel.angle)
#acf(redis_space[[11]]$rel.angle, na.action=na.pass)
#plot(redis_space[[11]]$x,redis_space[[11]]$y,asp=1)

# 6. transform ltraj object into dataframe to extract movement metrics (turning angles)
mvt_summary2 <- ld(redis_space)

# selection of trajectories with at least 5 fixes for the autocorrelation function
counts <- ddply(mvt_summary2, .(id), summarise, count = length(id))
mvt_summary2 <- merge(mvt_summary2,counts,by=c("id"))
mvt_summary2 <- subset(mvt_summary2, count >= 5)
mvt_summary2$rel.ang <- NULL
mvt_summary2$count <- NULL

# reset factor id
mvt_summary2$id <- factor(mvt_summary2$id)

turning <- ddply(mvt_summary2, .(id), summarize, mean_turning=mean(rel.angle, na.rm=T))
sd_ta <- ddply(mvt_summary2, .(id), summarize, sd_turning=sd(rel.angle, na.rm=T))
turning_summary <- cbind(sd_ta,turning)
turning_summary[,3] <- NULL

# convert dataframe back into ltraj object
redis_space2 <- dl(mvt_summary2)

# 7. extracting autocorrelation structure
# a) correlelogram
for (i in 1:length(redis_space2)){
relangle <- redis_space2[[i]]$rel.angle
acf_object <- acf(relangle,na.action=na.pass, plot=FALSE)
if (i == 1){ac <- as.data.frame(acf_object$acf)
            ac$lag <- seq(1:length(acf_object$acf))
            ac$id  <- rep(attr(redis_space[[i]], c("id")), length(acf_object$acf))}

if (i> 1){ac.t <- as.data.frame(acf_object$acf)
          ac.t$lag <- seq(1:length(acf_object$acf))
          ac.t$id  <- rep(attr(redis_space[[i]], c("id")), length(acf_object$acf))

          ac <- rbind(ac,ac.t)
}}

colnames(ac) <- c("ACF","lag","id")
ac <- subset(ac,ac$lag >= 3)
# extract time of strongest positive autocorrelation (indicating period)
max_acf <- ddply(ac, .(id), summarize, max = max(ACF, na.rm=TRUE), max_lag=lag[which.max(ACF)]) 
# specify threshold that indicates period and set period to lag at strongest autocorrelation
max_acf$period <- ifelse(max_acf$max>0.4,max_acf$max_lag,0)
period <- max_acf[c("id","period")]

ta_summary <- merge(turning_summary,period,by=c('id'))

# b) periodogram
#spec <- redis_space[[1]]$rel.angle
#spec <- na.omit(spec)
#spectrum(spec, log="dB")

#merge all movement metrics into one dataframe


# sqldf does not support > 2 table merge?!



trajectory.data.summary <- sqldf("select *,
                                  net_disp / gross_disp as NGDR,
                                  t.mean_turning, t.sd_turning,
                                  t.period
                                  from disp d
                                  left join ta_summary t
                                  on d.id=t.id ")


# export aggregated data on movement
#write.table(trajectory.data.summary, file = paste(gsub("data/",merge.folder,to.data),"trajectory.data.summary.txt", sep = "/"), sep = "\t")

