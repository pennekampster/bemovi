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
# reduce file name to original
trajectory.data$file <- gsub("Traj_" ,"",trajectory.data$file)
trajectory.data$file <- gsub(".avi.txt" ,"",trajectory.data$file)

#calculate summary stats (count of frames) and merge with original data for trajectory selection
start_frame <- ddply(trajectory.data, .(trajectory,file), .fun = function(a){a[which.min(a$frame), ]})
end_frame <- ddply(trajectory.data, .(trajectory,file), .fun = function(a){a[which.max(a$frame), ]})
names(start_frame) <- c("start_frame","X_start","Y_start","file","trajectory")
names(end_frame) <- c("end_frame","X_end","Y_end","file","trajectory")
net_disp <- merge(start_frame,end_frame,by=c("trajectory","file"))
net_disp$net_disp <- round(sqrt((net_disp$X_end - net_disp$X_start)^2 + (net_disp$Y_end - net_disp$Y_start)^2))
#net speed in pixels/second
net_disp$duration <- (net_disp$end_frame - net_disp$start_frame)/27
net_disp$net_speed <- net_disp$net_disp/net_disp$duration
net_disp_summary <- net_disp[c(1,2,9,10,11)]
net_disp_summary$id <- paste(net_disp_summary$file,net_disp_summary$trajectory,sep="-")

#merge summary data with original trajectories
trajectory.data.summary <- merge(trajectory.data,net_disp_summary,by=c("trajectory","file"))

# 0. prepare data for analysis with adehabitat package
# create unique ID consisting of trajectory ID and file
id <- paste(trajectory.data$file,trajectory.data$trajectory,sep="-")
trajectory.data <- cbind(trajectory.data,id)

# filter very short trajectories out, otherwise problems when rediscretizing based on distance
trajectory.data.summary <- sqldf("select t.*
                                  from 'trajectory.data' t, net_disp_summary n
                                  where t.id=n.id AND n.net_disp >= 100")

# subsetting and plotting to check the effect of filtering
#subset <- subset(trajectory.data.summary, trajectory.data.summary$file == "Traj_Data34.avi.txt" |
#                 trajectory.data.summary$file == "Traj_Data38.avi.txt" |
#                 trajectory.data.summary$file == "Traj_Data49.avi.txt")

#subset34 <- subset(trajectory.data.summary, trajectory.data.summary$file == "Traj_Data34.avi.txt")
#subset38 <- subset(trajectory.data.summary, trajectory.data.summary$file == "Traj_Data38.avi.txt")
#subset49 <- subset(trajectory.data.summary, trajectory.data.summary$file == "Traj_Data49.avi.txt")

# plot to check which trajectories were included
#ggplot(subset34,aes(x=subset34$Y,y=subset34$X)) + geom_point()

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

# get gross displacement 
mvt_gross <- ld(mvt_data)
mvt_gross_summary <- sqldf("select id, sum(dist) as gross_disp
                            from mvt_gross 
                            group by id
                           ")

# merge net and gross displacement
trajectory.data.summary <- sqldf("select n.*, round(gross_disp) as gross_disp, net_disp / gross_disp as NGDR
                            from net_disp_summary n, mvt_gross_summary m  
                            where n.id=m.id
                           ")

# 5. rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 10)

# plotting of selected trajectories
plot(redis_space[[11]]$rel.angle)
acf(redis_space[[11]]$rel.angle, na.action=na.pass)
plot(redis_space[[11]]$x,redis_space[[11]]$y,asp=1)

# 6. transform ltraj object into dataframe to extract movement metrics (turning angles)
mvt_summary <- ld(redis_space)
turning <- ddply(mvt_summary, .(id), summarize, mean_turning=mean(rel.angle, na.rm=T))
sd_ta <- ddply(mvt_summary, .(id), summarize, sd_turning=sd(rel.angle, na.rm=T))
turning_summary <- cbind(sd_ta,turning)
turning_summary[,3] <- NULL
trajectory.data.summary <- merge(trajectory.data.summary,turning_summary,by=c("id"))

# 7. extracting autocorrelation structure
# a) correlelogram
for (i in 1:length(redis_space)){
relangle <- redis_space[[i]]$rel.angle
acf_object <- acf(relangle,na.action=na.pass, plot=FALSE)
if (i == 1){ac <- as.data.frame(acf_object$acf)
            ac$lag <- seq(1:length(acf_object$acf))
            ac$id  <- rep(attr(redis_space[[i]], c("id")), length(acf_object$acf))}

if (i> 1){ac.t <- as.data.frame(acf_object$acf)
          ac.t$lag <- seq(1:length(acf_object$acf))
          ac.t$id  <- rep(attr(redis_space[[i]], c("id")), length(acf_object$acf))
ac <- rbind(ac,ac.t)
}
}
colnames(ac) <- c("ACF","lag","id")
ac <- subset(ac,ac$lag >= 3)
# extract time of strongest positive autocorrelation (indicating period)
max_acf <- ddply(ac, .(id), summarize, max = max(ACF, na.rm=TRUE), max_lag=lag[which.max(ACF)]) 
# specify threshold that indicates period and set period to lag at strongest autocorrelation
max_acf$period <- ifelse(max_acf$max>0.4,max_acf$max_lag,0)
period <- max_acf[c("id","period")]

# b) periodogram
#spec <- redis_space[[1]]$rel.angle
#spec <- na.omit(spec)
#spectrum(spec, log="dB")

#merge with trajectory data summary
trajectory.data.summary <- merge(trajectory.data.summary,period,by=c("id"))

# export aggregated data on movement
write.table(trajectory.data.summary, file = paste(gsub("data/",merge.folder,to.data),"trajectory.data.summary.txt", sep = "/"), sep = "\t")

