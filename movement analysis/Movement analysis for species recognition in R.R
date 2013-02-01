rm(list=ls())

# code to extract movement characteristics from the results files of the ParticleTracker
# still contains code to extract positions from raw text file
# uses the adehabitatLT package to calculate movement metrics (and trials to explore functionality of the package)
library(adehabitatLT)
library(grid)
library(plyr)
library(ggplot2)

# load trajectory data
trajectory.data <- read.table("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/trajectory.data.txt", header=TRUE, sep="\t")

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

#merge summary data with original trajectories
trajectory.data.summary <- merge(trajectory.data,net_disp_summary,by=c("trajectory","file"))

# prepare data for analysis with adehabitat package
# create unique ID consisting of trajectory ID and file
id <- paste(trajectory.data.summary$file,trajectory.data.summary$trajectory,sep="-")
trajectory.data.summary <- cbind(trajectory.data.summary,id)

# filter very short trajectories out, otherwise problems when rediscretizing based on distance
trajectory.data.summary <- subset(trajectory.data.summary,net_disp >= 100)

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

# get gross displacement and merge with original data
mvt_gross <- ld(mvt_data)
sum <- ddply(mvt_gross,.(id),summarize, gross_disp=sum(dist, na.rm=T))
mvt_gross_summary <- cbind(sum,file,trajectory)

trajectory.data.summary <- merge(trajectory.data.summary,mvt_gross_summary,by=c("id"))
trajectory.data.summary$NGDR <- trajectory.data.summary$net_disp/trajectory.data.summary$gross_disp

trajectory.data.summary$file.y <- NULL
trajectory.data.summary$trajectory.y <- NULL

# 5. rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 25)

# 6. transform ltraj object into dataframe to extract movement metrics
mvt_summary <- ld(redis_space)

# split unique ID string into trajectory and file data for remerge with trajectory data
sum_mat <- as.matrix(sum[,1]) 
id_to_original <- t(as.data.frame(lapply(sum_mat[,1],function(x)strsplit(x,"-"))))
colnames(id_to_original) <- c("file","trajectory")



# analysis of turning angles time series
# extract cosine of the relative angles for further analysis
# cosrelangle <- redis_space[[2]]$rel.angle
# layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1))
# plot(cosrelangle, type="l", )
# plot(mvt_data[[2]]$x,mvt_data[[2]]$y)

# temporary plotting
ggplot(trajectory.data.summary, aes(x=trajectory.data.summary$NGDR, color=as.factor(trajectory.data.summary$file))) + geom_density()
