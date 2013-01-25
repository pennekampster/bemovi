# code to extract movement characteristics from the results files of the ParticleTracker
# still contains code to extract positions from raw text file
# uses the adehabitatLT package to calculate movement metrics (and trials to explore functionality of the package)
library(adehabitatLT)
library(grid)
library(plyr)

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

#subset data for movement analysis
trajectory.data.summary.test <- subset(trajectory.data.summary, file == "Traj_Data34.avi.txt" & trajectory == 73)
trajectory.data.summary.test
trajectory.data.summary <- subset(trajectory.data.summary, (net_speed > 50 &  net_speed < 300) & net_disp > 50)
unique(trajectory.data.summary.test$trajectory)

attach(trajectory.data.summary.test)
plot(Y, X+as.numeric(height), xlim=c(0,as.numeric(width)), ylim=c(0,as.numeric(height)), pch=15, cex=0.5, asp=1, col=net_speed)
detach(trajectory.data.summary_subset)

trajectory.data.summary$sec <- trajectory.data.summary$frame  
trajectory.data.summary$datetime <- as.POSIXct(trajectory.data.summary$sec, origin = "1900-01-01", format = "%OS")
trajectory.data.summary$sec <- NULL
options(digits.secs=2)

# convert raw data in ltraj class (typeII=FALSE meaning that no time was recorded)
mvt_data <- as.ltraj(xy = trajectory.data.summary[,c("X","Y")], date = trajectory.data.summary$datetime, typeII=TRUE, id = trajectory.data.summary$trajectory)

# add positions where locations should have been measured, but have not due to detection problems ..
mvt_data <- setNA(mvt_data, date.ref = trajectory.data.summary$datetime, dt=1, units=c("sec"))

#check whether adding positions rendered trajectory regular
is.regular(mvt_data)

#to interpolate the positions which are missing, put the time interval (dt) to 1 
mvt_data <- redisltraj(na.omit(mvt_data), 1, type="time")

#rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 25)


#extract cosine of the relative angles for further analysis
cosrelangle <- redis_space[[2]]$rel.angle
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1))
plot(cosrelangle, type="l", )
plot(mvt_data[[2]]$x,mvt_data[[2]]$y)


