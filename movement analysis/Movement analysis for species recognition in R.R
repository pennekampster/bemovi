rm(list=ls())

library(adehabitatLT)
library(grid)
library(plyr)
library(ggplot2)

# code to extract movement characteristics from the results files of the ParticleTracker
# uses the adehabitatLT package to calculate movement metrics (and trials to explore functionality of the package)
# 1. calculates summary statistics on net and gross movement
# 2. transforms the raw data (X and Y positions in to ltraj object to extract mean and SD of turning angles)
# 3. aggregates statistics on movement with trajectory ID and file name for species classification

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

# 0. prepare data for analysis with adehabitat package
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

# get gross displacement and STD of turning angles and merge with original data
mvt_gross <- ld(mvt_data)
sum <- ddply(mvt_gross, .(id), summarize, gross_disp=sum(dist, na.rm=T))

# split unique ID string into trajectory and file data for remerge with trajectory data
sum_mat <- as.matrix(sum[,1:2]) 
id_to_original <- t(as.data.frame(lapply(sum_mat[,1],function(x)strsplit(x,"-"))))
id <- paste(id_to_original[, 1],id_to_original[, 2],sep="-")
id_to_original <- cbind(id_to_original,id)
colnames(id_to_original) <- c("file","trajectory","id")
mvt_gross_summary <- merge(sum,id_to_original,by=c("id"))

trajectory.data.summary <- merge(net_disp_summary,mvt_gross_summary,by=c("file","trajectory"))
trajectory.data.summary$NGDR <- trajectory.data.summary$net_disp/trajectory.data.summary$gross_disp

# 5. rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 10)

# 6. transform ltraj object into dataframe to extract movement metrics
mvt_summary <- ld(redis_space)
turning <- ddply(mvt_summary, .(id), summarize, mean_turning=mean(rel.angle, na.rm=T))
sd_ta <- ddply(mvt_summary, .(id), summarize, sd_turning=sd(rel.angle, na.rm=T))
turning_summary <- cbind(sd_ta,turning)
turning_summary[,3] <- NULL
turning_summary <- merge(turning_summary,id_to_original,by=c("id"))

trajectory.data.summary <- merge(trajectory.data.summary,turning_summary,by=c("id"))
trajectory.data.summary$datetime <- NULL

trajectory.data.summary$file.y <- NULL
trajectory.data.summary$trajectory.y <- NULL

# 7. extracting autocorrelation structure
# analysis of turning angles time series
for (i in 1:length(redis_space)){
relangle <- redis_space[[i]]$rel.angle
acf_object <- acf(relangle,na.action=na.pass, plot=FALSE)
if (i == 1){dd <- as.data.frame(acf_object$acf)
            dd$lag <- seq(1:length(acf_object$acf))    
}

if (i> 1){dd.t <- as.data.frame(acf_object$acf)
          dd.t$lag <- seq(1:length(acf_object$acf))
dd <- rbind(dd,dd.t)}
}
redis_space[[1]]


# plotting of autocorrelation function
#acf(relangle,na.action=na.pass)
#layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1))
#plot(relangle, type="l", )
#plot(mvt_data[[14]]$x,mvt_data[[14]]$y,asp=1)



subset <- subset(trajectory.data, trajectory.data$file == "Traj_Data34.avi.txt" |
                                  trajectory.data$file == "Traj_Data38.avi.txt" |
                                  trajectory.data$file == "Traj_Data49.avi.txt")

subset34 <- subset(trajectory.data, trajectory.data$file == "Traj_Data34.avi.txt")
subset38 <- subset(trajectory.data, trajectory.data$file == "Traj_Data38.avi.txt")
subset49 <- subset(trajectory.data, trajectory.data$file == "Traj_Data49.avi.txt")

# temporary plotting
ggplot(subset49,aes(x=subset49$Y,y=subset49$X)) + geom_point()
ggplot(trajectory.data.summary, aes(x=trajectory.data.summary$sd_turning, color=as.factor(trajectory.data.summary$file))) + geom_density()

# use a PCA to visualize whether species can be separated by PCs of movement
fit <- princomp(trajectory.data.summary[, 4:10], cor=TRUE)
PC1 <- fit$scores[,1]
PC2 <- fit$scores[,2]

plot_PCA <- cbind(PC1,PC2,trajectory.data.summary[2:3])
plot(plot_PCA$PC1,plot_PCA$PC2, col=plot_PCA$file.x)
