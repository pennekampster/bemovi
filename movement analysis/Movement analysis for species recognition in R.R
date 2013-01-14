# code to extract movement characteristics from the results files of the ParticleTracker
# still contains code to extract positions from raw text file
# uses the adehabitatLT package to calculate movement metrics (and trials to explore functionality of the package)

library(lattice)
library(ggplot2)
library(adehabitatLT)
library(grid)
library(plyr)

text <- readLines("C:\\Video analysis for species recognition\\selected trajects\\Traj_Data45.avi.txt")
string <- "(^[[:digit:]]+)"

#find lines which match the string
txtsubset <-  grep(string, text) 

#subset the original file by only retaining lines that matched the string
out <- text[txtsubset]

#save resulting text file after cleaning
writeLines(out,con="c:\\users\\Frank\\documents\\file.txt",sep="\n")

csv <- read.csv("c:\\users\\Frank\\documents\\file.txt", header=FALSE, sep=" ")

lag <- csv[2:nrow(csv),1]
lag <- append(lag,1)
final <- cbind(csv,lag)

# assign a counter creating a unique trajectory
trajectory <- 1
for (i in 1:nrow(final)){
  if (final[i,11]>final[i,1]){
      final[i,"trajectory"] <- trajectory}
  
  if (final[i,11]<=final[i,1]){
      final[i,"trajectory"] <- trajectory
      trajectory <- trajectory+1}
} 

# invert Y-axis to account for origin in upper-left corner
final$V2 <- -final$V2

#rename drop unused information and rename variables
final$lag <- NULL
final <- final[c(-4,-5,-6,-7,-8,-9,-10)]
final$trajectory <- final$trajectory
names(final) <- c("frame","X","Y", "trajectory")



#calculate summary stats (count of frames) and merge with original data for trajectory selection
count <- tapply(final$frame, final$trajectory, function(x) length(unique(x)))
count <- as.data.frame(cbind(count,unique(final$trajectory)))
names(count) <- c('frame_total','trajectory')
final_summary <- merge(count,final,by=c("trajectory"))

minimum <- ddply(final_summary, .(trajectory), .fun = function(a){a[which.min(a$frame), ]})
maximum <- ddply(final_summary, .(trajectory), .fun = function(a){a[which.max(a$frame), ]})

minimum <- minimum[c(-2)]
names(minimum) <- c("trajectory","frame_start","X_start","Y_start")

maximum <- maximum[c(-2)]
names(maximum) <- c("trajectory","frame_end","X_end","Y_end")

net_disp <- merge(minimum,maximum,by=c("trajectory"))

net_disp$net_disp <- round(sqrt((net_disp$X_end - net_disp$X_start)^2 + (net_disp$Y_end - net_disp$Y_start)^2))
#net speed in pixels/second
net_disp$duration <- (net_disp$frame_end - net_disp$frame_start)/27
net_disp$net_speed <- net_disp$net_disp/net_disp$duration
final_summary <- merge(final,net_disp,by=c("trajectory"))
final_summary <- subset(final_summary, final_summary$net_speed > 20 & final_summary$net_disp > 10)


#plot trajectories
ggplot(final_summary, aes(x=final_summary$X, y=final_summary$Y, color=as.factor(final_summary$trajectory))) +
geom_point(shape=19, size = 0.1, alpha=0.5) +  
theme_bw() +
coord_fixed(xlim=c(-2048,0),ylim=c(0,2048))+
theme(panel.grid.minor=element_blank(), 
      panel.grid.major=element_blank(),
      axis.text.x  = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y  = element_blank(), 
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      plot.background=element_blank(),
      panel.border = element_blank(),
      panel.margin = unit(0,"cm"),
      plot.margin = rep(unit(0,"cm"),4),
      axis.ticks.length = unit(0.001,"cm"),
      axis.ticks.margin = unit(0.001,"cm"),
      legend.position="none") +
scale_x_continuous(expand=c(0,0)) + 
scale_y_continuous(expand=c(0,0))

#subset data for movement analysis
final <- subset(final_summary, (trajectory < 50  ))

final$sec <- final$frame  
final$da <- as.POSIXct(final$sec, origin = "1900-01-01", format = "%OS")
options(digits.secs=2)

# convert raw data in ltraj class (typeII=FALSE meaning that no time was recorded)
mvt_data <- as.ltraj(xy = final[,c("X","Y")], date = final$da, typeII=TRUE, id = final$trajectory)

# add positions where locations should have been measured, but have not due to detection problems ..
mvt_data <- setNA(mvt_data, date.ref = final$da, dt=1, units=c("sec"))

#check whether adding positions rendered trajectory regular
#is.regular(mvt_data)

#to interpolate the positions which are missing, put the time interval (dt) to 1 
mvt_data <- redisltraj(na.omit(mvt_data), 1, type="time")

# plot cell trajectories
plot(mvt_data)

#temporal subsampling to get regular trajectories and avoid temporal autocorrelation
#mvt_data <- redisltraj(na.omit(mvt_data), 5, type="time")

#rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 25)
plot(redis_space)

#sliding window to smoothen values (does not work when trajectories have less locations than specified for the step)
#sliwinltr(redis_space, function(x) mean(cos(x$rel.angle)), type="locs", step=2)

#extract cosine of the relative angles for further analysis
cosrelangle <- redis_space[[2]]$rel.angle
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(1,1))
plot(cosrelangle, type="l", )
plot(mvt_data[[2]]$x,mvt_data[[2]]$y)

#dynamic exploration of the trajectory (does not work with RStudio)
#trajdyn(mvt_data)

#check for autocorrelation in distance parameters
wawotest(mvt_data)

#autocorrelation function of distance
acfdist.ltraj(mvt_data, which = c("dist"), nrep = 999, lag = 10, plot = TRUE, xlab = "Lag", ylab = "autocorrelation")

#autocorrelation function of turning angles
acfang.ltraj(mvt_data, which = c("relative"), nrep = 999, lag = 10, plot = TRUE, xlab = "Lag", ylab = "autocorrelation")

testang.ltraj(mvt_data, "relative")

#function to quickly transform ltraj object into dataframe 
# -> easy way to get turning angles and move lengths
mvt_data_df <- ld(mvt_data)

#plot characteristics of single trajectories
plotltr(mvt_data, "R2n")

# access ltraj object instances i.e. single trajectories
plot(mvt_data[[3]]$x,mvt_data[[3]]$y)

