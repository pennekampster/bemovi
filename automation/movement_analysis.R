# code to extract movement characteristics from the results files of the ParticleTracker
# uses the adehabitatLT package to calculate movement metrics 
# 1. calculates summary statistics on net and gross movement
# 2. transforms the raw data (X and Y positions in to ltraj object to extract mean and SD of turning angles)
# 3. aggregates statistics on movement and stores data for further classification

filter_trajects <- function(trajectory.data){
# function to filter trajectories before analysis
# importantly, trajectories must comply to minimum net displacement otherwise problems
# occur later on with trajectory simplification

# required libraries
library(sqldf)
library(plyr)
#library(grid)
#library(ggplot2)
#library(reshape)
  
# 0. prepare data for analysis with adehabitat package
# create unique ID consisting of trajectory ID and file
id <- paste(trajectory.data$file,trajectory.data$trajectory,sep="-")
trajectory.data <- cbind(trajectory.data,id)

#subset dataset to only include relevant movement information
trajectory.data <- trajectory.data[,c("file","X","Y","frame","id","trajectory")]

#calculate summary stats (count of frames) to perform trajectory selection
start_frame <- sqldf("select min(frame) as start_frame, X, Y, file, id from 'trajectory.data' group by id")
end_frame <- sqldf("select max(frame) as end_frame, X, Y, file, id from 'trajectory.data' group by id")

fixes_count <- sqldf("select count(frame) as count, id from 'trajectory.data' group by id")

traject_features <- sqldf("select s.id, 
                          round(sqrt(power((e.X - s.X),2) + power((e.Y - s.Y),2))) as net_disp,
                          (e.end_frame - s.start_frame)+1 as frame_range
                          from start_frame as s, end_frame as e
                          where s.id=e.id")

traject_features <- merge(traject_features,fixes_count,by=c("id"))
traject_features$net_speed <- traject_features$net_disp/traject_features$frame_range

# detection rate quantifies how often an object was detected given the number of frames it spans (1 = 100%)
traject_features$detection_rate <- traject_features$count/traject_features$frame_range
traject_features$detection_rate <- traject_features$count/traject_features$frame_range

# filter very short trajectories out, otherwise problems when rediscretizing based on distance
trajectory.data <- sqldf("select t.*
                from 'trajectory.data' t, traject_features c
                where t.id=c.id AND count > 0 AND net_speed > 1 AND detection_rate > 0.7 AND net_disp > 100")

# drop factor levels of id
trajectory.data$id <- factor(trajectory.data$id)

assign("trajectory.data",trajectory.data,envir = .GlobalEnv)
}



# plot effect of filtering
#trajectory.data <- subset(trajectory.data)
#plot(trajectory.data$Y, trajectory.data$X+2048, xlim=c(0,2048), ylim=c(0,2048), col="red", pch=1, cex=1.5, asp=1)
#par(new=T)
#plot(trajectory.data.summary$Y, trajectory.data.summary$X+2048, xlim=c(0,2048), ylim=c(0,2048), col="#FFFF00", pch=1, cex=1, asp=1)

extract_movement <- function(trajectory.data){
# Function to extract movement metrics from filtered trajectories

# required libraries
library(adehabitatLT)
library(circular)

# 1. convert frame into time class (each frame representing a time step of a second)
trajectory.data$sec <- trajectory.data$frame  
trajectory.data$datetime <- as.POSIXct(trajectory.data$sec, origin = "1900-01-01", format = "%OS")
trajectory.data$sec <- NULL
options(digits.secs=2)

# 2. convert raw data in ltraj class (typeII=FALSE meaning that no time was recorded)
mvt_data <- as.ltraj(xy = trajectory.data[,c("X","Y")], date = trajectory.data$datetime, typeII=TRUE, id = trajectory.data$id)

# 3. add positions where locations should have been measured (i.e. each second), but have not due to e.g. detection problems...
mvt_data <- setNA(mvt_data, date.ref = trajectory.data$datetime, dt=1, units=c("sec"))

# optional: check whether adding positions rendered trajectory regular
# is.regular(mvt_data)

# 4. interpolate the positions which are missing (added as missing values before [see 3.]) by putting the time interval (dt) to 1 
mvt_data <- redisltraj(na.omit(mvt_data), 1, type="time")
mvt_summary <- ld(mvt_data)

# get gross & net displacement before rediscretizing the trajectories
gross_disp <- ddply(mvt_summary, .(id), summarise, gross_disp = sum(dist, na.rm=TRUE), 
                    max_date = max(date, na.rm = TRUE),
                    min_date = min(date, na.rm = TRUE))
gross_disp$frame_count <- gross_disp$max_date - gross_disp$min_date
gross_disp$max_date <- NULL
gross_disp$min_date <- NULL

net_disp <-   sqldf("select id, sqrt(R2n) as net_disp
                  from mvt_summary 
                  group by id
                  having date = max(date)")

disp <- merge(net_disp,gross_disp,by=c('id'))

# 5. rediscretize the trajectory in space to analyze geometrical properties of the trajectory
redis_space <- redisltraj(mvt_data, 50, nnew = 10)

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

# reset factor id and burst
mvt_summary2$id <- factor(mvt_summary2$id)
mvt_summary2$burst <- factor(mvt_summary2$burst)

# calculate the mean and Sd of the turning angles (in radians)
turning <- ddply(mvt_summary2[!is.na(mvt_summary2$rel.angle),], .(id), summarize, mean_turning=circ.mean(rel.angle))
sd_ta <- ddply(mvt_summary2, .(id), summarize, sd_turning=sd.circular(rel.angle, na.rm=T))
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
trajectory.data.summary <- sqldf("select d.id, d.net_disp, d.gross_disp,
                                  net_disp / gross_disp as NGDR,
                                  net_disp / frame_count as net_speed,
                                  gross_disp / frame_count as gross_speed,        
                                  t.mean_turning, t.sd_turning,
                                  t.period
                                  from disp d
                                  left join ta_summary t
                                  on d.id=t.id")

# provide output with trajectory and file information
trajectory.data.summary <- sqldf("select DISTINCT t.*, f.file, f.trajectory
                                  from 'trajectory.data.summary' t
                                  left join 'trajectory.data' f
                                  on t.id=f.id")

# create folder to save results
dir.create(paste0(to.data,merged.data.folder), showWarnings = FALSE)  

# export aggregated data on movement
write.table(trajectory.data.summary, file = paste(paste0(to.data,merged.data.folder),"trajectory.data.summary.txt", sep = "/"), sep = "\t")
}


