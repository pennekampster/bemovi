# function to subsample by a specified interval from the trajectory data

temporal_simplification <- function(df,phi){
  df <- df[df$frame == min(df$frame) | df$frame %% phi  == 0  | df$frame == min(df$frame), ] 
}

anglefun <- function(xx,yy,bearing=TRUE,as.deg=FALSE){
  
  ## http://quantitative-ecology.blogspot.ch/2007/05/anglefun-function-xxyy-bearing-true-as.html
  ## function changed such that angles are reported with the x-axis as North (see comment thread)
  ## calculates the compass bearing of the line between two points
  ## xx and yy are the differences in x and y coordinates between two points
  ## Options:
  ## bearing = FALSE returns +/- pi instead of 0:2*pi
  ## as.deg = TRUE returns degrees instead of radians
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  b<-sign(yy)
  b[b==0]<-1  #corrects for the fact that sign(0) == 0
  tempangle = b*(xx<0)*pi+atan(yy/xx)
  
  
  if(bearing){
    #return a compass bearing 0 to 2pi
    #if bearing==FALSE then a heading (+/- pi) is returned
    tempangle[tempangle<0]<-tempangle[tempangle<0]+2*pi
  }
  tempangle <- tempangle*c
  tempangle <- c(tempangle,-9999)
  tempangle[tempangle==-9999] <- NA
  return(tempangle)
}


# # funtion not used!!!
# bearing.ta <- function(loc1,loc2,loc3,as.deg=FALSE){
#   ## calculates the bearing and length of the two lines
#   ##    formed by three points
#   ## the turning angle from the first bearing to the
#   ##    second bearing is also calculated
#   ## locations are assumed to be in (X,Y) format.
#   ## Options:
#   ## as.deg = TRUE returns degrees instead of radians
#   if (length(loc1) != 2 | length(loc2) != 2 | length(loc3) !=2){
#     print("Locations must consist of either three vectors, length == 2,
# or three two-column dataframes")
#     return(NaN)
#   }
#   c = 1
#   if (as.deg){
#     c = 180/pi
#   }
#   
#   locdiff1<-loc2-loc1
#   locdiff2<-loc3-loc2
#   bearing1<-anglefun(locdiff1[1],locdiff1[2],bearing=F)
#   bearing2<-anglefun(locdiff2[1],locdiff2[2],bearing=F)
#   
#   if(is.data.frame(locdiff1)){
#     dist1<-sqrt(rowSums(locdiff1^2))
#     dist2<-sqrt(rowSums(locdiff2^2))
#   }else{
#     dist1<-sqrt(sum(locdiff1^2))
#     dist2<-sqrt(sum(locdiff2^2))
#   }
#   
#   ta=(bearing2-bearing1)
#   
#   ta[ta < -pi] = ta[ta < -pi] + 2*pi
#   ta[ta > pi] = ta[ta > pi] - 2*pi
#   return(list(bearing1=unlist(bearing1*c),bearing2=unlist(bearing2*c),
#               ta=unlist(ta*c),dist1=unlist(dist1),dist2=unlist(dist2)))
# }

# shortened function to calculate turning angles from absolute angles
rel.angle <- function(abs.angle){
 
  abs.angle <- na.omit(abs.angle)
  
  if (length(abs.angle)>2){
  obs <- length(abs.angle)
  ta <- abs.angle[2:(obs)]-abs.angle[1:(obs-1)]
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi
  ta <- c(-9999,ta,-9999)
  ta[ta == -9999] <- NA
  return(ta)}
  
  else {
  obs <- (length(abs.angle)+1)
  ta <- rep(NA,obs)
  return(ta)}
}

step_length <- function(x,y){

# function to extract step lengths from X- & Y-coordinates
  
  step <- sqrt(diff(x)^2+diff(y)^2)  
  step <- c(step,-9999)
  step[step == -9999] <- NA
  return(step)  
  
}

net_displacement <- function(x,y){

# function to calculate the net squared displacement for each step
  
   start_x <- x[1]
   start_y <- y[1]
   R2n <- (start_x-x)^2+(start_y-y)^2
   return(R2n)

}

calculate_mvt <- function(data){
  
  
  library(circular)
  library(CircStats)
  library(dplyr)
  library(sqldf)  

  data_full <- data
  
  # create unique ID consisting of trajectory ID and file
  id <- paste(data$file,data$trajectory,sep="-")
  data <- cbind(data,id)  
  
  #order dataframe
  data <- data[order(data$file,data$trajectory,trajectory.data$frame), ]
  
  # filter out single coordinates which do not form trajectories
  data <- data[!is.na(data$trajectory),]
  
  #filter out duplicate positions, if available
  data <- data[-which(diff(data$X) == 0 & diff(data$Y) == 0),]
  
  #subset dataset to only include relevant movement information
  data <- data[,c("file","X","Y","frame","id","trajectory")]
  
  # extract movement metrics
#   mvt_summary <- ddply(data, .(id), mutate, step_length = step_length(X,Y),
#                        gross_disp = cumsum(step_length),
#                        net_disp = net_displacement(X,Y),
#                        abs_angle = anglefun(diff(X),diff(Y)),
#                        rel_angle = rel.angle(anglefun(diff(X),diff(Y))))
  
  mvt_summary <- data %.%
                 group_by(id) %.%
                 mutate(step_length = step_length(X,Y),
                        gross_disp = cumsum(step_length),
                        net_disp = net_displacement(X,Y),
                        abs_angle = anglefun(diff(X),diff(Y)),
                        rel_angle = rel.angle(anglefun(diff(X),diff(Y))))

  data <- sqldf("select t.*, step_length, net_disp, abs_angle, rel_angle  
                from data_full t 
                left join mvt_summary m
                on t.id=m.id AND t.frame=m.frame")
  
  return(data)
}



