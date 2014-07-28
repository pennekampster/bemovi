# define the functions to calculate movement parameters
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
    ta <- as.numeric(ta)
    return(ta)}
  
  else {
    obs <- (length(abs.angle)+1)
    ta <- rep(NA,obs)
    ta <- as.numeric(ta)
    return(ta)}
}

step_length <- function(x,y){
  
  # function to extract step lengths from X- & Y-coordinates
  
  step <- sqrt(diff(x)^2+diff(y)^2)  
  step <- c(step,-9999)
  step[step == -9999] <- NA
  return(step)  
  
}

step_duration <- function(frame_){
  
  # function to extract step duration for each step
  step_duration <- diff(frame_)
  step_duration <- c(step_duration,-9999)
  step_duration[step_duration == -9999] <- NA
  return(step_duration)  
  
}


net_displacement <- function(x,y){
  
  # function to calculate the net squared displacement for each step
  start_x <- x[1]
  start_y <- y[1]
  R2n <- (start_x-x)^2+(start_y-y)^2
  return(R2n)
  
}
