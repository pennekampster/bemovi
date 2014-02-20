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
  return(tempangle*c)
}

bearing.ta <- function(loc1,loc2,loc3,as.deg=FALSE){
  ## calculates the bearing and length of the two lines
  ##    formed by three points
  ## the turning angle from the first bearing to the
  ##    second bearing is also calculated
  ## locations are assumed to be in (X,Y) format.
  ## Options:
  ## as.deg = TRUE returns degrees instead of radians
  if (length(loc1) != 2 | length(loc2) != 2 | length(loc3) !=2){
    print("Locations must consist of either three vectors, length == 2,
or three two-column dataframes")
    return(NaN)
  }
  c = 1
  if (as.deg){
    c = 180/pi
  }
  
  locdiff1<-loc2-loc1
  locdiff2<-loc3-loc2
  bearing1<-anglefun(locdiff1[1],locdiff1[2],bearing=F)
  bearing2<-anglefun(locdiff2[1],locdiff2[2],bearing=F)
  
  if(is.data.frame(locdiff1)){
    dist1<-sqrt(rowSums(locdiff1^2))
    dist2<-sqrt(rowSums(locdiff2^2))
  }else{
    dist1<-sqrt(sum(locdiff1^2))
    dist2<-sqrt(sum(locdiff2^2))
  }
  
  ta=(bearing2-bearing1)
  
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi
  return(list(bearing1=unlist(bearing1*c),bearing2=unlist(bearing2*c),
              ta=unlist(ta*c),dist1=unlist(dist1),dist2=unlist(dist2)))
}

# shortened function to calculate turning angles from absolute angles
rel.angle <- function(abs.angle){
  obs <- length(abs.angle)
  ta <- abs.angle[2:(obs-1)]-abs.angle[1:(obs-2)]
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi
  ta <- c(-9999,ta,-9999)
  ta[ta == -9999] <- NA 
  return(ta)
}


