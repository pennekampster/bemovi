## Function to verify and stitch the trajectories of Link_particles

## in previous trajectory.data, we could find:
# one individuals with multiple trajectories or
# one trajectory for multiple individuals
# the goal here is to have one trajectory for one individuals

## on the script master.worker, at a new folder on the intermediate directories
# stitch.data.folder <- "6 - stitch data/"

## STEP 1: identify a change within trajectory
## STEP 2: create a new_trajectory 
## STEP 3: summarise morphological information to search similar individuals
## STEP 4: search individuals in other trajectories
## STEP 5: replace new trajectories to use them for overlays

stitch <- function(to.data, merged.data.folder,
                          stitch.data.folder, accuracy=.5, morphological_parameter, search_area=20){
    
## import data  
load(paste0(to.data,merged.data.folder,"Master.RData"))
# we import the trajectory.data 
tt <- trajectory.data


## STEP 1: identify a change within trajectory
# percentage of change between the frame n+1 and n, 
# for each morphological parameter. 

# morphological_parameter <- c("Area","Perimeter","Major")
# accuracy <- 0.5
 
percent_change <- function(dt){
    #dt <- tt[tt$id=="data00003-6",] 
    meta_data <-dt[,c("file","frame","trajectory","X","Y",morphological_parameter),with=F]
    frm   <- dt[1:(nrow(dt)-1),morphological_parameter,with=F]
    twrds <- dt[2:nrow(dt),morphological_parameter,with=F]  
    dfrnce  <- twrds-frm
    perc_change <- abs(dfrnce)/frm 

    zeros<-data.frame(t(rep(0,length(morphological_parameter))))
    names(zeros)<-names(perc_change)
    perc_change <- rbind(zeros,perc_change)    
    setnames(perc_change,
       old=morphological_parameter,
       new=paste0("diff_",morphological_parameter))
    out<-cbind(meta_data ,perc_change)
    return(out)
  }# end of percent_change function
 
# # for ONE video
#sub_tt <- tt[tt$file=="data00001",]
#setkey(sub_tt, id)
 
#new_tt <- sub_tt[,percent_change(.SD),by=list(id), .SDcols=c("file","frame","trajectory","X","Y",morphological_parameter)]



# for ALL video in the folder
setkey(tt, id)
new_tt <- tt[,percent_change(.SD), by=list(id), .SDcols=c("file","frame","trajectory","X","Y",morphological_parameter)]


# Comparison of previous morphological parameters to detect unexpeted change 
# (superior to "accuracy")
sub_new_tt <- new_tt[,grep("diff",colnames(new_tt)), with=F] 
TRUE_diff <- apply(sub_new_tt > accuracy , 1, function(x){sum(x==TRUE)})
new_tt[,"TRUE_diff"] <- ifelse(TRUE_diff != 0, "YES", "NO")


## STEP 2: Create a new_trajectory 
## using the max of trajectory for each file
new_tt[,"new_trajectory"] <- new_tt[,trajectory]
new_tt2 <- new_tt

# subset of mismatch with the previous 
yes_new_tt2 <- new_tt2[TRUE_diff=="YES"]
for (i in 1:nrow(yes_new_tt2)) {
  #i=1
  file.wanted <- yes_new_tt2[i,file]
  id.wanted <- yes_new_tt2[i,id]
  frame.wanted <- yes_new_tt2[i,frame]

  new_tt2[which(new_tt2$id==id.wanted & new_tt2$frame==frame.wanted):max(which(new_tt2$id==id.wanted)), "new_trajectory"] <- max(new_tt2[new_tt2$file==file.wanted,new_trajectory])+1
  }

# attribute a new identity
new_tt2[,"new_id"] <- paste(new_tt2[,file], new_tt2[,new_trajectory], sep="-")

library(plyr)
## STEP 3: summarise morphological information to search similar individuals
# summarize information to find correspondance between trajectories of the same organisms...
new_tt3 <- ddply(as.data.frame(new_tt2), .(new_id), summarize, 
            old_id = unique(id) ,    
            trajectory = unique(trajectory),
            file = unique(file),
            frame_start=unique(min(frame)),
            frame_end=unique(max(frame)),
            X_start=unique(X)[1],
            X_end=unique(X)[max(length(unique(X)))],
            Y_start=unique(Y)[1],
            Y_end=unique(Y)[max(length(unique(Y)))],
            meanArea=mean(Area),
            meanPerimeter=mean(Perimeter),
            meanMajor=mean(Major),
            new_trajectory=unique(new_trajectory))
new_tt3 <- new_tt3[order(new_tt3$file, new_tt3$frame_start, new_tt3$frame_end),]
new_tt3 <- as.data.table(new_tt3)


## STEP 4: search individuals in other trajectories


for (k in 1:length(unique(new_tt3$new_id))){
  # k<- 2# example 
  sub_new_tt3<-subset(new_tt3, new_id==unique(new_tt3$new_id)[k])
  other <- subset(new_tt3, file==sub_new_tt3$file
                  & frame_start > sub_new_tt3$frame_end 
                  & X_start <= sub_new_tt3$X_end + search_area
                  & X_start >= sub_new_tt3$X_end - search_area
                  & Y_start <= sub_new_tt3$Y_end + search_area
                  & Y_start >= sub_new_tt3$Y_end - search_area
                  & new_tt3$meanArea < sub_new_tt3$meanArea + (sub_new_tt3$meanArea*accuracy)
                  & new_tt3$meanArea > sub_new_tt3$meanArea - (sub_new_tt3$meanArea*accuracy)
                  & new_tt3$meanPerimeter < sub_new_tt3$meanPerimeter + (sub_new_tt3$meanPerimeter*accuracy)
                  & new_tt3$meanPerimeter > sub_new_tt3$meanPerimeter - (sub_new_tt3$meanPerimeter*accuracy)
                  & new_tt3$meanMajor < sub_new_tt3$meanMajor + (sub_new_tt3$meanMajor*accuracy)
                  & new_tt3$meanMajor > sub_new_tt3$meanMajor - (sub_new_tt3$meanMajor*accuracy))
  
  if(nrow(other)!=0)
    ## sometimes, possible correspondances are possible but they concern only very small particles. 
    ## Thus we choose the first one, subjectively, because followers will be chosen next in the loop.
    #traj2[which(traj2$id==other[1,"id"]),"new_trajectory"]<- unique(sub_new_tt3$new_trajectory)  
  new_tt3[which(new_tt3$new_id==other[1,new_id]),"new_trajectory"]<- unique(sub_new_tt3$new_trajectory)
  
}## loop k
new_tt3 <- new_tt3[order(new_tt3$new_trajectory, new_tt3$frame_start),]

## STEP 5: replace new trajectories ... 
real_tt <- tt
real_tt$new_traj <- NA
real_tt <- as.data.frame(real_tt)

for(i in 1:nrow(new_tt3)){
  # i=2  #example
  id.wanted <- new_tt3[i,]$old_id
  frame_start <- new_tt3[i,]$frame_start
  frame_end <- new_tt3[i,]$frame_end
  new_trajectory <- new_tt3[i,]$new_trajectory
  
  sub_real_tt <- real_tt[which(real_tt$id==id.wanted & real_tt$frame >= frame_start & real_tt$frame <= frame_end),]
  real_tt[which(real_tt$id==id.wanted & real_tt$frame >= frame_start & real_tt$frame <= frame_end),]$new_traj <- rep(new_trajectory, nrow(sub_real_tt))
  } 

# replace the 
real_tt$trajectory <- real_tt$new_traj
real_tt2 <- subset(real_tt, select=-new_traj)

# save the new file
trajectory.data <- as.data.table(real_tt2)
dir.create(paste0(to.data,stitch.data.folder),showWarnings=F)
#traj28 <- trajectory.data[which(trajectory.data$trajectory=="28"),]
save(trajectory.data, file=paste0(to.data, stitch.data.folder,"Master.RData"))

} #end stitch function












