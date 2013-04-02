library(plyr)
library(sqldf)
library(randomForest)

# code to do classification based on both morphology and movement data
# will integrate all of OWen's previous ideas and re-use available codes

## Owen's paths
to.code.owen <- "/Users/owenpetchey/work/git/franco/automation/"
to.data.owen <- "/Users/owenpetchey/Desktop/hard.test/"

## Frank's paths
to.code.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/automation/"
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/data/"

## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
  to.code <- to.code.frank
  to.data <- to.data.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
  to.code <- to.code.owen
  to.data <- to.data.owen}

# specify folder for saving the predictions (jpeg plots) and the video overlays
raw.video.folder <- "1 - raw/"
trajectory.data.folder <- "2 - trajectory data/"
class.data <- "6 - Classification/"
prediction.folder <- "7 - Prediction/"
prediction.folder2 <- "8 - Prediction overlay/"

# import morphology data
morphology.data <- read.table(paste0(to.data,class.data,"morphology.data.summary.txt"), sep = "\t")

# import trajectory data
trajectory.data <- read.table(paste0(to.data,class.data,"trajectory.data.summary.txt"), sep = "\t")

#merge morphology data with meaningful trajectory data (meaningful = show properties used for filter trajectories)
all_data <- sqldf("select *
                  from 'trajectory.data' t, 'morphology.data' m
                  where t.file=m.file AND t.trajectory=m.trajectory
                  ")

# labelling files according to species
all_data$species <- factor(all_data$file,
                    levels = c("Data34","Data38","Data45","Data49","Data52","Data57"),
                    labels = c("Colpidium", "Paramecium", "Colpidium&Paramecium","Loxocephalus","Loxocephalus&Colpidium","Colpidium&Paramecium&Loxocephalus")) 

# only take monocultures
species_subsets <- sqldf("select *
                         from all_data
                         where species in ('Colpidium', 'Paramecium', 'Loxocephalus')")


# take monocultures of three species and classify by morphology (excluding the grey value for the moment)
three_moncultures <- sqldf("select *
                       from all_data
                       where species in ('Colpidium','Paramecium', 'Loxocephalus')")

# define classification table function
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}

# test randomForest classification for species recognition
# use factor function to only classify species comprised in the dataset
train_rf <- cbind(three_moncultures[, c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21)])
samp <- sample(1:length(train_rf[,1]), length(train_rf[,1])/2)
no.samp <- c(1:length(train_rf[,1]))[-samp]
# train randomForest
rf_fit <- randomForest(factor(species) ~ net_speed + period + NGDR + sd_turning + grey + area + perimeter + major + minor + AR , data=train_rf[samp,], importance=TRUE, proximity=TRUE)
print(rf_fit)
plot(rf_fit)
varImpPlot(rf_fit)
MDSplot(rf_fit, train_rf[samp,]$species)
table(factor(train_rf[no.samp,]$species), predict(rf_fit, train_rf[no.samp,]))

# use monoculture to predic species identity in communities
three_monocult <- sqldf("select *
                         from all_data
                         where species in ('Colpidium','Paramecium','Loxocephalus')")

mixed_cult <- sqldf("select *
                    from all_data
                    where species in ('Colpidium&Paramecium', 'Loxocephalus&Colpidium', 'Colpidium&Paramecium&Loxocephalus')")

# classification by randomForest
train_rf <- cbind(three_monocult[, c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21)])
rf_fit <- randomForest(factor(species) ~ net_speed + period + NGDR + sd_turning + grey + area + perimeter + major + minor + AR , data=train_rf, importance=TRUE, proximity=TRUE)
predict_spec <- predict(rf_fit, mixed_cult[, c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21)])
predict <- cbind(mixed_cult,predict_spec)

# visualize prediction
# refilter trajectories with complete data
trajectory.data <- read.table(paste0("C:/Users/Frank/Documents/PhD/Programming/franco/data/2 - trajectory data/trajectory.data.txt"), header=TRUE, sep="\t")
filter_trajects(trajectory.data)
trajectory_raw <- trajectory.data
trajectory_raw$id <- paste(trajectory_raw$file,trajectory_raw$trajectory,sep="-")

predict_visual <- sqldf("select t.*, p.predict_spec 
                        from trajectory_raw t
                        left join predict p 
                        on p.id=t.id
                        where t.file in ('Data45','Data52','Data57')")

# rename NAs from sql merge as "unknown"
predict_visual[is.na(predict_visual)] <- "unknown"
predict_visual$predict_spec <- factor(predict_visual$predict_spec)

# function to create overlays
source(paste(to.code, "batch_process_videos.r", sep=""))
create_prediction_plots(to.data,735,690,10)

# extract summary stats on species counts and trait destributions per species
summary_counts <- ddply(predict_visual, .(predict_spec,file,frame), summarise, count = length(predict_spec))
summary_means <- ddply(summary_counts, .(predict_spec,file), summarise, mean_count = mean(count))

# visualize
library(ggplot2)
ggplot(predict, aes(x=predict$area, color=predict$predict_spec)) + geom_density()
ggplot(predict, aes(x=predict$grey, color=predict$predict_spec)) + geom_density()
ggplot(predict, aes(x=predict$major, color=predict$predict_spec)) + geom_density()
ggplot(predict, aes(x=predict$perimeter, color=predict$predict_spec)) + geom_density()
ggplot(predict, aes(x=predict$circularity, color=predict$predict_spec)) + geom_density()
ggplot(predict, aes(x=predict$AR, color=predict$predict_spec)) + geom_density()

# 2. movement characteristics
ggplot(predict, aes(x=predict$net_speed, color=as.factor(predict$predict_spec))) + geom_density()
ggplot(predict, aes(x=predict$NGDR, color=as.factor(predict$predict_spec))) + geom_density()
ggplot(predict, aes(x=predict$sd_turning, color=as.factor(predict$predict_spec))) + geom_density()
ggplot(predict, aes(x=predict$period, color=as.factor(predict$predict_spec))) + geom_density()

# use a PCA to visualize whether species can be separated by morphology & movement
fit_move <- princomp(predict[, c(4,5,6,7,8,9)], cor=TRUE)
fit_morph <- princomp(predict[, c(14,15,16,17,18,19,20)], cor=TRUE)
PC1_morph <- fit_morph$scores[,1]
PC2_morph <- fit_morph$scores[,2]
PC1_move <- fit_move$scores[,1]
PC2_move <- fit_move$scores[,2]
plot_PCA_morph <- cbind(PC1_morph,PC2_morph,predict[c(2,3,21)])
plot_PCA_move <- cbind(PC1_move,PC2_move,predict[c(2,3,21)])
ggplot(plot_PCA_morph, aes(x=PC1_morph, y=PC2_morph, color=plot_PCA_morph$species)) + geom_point()
ggplot(plot_PCA_move, aes(x=PC1_move, y=PC2_move, color=plot_PCA_move$species)) + geom_point()



