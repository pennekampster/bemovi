library(ggplot2)
library(nnet)
library(sqldf)

# code to do classification based on both morphology and movement data
# will integrate all of OWen's previous ideas and re-use available codes

## Owen's paths
to.data.owen <- "/Users/owenpetchey/Desktop/franco.test.vids/merge morphology and trajectory/"

## Frank's paths
to.data.frank <- "C:/Users/Frank/Documents/PhD/Programming/franco/merge morphology and trajectory/"

## what OS are we on?
OS <- .Platform$OS.type
## if on windows, use Frank's paths
if(.Platform$OS.type == "windows"){
  to.data <- to.data.frank}
## otherwise use Owen's
if(.Platform$OS.type == "unix"){
  to.data <- to.data.owen}

# import morphology data
morphology.data <- read.table(paste0(to.data,"morphology.data.summary.txt"), sep = "\t")

# import trajectory data
trajectory.data <- read.table(paste0(to.data,"trajectory.data.summary.txt"), sep = "\t")

#merge morphology data with meaningful trajectory data (meaningful = show properties used for filter trajectories)
all_data <- sqldf("select *
                  from 'trajectory.data' t, 'morphology.data' m
                  where t.file=m.file AND t.trajectory=m.trajectory
                  ")

# labelling files according to species
all_data$species <- factor(all_data$file,
                    levels = c("Data34","Data38","Data45","Data49","Data52","Data57"),
                    labels = c("Colpidium", "Paramecium", "Colpidum&Paramecium","Loxocephalus","Loxocephalus&Colpidum","Colpidum&Paramecium&Loxocephalus")) 

# only take monocultures
species_subsets <- sqldf("select *
                         from all_data
                         where species in ('Colpidium', 'Paramecium', 'Loxocephalus')")

# temporary plotting of aggregate variables to check which have highest discrimination power
# 1. morphology
ggplot(species_subsets, aes(x=species_subsets$area, color=species_subsets$species)) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$grey, color=species_subsets$species)) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$major, color=species_subsets$species)) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$perimeter, color=species_subsets$species)) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$circularity, color=species_subsets$species)) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$AR, color=species_subsets$species)) + geom_density()

# 2. movement characteristics
ggplot(species_subsets, aes(x=species_subsets$net_speed, color=as.factor(species_subsets$species))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$NGDR, color=as.factor(species_subsets$species))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$sd_turning, color=as.factor(species_subsets$species))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$period, color=as.factor(species_subsets$species))) + geom_density()

# use a PCA to visualize whether species can be separated by morphology & movement
fit_move <- princomp(species_subsets[, c(4,5,6,7,8,9,10,11)], cor=TRUE)
fit_morph <- princomp(species_subsets[, c(14,15,16,17,18,19,20)], cor=TRUE)
PC1_morph <- fit_morph$scores[,1]
PC2_morph <- fit_morph$scores[,2]
PC1_move <- fit_move$scores[,1]
PC2_move <- fit_move$scores[,2]
plot_PCA_morph <- cbind(PC1_morph,PC2_morph,species_subsets[c(2,3,21)])
plot_PCA_move <- cbind(PC1_move,PC2_move,species_subsets[c(2,3,21)])
ggplot(plot_PCA_morph, aes(x=PC1_morph, y=PC2_morph, color=plot_PCA_morph$species)) + geom_point()
ggplot(plot_PCA_move, aes(x=PC1_move, y=PC2_move, color=plot_PCA_move$species)) + geom_point()

# ANN (based on morphological data)
# take monocultures of three species and classify by morphology (excluding the grey value for the moment)
three_moncultures <- sqldf("select *
                       from all_data
                       where species in ('Colpidium','Paramecium', 'Loxocephalus')")

train <- cbind(three_moncultures[, c(14,16,17,18,19,20)])
target <- class.ind(three_moncultures[,"species"])
target <- target[, c(1,2,4)]
samp <- sample(1:length(train[,1]), length(train[,1])/2)
no.samp <- c(1:length(train[,1]))[-samp]
nn <- nnet(train[samp,], target[samp,], size = 4, rang = 0.1, decay = 5e-4, maxit = 200)
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
test.cl(target[no.samp,], predict(nn, train[no.samp,]))

# train NN on monocultures and test on mixed cultures (classification without grey value!!!!!)
# 1. Colpidium vs. Paramecium
two_monocult <- sqldf("select *
                         from all_data
                         where species in ('Colpidium', 'Paramecium')")

mixed_cult <- sqldf("select *
                         from all_data
                         where species in ('Colpidum&Paramecium')")

train <- cbind(two_monocult[, c(14,16,17,18,19,20)])
target <- class.ind(two_monocult[,"species"])
target <- target[, c(1,2)]
nn <- nnet(train, target, size = 4, rang = 0.1, decay = 5e-4, maxit = 200)
classified <- predict(nn, mixed_cult[, c(14,16,17,18,19,20)])
predict_spec <- as.data.frame(max.col(classified))
names(predict_spec) <- c("predict_spec")
predict <- cbind(mixed_cult[,1:3],predict_spec)
predict$predict_spec <- as.factor(ifelse(predict$predict_spec == 1,"Colpidum","Paramecium"))

# 2. Colpidium vs. Loxocephalus
two_monocult <- sqldf("select *
                         from all_data
                         where species in ('Colpidium', 'Loxocephalus')")

mixed_cult <- sqldf("select *
                    from all_data
                    where species in ('Loxocephalus&Colpidum')")

train <- cbind(two_monocult[, c(14,16,17,18,19,20)])
target <- class.ind(two_monocult[,"species"])
target <- target[, c(1,4)]
nn <- nnet(train, target, size = 4, rang = 0.1, decay = 5e-4, maxit = 200)
classified <- predict(nn, mixed_cult[, c(14,16,17,18,19,20)])
predict_spec <- as.data.frame(max.col(classified))
names(predict_spec) <- c("predict_spec")
predict <- cbind(mixed_cult[,1:3],predict_spec)
predict$predict_spec <- as.factor(ifelse(predict$predict_spec == 1,"Colpidum","Loxocephalus"))

# 3. Colpidium vs. Paramecium vs. Loxocephalus
three_monocult <- sqldf("select *
                         from all_data
                         where species in ('Colpidium','Paramecium','Loxocephalus')")

mixed_cult <- sqldf("select *
                    from all_data
                    where species in ('Colpidum&Paramecium&Loxocephalus')")

train <- cbind(three_monocult[, c(14,16,17,18,19,20)])
target <- class.ind(three_monocult[,"species"])
target <- target[, c(1,2,4)]
nn <- nnet(train, target, size = 4, rang = 0.1, decay = 5e-4, maxit = 200)
classified <- round(predict(nn, mixed_cult[, c(14,16,17,18,19,20)]),2)
predict_spec <- as.data.frame(max.col(classified))
names(predict_spec) <- c("predict_spec")
predict <- cbind(mixed_cult[,1:3],predict_spec)
predict$predict_spec <- as.factor(ifelse(predict$predict_spec == 1,'Colpidum',ifelse(predict$predict_spec == 2,'Paramecium','Loxocephalus')))

