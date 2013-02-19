library(ggplot2)

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
fit_morph <- princomp(species_subsets[, c(14,15,16,17,18)], cor=TRUE)
PC1_morph <- fit_morph$scores[,1]
PC2_morph <- fit_morph$scores[,2]
PC1_move <- fit_move$scores[,1]
PC2_move <- fit_move$scores[,2]
plot_PCA_morph <- cbind(PC1_morph,PC2_morph,species_subsets[c(2,3,21)])
plot_PCA_move <- cbind(PC1_move,PC2_move,species_subsets[c(2,3,21)])
ggplot(plot_PCA_morph, aes(x=PC1_morph, y=PC2_morph, color=plot_PCA_morph$species)) + geom_point()
ggplot(plot_PCA_move, aes(x=PC1_move, y=PC2_move, color=plot_PCA_move$species)) + geom_point()

