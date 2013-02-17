library(ggplot2)

# code to do classification based on both morphology and movement data
# will integrate all of OWen's previous ideas and re-use available codes


# import movement data
morphology.data <- read.table(paste("C:/Users/Frank/Documents/PhD/Programming/franco/merge morphology and trajectory","morphology.data.summary.txt", sep = "/"), sep = "\t")

# import movement data
trajectory.data <- read.table(paste("C:/Users/Frank/Documents/PhD/Programming/franco/merge morphology and trajectory","trajectory.data.summary.txt", sep = "/"), sep = "\t")
trajectory.data$file <- gsub("Traj_" ,"",trajectory.data$file)
trajectory.data$file <- gsub(".avi.txt" ,"",trajectory.data$file)

#merge morphology data with meaningful trajectory data (meaningful = show properties used for filter trajectories)
all_data <- sqldf("select *
                  from 'trajectory.data' t, 'morphology.data' m
                  where t.file=m.file AND t.trajectory=m.trajectory
                  ")

# only take monocultures
species_subsets <- sqldf("select *
                         from all_data
                         where file in ('Data34', 'Data38', 'Data49')")


# temporary plotting of aggregate variables to check which have highest discrimination power
# 1. morphology
ggplot(species_subsets, aes(x=species_subsets$area, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$grey, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$major, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$perimeter, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$circularity, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$AR, color=as.factor(species_subsets$file))) + geom_density()

# 2. movement characteristics
ggplot(species_subsets, aes(x=species_subsets$net_speed, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$NGDR, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$sd_turning, color=as.factor(species_subsets$file))) + geom_density()
ggplot(species_subsets, aes(x=species_subsets$period, color=as.factor(species_subsets$file))) + geom_density()

# use a PCA to visualize whether species can be separated by morphology & movement
fit_move <- princomp(species_subsets[, c(4,5,6,7,8,9,10,11)], cor=TRUE)
fit_morph <- princomp(species_subsets[, c(14,15,16,17,18)], cor=TRUE)
PC1_morph <- fit_morph$scores[,1]
PC2_morph <- fit_morph$scores[,2]
PC1_move <- fit_move$scores[,1]
PC2_move <- fit_move$scores[,2]
plot_PCA_morph <- cbind(PC1_morph,PC2_morph,species_subsets[2:3])
plot_PCA_move <- cbind(PC1_move,PC2_move,species_subsets[2:3])

ggplot(plot_PCA_morph, aes(x=PC1_morph, y=PC2_morph, color=as.factor(plot_PCA_morph$file))) + geom_point()
ggplot(plot_PCA_move, aes(x=PC1_move, y=PC2_move, color=as.factor(plot_PCA_morph$file))) + geom_point()
