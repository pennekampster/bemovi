rm(list=ls())


## This code reads in particle information previously obtained via imagej.
## The data files are the six csv files in the same directory as this file.
## You need to set this to point to where your copy of the folder is...


## To do: tidy the whole thing!

## Read in the six data files
setwd("~/Desktop/forJana")
files <- dir()
files <- files[grep(".csv", files)]

## Description of the cultures the data in each file comes from
fnames <- c("Colpidium1", "Colpidium2", "Paramecium1", "Paramecium2", "Both1", "Both2")
species <- c("Colpidium", "Colpidium", "Paramecium", "Paramecium", "Both1", "Both2")
colrz1 <- c("blue", "blue", "red", "red", "green", "green")


## add this information to the data, and merge into one data frame
tt <- read.csv(files[1])
tt$Treatment <- rep(fnames[1], length(tt[,1]))
tt$Species <- rep(species[1], length(tt[,1]))
dd <- tt
for(i in 2:length(files)) {
	tt <- read.csv(files[i])
	tt$Treatment <- rep(fnames[i], length(tt[,1]))
	tt$Species <- rep(species[i], length(tt[,1]))
	dd <- rbind(dd, tt)
}
str(dd)


## Number of particles per frame in the first colpidium video
plot(table(dd$Slice[dd$Treatment=="Colpidium1"]),
	xlab="Frame", ylab="Number of particles")

## Number of particles per frame for each of the six videos
matplot(t(table(dd$Treatment, dd$Slice)),
	xlab="Frame", ylab="Number of particles",
	type="b", pch=19)



library(lattice)

## Hitogram of area, by species
histogram( ~ Area | Species, data=dd, layout=c(1,4))



## Bivariate plots of various dimensions of Colpidium and Paramecium
## There is some overlap...
layout(matrix(1:4, 2, 2))
dd.sub <- subset(dd, !is.na(match(Treatment, c("Colpidium1", "Colpidium2", "Paramecium1", "Paramecium2"))))
plot(Major ~ Minor, col=colrz1[match(dd.sub$Treatment, fnames)],
	data=dd.sub, pch=19, cex=0.2)
legend("topright", legend=c("Colpidium", "Paramecium"), pch=1, col=c("blue", "red"))
plot(Major ~ Round, col=colrz1[match(dd.sub$Treatment, fnames)],
	data=dd.sub, pch=19, cex=0.2)
legend("topright", legend=c("Colpidium", "Paramecium"), pch=1, col=c("blue", "red"))
plot(Circ. ~ Round, col=colrz1[match(dd.sub$Treatment, fnames)],
	data=dd.sub, pch=19, cex=0.2)
legend("topleft", legend=c("Colpidium", "Paramecium"), pch=1, col=c("blue", "red"))
plot(Area ~ Round, col=colrz1[match(dd.sub$Treatment, fnames)],
	data=dd.sub, pch=19, cex=0.2)
legend("topleft", legend=c("Colpidium", "Paramecium"), pch=1, col=c("blue", "red"))



## Some preliminaries
library(nnet)
for.nn <- c("Colpidium", "Paramecium")
fnn <- !is.na(match(dd$Species, for.nn))
fnn <- dd[fnn,]



## Do a pca of five morphological variables of the single species data,
## and plot this. It shows quite nice separation of the species
all <- cbind(fnn$Major, fnn$Minor, fnn$Area, fnn$Round, fnn$Circ.)
pp <- prcomp(scale(all))
summary(pp) ## 95% of variance in first two axes (nice)
plot(pp$x[,1], pp$x[,2],
	col=as.numeric(as.factor(dd[,"Species"])),
	pch=19, cex=0.2,
	xlab="PCA1", ylab="PCA2")



## Train artificial neural net on half of single species data, test on the other half
train <- cbind(fnn$Major, fnn$Minor, fnn$Area, fnn$Round, fnn$Circ.)
target <- class.ind(fnn[,"Species"])
samp <- sample(1:length(train[,1]), length(train[,1])/2)
no.samp <- c(1:length(train[,1]))[-samp]
nn <- nnet(train[samp,], target[samp,], size = 4, rang = 0.1,
            decay = 5e-4, maxit = 200)
test.cl <- function(true, pred) {
    true <- max.col(true)
    cres <- max.col(pred)
    table(true, cres)
}
test.cl(target[no.samp,], predict(nn, train[no.samp,]))
## Its works very well... there is a very small proportion of false positives and negatives.


## Preliminaries for plotting
pred <- c("Colpidium", "Paramecium")[max.col(predict(nn, train[no.samp,]))]
true <- fnn[no.samp,"Species"]
colr <- ifelse(pred=="Colpidium" & true=="Colpidium", 1,
	ifelse(pred=="Paramecium" & true=="Colpidium", 3,
	ifelse(pred=="Paramecium" & true=="Paramecium", 2,
	4)))



layout(matrix(1:2, 1, 2))
## The training data
plot(pp$x[samp,1], pp$x[samp,2],
	col=as.numeric(as.factor(dd[samp,"Species"])),
	pch=19, cex=0.3,
	xlab="PCA1", ylab="PCA2", main="Training data")
plot(pp$x[no.samp,1], pp$x[no.samp,2],
	col=colr,
	pch=19, cex=0.3,
	xlab="PCA1", ylab="PCA2",
	main="Test data")
## You need to add a legend to both of these. You might be able to copy one from below.









#### From here on is not so relevant. (I was testing the neural net on
## the two videos that resulted in datasets 5 and 6).
## now train again on all of the single species data
nn <- nnet(train, target, size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200)
for.nn2 <- c("Both1", "Both2")
fnn2 <- !is.na(match(dd$Species, for.nn2))
fnn2 <- dd[fnn2,]
test <- cbind(fnn2$Major, fnn2$Minor, fnn2$Area, fnn2$Round, fnn2$Circ.)
ppp <- predict(nn, test)
##hist(ppp)
fnn2$pred <- c("Colpidium", "Paramecium")[max.col(ppp)]
rez1 <- apply(t(table(fnn2$pred, fnn2$Slice, fnn2$Species)[,10:126, 1]),2,mean)
rez2 <- apply(t(table(fnn2$pred, fnn2$Slice, fnn2$Species)[,10:126, 2]),2,mean)


train.pp <- t(t(pp$rotation) %*% t(scale(train))) 
test.pp <- t(t(pp$rotation) %*% t(scale(test)))

layout(matrix(1:3, 1, 3))
plot(train.pp[,1], train.pp[,2],
	col=as.numeric(as.factor(fnn[,"Species"])),
	pch=19, cex=0.2,
	xlab="PCA1", ylab="PCA2")
title("Training dataset")
legend("topright", legend=c("Colpidium", "Paramecium"),
	pch=19, col=c("black", "red"), cex=0.5)
plot(train.pp[,1], train.pp[,2],
	col=as.numeric(as.factor(fnn[,"Species"])),
	pch=19, cex=0.2,
	xlab="PCA1", ylab="PCA2")
points(test.pp[,1:2], col=as.numeric(as.factor(fnn2$pred))+2, cex=0.2)
legend("topright", legend=c("Colp. prediction", "Para. prediction"),
	pch=19, col=c(3, 4), cex=0.5)
title(paste(length(ppp[,1]), "of", sum(apply(ppp, 1, max)>0.9), "\nclassifications > 90%"))
text(-3.4, -4, labels=paste("Test vid5"), adj=0)
text(-3.4, -5, labels=paste("Num. Colp =", round(rez1[1], 1)), adj=0)
text(-3.4, -6, labels=paste("Num. Para =", round(rez1[2], 1)), adj=0)
text(1, -4, labels=paste("Test vid6"), adj=0)
text(1, -5, labels=paste("Num. Colp =", round(rez2[1], 1)), adj=0)
text(1, -6, labels=paste("Num. Para =", round(rez2[2], 1)), adj=0)



fnn2$pred1 <- c("Colpidium", "Paramecium")[max.col(ppp)]
fnn2$pred1 <- ifelse(apply(ppp, 1, max)>0.9, fnn2$pred1, "zzz")

plot(train.pp[,1], train.pp[,2],
	col=as.numeric(as.factor(fnn[,"Species"])),
	pch=19, cex=0.2,
	xlab="PCA1", ylab="PCA2")
points(test.pp[,1:2], col=as.numeric(as.factor(fnn2$pred1))+2, cex=0.2)
legend("topright", legend=c("Colp. prediction", "Para. prediction", "Not sure"),
	pch=19, col=c(3, 4, 5), cex=0.5)
##title(paste(length(ppp[,1]), "of", sum(apply(ppp, 1, max)>0.9), "\nclassifications > 90%"))

rez3 <- apply(t(table(fnn2$pred1, fnn2$Slice, fnn2$Species)[,10:126, 1]),2,mean)
rez4 <- apply(t(table(fnn2$pred1, fnn2$Slice, fnn2$Species)[,10:126, 2]),2,mean)

text(-3.4, -4, labels=paste("Test vid5"), adj=0)
text(-3.4, -5, labels=paste("Num. Colp =", round(rez3[1], 1)), adj=0)
text(-3.4, -6, labels=paste("Num. Para =", round(rez3[2], 1)), adj=0)
text(-3.4, -7, labels=paste("Not sure =", round(rez3[3], 1)), adj=0)
text(1, -4, labels=paste("Test vid6"), adj=0)
text(1, -5, labels=paste("Num. Colp =", round(rez4[1], 1)), adj=0)
text(1, -6, labels=paste("Num. Para =", round(rez4[2], 1)), adj=0)
text(1, -7, labels=paste("Not sure =", round(rez4[3], 1)), adj=0)




