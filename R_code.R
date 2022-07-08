# setwd("~/Uni/TUM/4. Semester/Advanced Statistical Methods/Final Project/Speed Dating")

require(tidyverse)

data <- read.csv("data_preprocessed.csv")
data %>% head()


################################## PCA-hobbies ##################################
# select columns
data_hobbies <- data[,c('gender', 'sports', 'tvsports', 'exercise',	'dining',	'museums',	'art',	'hiking',	'gaming',	'clubbing',	'reading',	'tv',	'theater',	'movies',	'concerts',	'music',	'shopping',	'yoga')]
head(data_hobbies)

# check dimensions
ncol(data_hobbies)
nrow(data_hobbies)

# PCA
require(easyCODA)

## first two dimensions
# general
head(data_hobbies)
data_hobbies_m <- as.matrix(data_hobbies)
# assign 'o' to males and 'x' to females
rownames(data_hobbies_m) <- ifelse(data_hobbies$gender == 1, 'm', 'f')
# remove gender column
data_hobbies_m <- data_hobbies_m[,colnames(data_hobbies_m)!="gender"]

# par(mfrow=c(1,2))
par(mfrow=c(1,1))
PCA_hobbies <- PCA(as.matrix(data_hobbies_m), weight=FALSE)
PLOT.PCA(PCA_hobbies,
         map="asymmetric",
         rescale=2,
         dim = c(1,2),
         axes.inv=c(-1,1),
         cols=c('lightblue', 'red'),
         cexs=c(0.5,0.8),
         fonts=c(2,1))

hobbies.rpc <- PCA_hobbies$rowpcoord

require(ellipse)
CIplot_biv(hobbies.rpc[,1], hobbies.rpc[,2], group=rownames(data_hobbies_m),
           groupcols=c('blue', 'darkgreen'),
           shade=FALSE,
           add=T,
           cex=0.5)
summary(PCA_hobbies)

CIplot_biv(hobbies.rpc[,1],
           hobbies.rpc[,2],
           group=rownames(data_hobbies_m),
           groupcols=c('blue', 'darkgreen'),
           shade=TRUE)

# by gender
hobbies_male <- as.matrix(data[data$gender == 1,] %>% .[,c('sports', 'tvsports', 'exercise',	'dining',	'museums',	'art',	'hiking',	'gaming',	'clubbing',	'reading',	'tv',	'theater',	'movies',	'concerts',	'music',	'shopping',	'yoga')])
hobbies_male %>% head()
hobbies_female <- as.matrix(data[data$gender == 0,] %>% .[,c('sports', 'tvsports', 'exercise',	'dining',	'museums',	'art',	'hiking',	'gaming',	'clubbing',	'reading',	'tv',	'theater',	'movies',	'concerts',	'music',	'shopping',	'yoga')])
hobbies_female %>% head()


rownames(hobbies_male) <- rep(' ', length(hobbies_male[,1]))
rownames(hobbies_female) <- rep(' ', length(hobbies_female[,1]))

PCA_hobbies_male <- PCA(hobbies_male, weight=FALSE)
PCA_hobbies_female <- PCA(hobbies_female, weight=FALSE)

par(mfrow=c(1,1))
PLOT.PCA(PCA_hobbies_male,
         map="asymmetric",
         rescale=3,
         axes.inv=c(1,1),
         dim = c(1,2),
         fonts=c(2,1),
         cexs=c(0.8,1.2),
         main='Males')
PLOT.PCA(PCA_hobbies_female,
         map="asymmetric",
         rescale=8,
         axes.inv=c(1,1),
         dim = c(1,2),
         fonts=c(2,1),
         cexs=c(0.8,1.2),
         main='Females')


# further dimensions
PLOT.PCA(PCA_hobbies, map="asymmetric",rescale=0.5, axes.inv=c(1,1), dim = c(1,3))
PLOT.PCA(PCA_hobbies, map="asymmetric",rescale=0.5, axes.inv=c(1,1), dim = c(2,3))
summary(PCA_hobbies)



################################## PCA-features ##################################
## desirable features of the opposite gender
data_des_features <- data[,c('gender', 'attr1_1',	'sinc1_1',	'intel1_1',	'fun1_1',	'amb1_1',	'shar1_1')]
colnames(data_des_features) <- c('gender', 'attr',	'sinc',	'intel',	'fun',	'amb',	'shar')
data_des_features_m <- as.matrix(data_des_features)
head(data_des_features)

# check dimensions
ncol(data_des_features)
nrow(data_des_features)

par(mfrow=c(1,1))
rownames(data_des_features_m) <- ifelse(data_des_features$gender == 1, 'm', 'f')
data_des_features_m <- data_des_features_m[,colnames(data_des_features_m)!="gender"]

PCA_des_features <- PCA(data_des_features_m, weight=FALSE)
par(mfrow=c(1,1))
PLOT.PCA(PCA_des_features,
         map="asymmetric",
         rescale=5,
         dim = c(1,2),
         axes.inv=c(1,1),
         cols=c('lightblue', 'red'),
         cexs=c(0.5,0.8),
         fonts=c(2,1))


# by gender
des_features_male <- as.matrix(data[data$gender == 1,] %>% .[,c('attr1_1',	'sinc1_1',	'intel1_1',	'fun1_1',	'amb1_1',	'shar1_1')])
des_features_male %>% head()
des_features_female <- as.matrix(data[data$gender == 0,] %>% .[,c('attr1_1',	'sinc1_1',	'intel1_1',	'fun1_1',	'amb1_1',	'shar1_1')])
des_features_female %>% head()

colnames(des_features_male) <- c('attr',	'sinc',	'intel',	'fun',	'amb',	'shar')
colnames(des_features_female) <- c('attr',	'sinc',	'intel',	'fun',	'amb',	'shar')

rownames(des_features_male) <- rep(' ', length(des_features_male[,1]))
rownames(des_features_female) <- rep(' ', length(des_features_female[,1]))


PCA_des_features_male <- PCA(as.matrix(des_features_male), weight=FALSE)
PCA_des_features_female <- PCA(as.matrix(des_features_female), weight=FALSE)
par(mfrow=c(1,2))
PLOT.PCA(PCA_des_features_male,
         map="asymmetric",
         axes.inv = c(-1,-1),
         rescale=10,
         main='Responses of Males',
         cexs=c(0.8, 1.2))
PLOT.PCA(PCA_des_features_female,
         map="asymmetric",
         rescale=10,
         cexs=c(0.8, 1.2),
         main='Responses of Females')


################################## Correspondence Analysis - features ##################################
imp1 <- as.data.frame(data)
par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0), cex.axis=0.8, mfrow=c(2,3), cex.axis=1)

hist(data$sinc1_1, main="Histogram Sincerity", cex.main=1, breaks = 6)
hist(data$fun1_1, main="Histogram Fun", cex.main=1, breaks = 6)
hist(data$attr1_1, main="Histogram Attractiveness", cex.main=1, breaks = 6)
hist(data$intel1_1, main="Histogram Intelligence", cex.main=1, breaks = 6)
hist(data$amb1_1, main="Histogram Ambition", cex.main=1, breaks = 6)
hist(data$shar1_1, main="Histogram Shared Interests", cex.main=1, breaks = 6)

# define function to make data categorical
categorize <- function(vec) {
  res <- rep(0, length(vec))
  res <- ifelse(vec > 10, 1, res)
  res <- ifelse(vec > 20, 2, res)
  res <- ifelse(vec > 30, 3, res)
  res <- ifelse(vec > 40, 4, res)
  res <- ifelse(vec > 50, 5, res)
  res <- ifelse(vec > 60, 6, res)
  res <- ifelse(vec > 70, 7, res)
  res <- ifelse(vec > 80, 8, res)
  res <- ifelse(vec > 90, 9, res)
  res.names <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  return(res)
}
categorize2 <- function(vec) {
  res <- rep(0, length(vec)) # not important ni
  res <- ifelse(vec > 5, 1, res) # little important li
  res <- ifelse(vec > 20, 2, res) # somewhat important si
  res <- ifelse(vec > 30, 3, res) # important i
  res <- ifelse(vec > 50, 4, res) # very important vi
  # res.names <- c('ni', 'li', 'si', 'i', 'vi')
  return(res)
}

fun1_1 <- categorize(data$fun1_1)
sinc1_1 <- categorize(data$sinc1_1)
attr1_1 <- categorize(data$attr1_1)
shar1_1 <- categorize(data$shar1_1)
amb1_1 <- categorize(data$amb1_1)

fun1_1 <- categorize2(data$fun1_1)
sinc1_1 <- categorize2(data$sinc1_1)
attr1_1 <- categorize2(data$attr1_1)
shar1_1 <- categorize2(data$shar1_1)
amb1_1 <- categorize2(data$amb1_1)

fun1_1 <- categorize2(data$fun1_1)
sinc1_1 <- categorize2(data$sinc1_1)
attr1_1 <- categorize2(data$attr1_1)
shar1_1 <- categorize2(data$shar1_1)
amb1_1 <- categorize2(data$amb1_1)

# cross-tabulate
fun1_1_attr1_1 <- table(fun1_1, attr1_1)
# rownames(fun1_1_attr1_1) <- c('ni', 'li', 'si', 'i', 'vi')
# colnames(fun1_1_attr1_1) <- c('ni', 'li', 'si', 'i', 'vi')

fun1_1_sinc1_1 <- table(fun1_1, sinc1_1)
# rownames(fun1_1_sinc1_1) <- c('ni', 'li', 'si', 'i', 'vi')
# colnames(fun1_1_sinc1_1) <- c('ni', 'li', 'si', 'i', 'vi')

fun1_1_shar1_1 <- table(fun1_1, shar1_1)
# rownames(fun1_1_shar1_1) <- c('ni', 'li', 'si', 'i', 'vi')
# colnames(fun1_1_shar1_1) <- c('ni', 'li', 'si', 'i', 'vi')

fun1_1_amb1_1 <- table(fun1_1, amb1_1)
# rownames(fun1_1_amb1_1) <- c('ni', 'li', 'si', 'i', 'vi')
# colnames(fun1_1_amb1_1) <- c('ni', 'li', 'si', 'i', 'vi')

# compute and plot Correspondence Analysis
ca1 <- ca(fun1_1_attr1_1)
plot(ca1, main="", mass=c(TRUE,TRUE), cex.main=0.95)
# summary(my.ca1)

ca1 <- ca(fun1_1_sinc1_1)
plot(ca1, main="", mass=c(TRUE,TRUE), cex.main=0.95)
summary(my.ca1)

ca1 <- ca(fun1_1_shar1_1)
plot(ca1, main="", mass=c(TRUE,TRUE), cex.main=0.95)
summary(my.ca1)

ca1 <- ca(fun1_1_amb1_1)
plot(ca1, main="", mass=c(TRUE,TRUE), cex.main=0.95)
summary(my.ca1)


################################## K-means clustering ##################################


data_features <- data[c('sinc1_1', 'fun1_1', 'attr1_1', 'intel1_1', 'amb1_1', 'shar1_1')]



## kmeans for 3 clusters, with 20 random starts
set.seed(1234)
features.km <- kmeans(data_features, centers=3, nstart=20)
features.km$betweenss/features.km$totss
features.km$size

## looping on k-means algorithm to decide how many clusters
set.seed(123)
features.BW <- rep(0, 10)
features.BSS <- rep(0, 10)
features.TSS <- rep(0, 10)
for(nc in 2:10) {
  features.km <- kmeans(data_features, centers=nc, nstart=20, iter.max=200)
  features.BW[nc] <- features.km$betweenss/features.km$totss
  features.BSS[nc] <- features.km$betweenss
  features.TSS[nc] <- features.km$totss
}
features.BW
features.BSS
features.TSS

## plot the proportion of between-cluster variance
par(mar=c(4.2,4,1,2), cex.axis=0.8, mfrow=c(1,2))
plot(features.BW, xlab="Nr of clusters", ylab="BSS/TSS")

## plot the increments in between-cluster variance
features.BWinc <- features.BW[2:10]-features.BW[1:9]  
plot(1:10, c(NA,NA, features.BWinc[2:9]), xlab="Nr of clusters", ylab="improvement")
lines(3:10, features.BWinc[2:9], col="red", lwd=2)

## looks like 3-cluster solution is a good choice
clnumber <- 3
set.seed(1234)
features.km2 <- kmeans(data_features, centers=clnumber, nstart=20, iter.max=200)
features.km2$betweenss/features.km2$totss
## cluster sizes
features.km2$size
par(mar = c(2, 2, 2, 2), mfrow=c(1,1))
pie(features.km2$size, labels=features.km2$size, main = "Cluster Size",
    col =  heat.colors(length(features.km2$size)),
    lty=0)
legend("topleft", legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
       fill = heat.colors(length(features.km2$size)), cex=0.75)

## average those in a cluster by the original lifestyle counts 
features.means <- matrix(0,nrow=clnumber,ncol=6)
# rownames(features.means) <- c("clus1","clus2")
# rownames(features.means) <- c("clus1","clus2", "clus3", "clus4")
rownames(features.means) <- c("clus1","clus2", "clus3")
colnames(features.means) <- colnames(data_features)[1:6]
for(j in 1:6) features.means[,j] <- tapply(data_features[,j],features.km2$cluster, mean)
round(features.means, 1)

## because data are skew we prefer medians (shown in class slides)
features.medians <- matrix(0,nrow=clnumber,ncol=6)
rownames(features.medians) <- c("clus1","clus2", "clus3")
colnames(features.medians) <- colnames(data_features)[1:6]
for(j in 1:6) features.medians[,j] <- tapply(data_features[,j],features.km2$cluster,median)
round(features.medians,1)

## boxplots of each variable, across the four clusters
par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0), cex.axis=0.8, mfrow=c(2,3), cex.axis=1)
for(j in 1:6) {
  boxplot(data_features[,j] ~ features.km2$cluster,main=colnames(data_features)[j], 
          xlab="Clusters", col=hcl(c(45,135,225,315), 60, 70))
}


cluster_result <- data_features
cluster_result$cluster <- features.km2$cluster
library(dplyr)
cluster_result %>% group_by(cluster) %>%
  summarise_each(funs(max, min, mean, median, sd), fun1_1)
cluster_result %>% group_by(cluster) %>%
  summarise_each(funs(max, min, mean, median, sd), amb1_1)
cluster_result %>% group_by(cluster) %>%
  summarise_each(funs(max, min, mean, median, sd), shar1_1)
cluster_result %>% group_by(cluster) %>%
  summarise_each(funs(max, min, mean, median, sd), attr1_1)


