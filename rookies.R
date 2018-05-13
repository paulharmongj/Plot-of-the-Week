
#
library(xlsx)
x <- read.xlsx('na.xlsx', sheetIndex = 1)

dim(x)               
Player_Names <- x[seq(3-1,156-1,by =3),1:2]
Stats <- x[seq(3,156,by =3),]

new_df <- cbind(Player_Names, Stats)[,-c(23,24)]
colnames(new_df) <- colnames(x)
library(tibble)
new_df <- as_tibble(new_df)
new_df$MIN <- as.numeric(new_df$GP)
new_df$PTS <- as.numeric(new_df$PTS)

x <- data.frame(new_df)

#new file
x <- read.csv('data/na.csv', header = TRUE)
x$GP <- as.numeric(x$GP)
data <- apply(x[,-c(1,2,3)],2, as.numeric)
data.scale <- apply(data, 2, scale)


#run the PCA
pc1 <- prcomp(data.scale, scale = TRUE, center = TRUE)
summary(pc1)
plot(pc1)

biplot(pc1)

#create the plot
plot(pc1$x[,1], pc1$x[,2], type = 'n', xlab = "First Component", ylab = "Second Component", main = "NBA Rookies", xlim = c(-11,6))
text(pc1$x[,1], pc1$x[,2], labels = paste(substring(x$FIRST,1,1),x$LAST))

#distances
distances <- dist(data.frame(pc1$x[,1],pc1$x[,2]))
distmat <- as.matrix(distances)
rownames(distmat) <- colnames(distmat) <- x$LAST

sort(distmat[,1])
sort(distmat[,3])

#add to the plot: nearest players
segments(pc1$x[1,1], pc1$x[1,2], pc1$x[5,1], pc1$x[5,2], lwd = 3, col = 'blue')
segments(pc1$x[3,1], pc1$x[3,2], pc1$x[11,1], pc1$x[11,2], lwd = 3, col = 'red')

#to the mean
Scores <- pc1$x[,1:2]
means <- apply(Scores,2,mean)
segments(pc1$x[1,1], pc1$x[1,2], means[1],means[2], lwd = 3, col = 'blue', lty = 2)
segments(pc1$x[3,1], pc1$x[3,2], means[1],means[2], lwd = 3, col = 'red', lty = 2)




#attempt to do some clustering
library(mclust)

clusters <- mclustBIC(Scores)
mod1 <- Mclust(Scores, x = clusters)
plot(pc1$x[,1], pc1$x[,2], type = 'n', xlab = "First Component", ylab = "Second Component", main = "NBA Rookies", xlim = c(-11,6))
text(pc1$x[,1], pc1$x[,2], labels = paste(substring(x$FIRST,1,1),x$LAST), col = mod1$classification)

#different clustering algorithm




#######################################
#Looking at data from previous seasons#
#######################################
library(readr)
rook03 <- read_csv("data/03_04Rookies.csv")
rook04 <- read_csv('data/04_05Rookies.csv')
rook05 <- read_csv('data/05_06Rookies.csv')
rook06 <- read_csv('data/06_07Rookies.csv')

nba_list1 <- list(rook03, rook04, rook05, rook06)

remove_losers <- function(data){
  data <- dplyr::filter(data, g > 29) 
}

nba_list <- lapply(nba_list1, remove_losers)


#scale and center
nba.pca <- function(data, under_30 = TRUE){
#removes the   
  
#scales the data  
  data.scale <- apply(data[,9:21], 2, scale)
  #runs the pca
   pca <- prcomp(data.scale, scale = TRUE, center = TRUE)

return(pca)}

pca_list <- lapply(nba_list, nba.pca)

par(mfrow =c(2,2))
lapply(pca_list, biplot)

#get the scores
get_scores <- function(pca_obj){
  pca_scores <- pca_obj$x[,c(1:3)]
  return(pca_scores)}

pca_scores_list <- lapply(pca_list, get_scores)
#automate clustering
nba_clust <- function(Scores){
  clusters <- mclustBIC(Scores)
  mod1 <- Mclust(Scores, x = clusters)
  return(mod1)
}

class_list <- lapply(pca_scores_list, nba_clust)

par(mfrow = c(1,1))

#generates plots of all each of the years (with BIC-optimized clusters)
par(mfrow = c(1,1))
for (j in 1:length(class_list)){
  plot(pca_scores_list[[j]][,1], pca_scores_list[[j]][,2], col = class_list[[j]]$classification, pch = 20, main = paste("PCA of NBA Rookies",j + 2002), xlab = "First Component", ylab = "Second Component", type = "n", xlim =c(-10,10))
  text(pca_scores_list[[j]][,1], pca_scores_list[[j]][,2], label = nba_list[[j]]$Name, col = class_list[[j]]$classification, pch = 20, cex = .9)
}


#predicted ROY
#pick 2006

#maximize on the first pc
for(j in 1:length(pca_scores_list)){
  rownames(pca_scores_list[[j]]) <- nba_list[[j]]$Name
}

pca_sort <- function(pca_list){
  which.max(abs(pca_list[,1]))
  #which.min(pca_list[,1])
}

lapply(pca_scores_list,pca_sort)




