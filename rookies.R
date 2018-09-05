
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
rook07 <- read_csv('data/07_08Rookies.csv')
rook08 <- read_csv('data/08_09Rookies.csv')
rook09 <- read_csv('data/09_10Rookies.csv')
rook10 <- read_csv('data/10_11Rookies.csv')
rook11 <- read_csv('data/11_12Rookies.csv')
rook12 <- read_csv('data/12_13Rookies.csv')
rook13 <- read_csv('data/13_14Rookies.csv')
rook14 <- read_csv('data/14_15Rookies.csv')
rook15 <- read_csv('data/15_16_Rookies.csv')
rook16 <- read_csv('data/16_17Rookies.csv')
rook17 <- read_csv('data/17_18Rookies.csv')

#paste("rook",seq(07,17, by = 1),sep = "")

nba_list1 <- list(rook03, rook04, rook05, rook06,rook07, rook08, rook09,rook10, rook11,rook12,rook13,rook14,rook15,rook16,rook17)

remove_losers <- function(data){
  data <- dplyr::filter(data, g > 29) 
}

count_losers <- function(data){
  data_new <- dplyr:: filter(data,g>29)
  count <- nrow(data) - nrow(data_new)
  prop <- count/nrow(data)
  return(prop)}

##plot of injured player proportions
inj.prop <- lapply(nba_list1, count_losers)
library(ggplot2); library(ggthemes)
ggplot() + geom_line(aes(x = 1:15, y = unlist(inj.prop)),size = 1.5) + stat_smooth(aes(x = 1:15, y = unlist(inj.prop)), color = "red", method = 'loess', fill = NA) + 
  theme_classic() + ggtitle("Proportion of rookies that played fewer than 30 games") + xlab("Year") + ylab("Percent Variation Explained") + 
  scale_x_continuous(breaks = c(1:16), labels = seq(2003,2018,by = 1))

return_stuff <- function(data){
  missed <- ifelse(data$g < 31, 1, 0) #returns 1 if they are injured/miss time
return(missed)}
missed <- unlist(lapply(nba_list1, return_stuff))
nrows.vec <- unlist(lapply(nba_list1, nrow))
year <- rep(c(2003:2017), times = c(nrows.vec))

#builds a dataset
df.year <- tibble(factor(year), missed)

library(viridis)
ggplot(df.year) + geom_bar(aes(year, fill = factor(missed))) + ggtitle("Proportion of Rookies by Playing Time") +
  theme_bw() + ylab("Count of Rookies") + xlab("Year") + scale_x_discrete(breaks =c(1:16),labels = seq(2003,2018,by = 1)) +
  scale_fill_viridis(name = "Playing Time", label = c(">= 30 games","< 30 games"),option = "viridis",discrete = TRUE)

#############


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
library(mclust)
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



#some code for the exploratory analysis
#plot of pct over time
##function to get the variation explained

get_Var <- function(pca_object){
  summary(pca_object)$importance['Cumulative Proportion',3]
  }

#variance explained
var_exp <- lapply(pca_list, get_Var)
library(ggplot2); library(ggthemes)
ggplot() + geom_line(aes(x = 1:15, y = unlist(var_exp))) + stat_smooth(aes(x = 1:15, y = unlist(var_exp)), color = "red", method = 'loess', fill = NA) + 
  theme_bw() + ggtitle("Variation Explained by the First Two PCs") + xlab("Year") + ylab("Percent Variation Explained") + 
  scale_x_continuous(breaks = c(1:16), labels = seq(2003,2018,by = 1))


