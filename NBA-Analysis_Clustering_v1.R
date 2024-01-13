# Install all the important packages

install.packages("readr") # for Read CSV
library(readr)

install.packages("ggplot2") # for Visuals
library(ggplot2)

library(dplyr) # for Pipes & Data Manipulation
library(dendextend) # for Other Cleaning & Clustering
library(xlsx)
library(NbClust)
library(factoextra)


# Read CSV Data
setwd("C:/Users/PC/Documents/R_WorkFiles/R_Work_Files/R_Work_Files")
nba_raw_data <- read_csv("nba_2023-2024_current_stats.csv") # Import Total Player Data
View(nba_raw_data)


# Data Description
str(nba_raw_data) # Data types of variables
head(nba_raw_data) # First 6 rows
tail(nba_raw_data) # Last 6 rows
dim(nba_raw_data) # Dimensions of rows x columns


# Change the name of some variables for clarity 
colnames(nba_raw_data)[5] <- 'Team'
colnames(nba_raw_data)[1] <- 'Rank'
colnames(nba_raw_data)


# Convert team and position variables into factors
class(nba_raw_data$Team)
class(nba_raw_data$Pos)
nba_raw_data$Team <- as.factor(nba_raw_data$Team)
nba_raw_data$Pos <- as.factor(nba_raw_data$Pos)
levels(nba_raw_data$Team) # Check for the convertion accuracy
levels(nba_raw_data$Pos)


# Cleaning the Data (Eliminating the Columns has NA Data) // (we will re-create this columns)
nba_raw_data_v2 <- nba_raw_data[, c("Rank", "Player", "Age", "Team", "Pos", "G", "GS", "MP", "FG", "FGA", "3P", "3PA", "2P", "2PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")]
is.na(nba_raw_data_v2)
any(is.na(nba_raw_data_v2)) # Not found any NA Value


# When we examine the dataset, we find that some players' stats recorded as cumulative with the 'TOT' record tag. We will clean.
nba_raw_data_v2[nba_raw_data_v2$Team == "TOT", ]
nba_raw_data_v2[nba_raw_data_v2$Player == "Daniel Theis", ] # As we can see in the example, 'TOT' row includes the players' all stats in the different teams. We will delete the 'TOT' column and apply summarization during the analysis process.
nba_raw_data_v2 <- nba_raw_data_v2[nba_raw_data_v2$Team != "TOT", ]
summary(nba_raw_data_v2$Team)

nba_raw_data_v2 <- droplevels(nba_raw_data_v2) # Clean the teams not used
summary(nba_raw_data_v2$Team)


##### Data Pre-processing

# Exclude team, age, position
View(nba_raw_data_v2)

nba_raw_data_v2 <- nba_raw_data_v2[, -c(1, 4)] # Exclude rank and team information, they do not necessary
nba_raw_data_v2[, c(1,2,3)] -> nba_player_feature_data
nba_raw_data_v3 <- nba_raw_data_v2 %>%
  group_by(Player) %>%
  summarise(across(c(G, GS, MP, FG, FGA, `3P`, `3PA`, `2P`, `2PA`, FT, FTA, ORB, DRB, TRB, AST, STL, BLK, TOV, PF, PTS), sum, na.rm = TRUE))

View(nba_raw_data_v3)
nba_raw_data_v3$FG_Success <- ifelse(nba_raw_data_v3$FGA == 0, 0, nba_raw_data_v3$FG / nba_raw_data_v3$FGA)
nba_raw_data_v3$"3P_Success" <- ifelse(nba_raw_data_v3$"3PA" == 0, 0, nba_raw_data_v3$"3P" / nba_raw_data_v3$"3PA")
nba_raw_data_v3$"2P_Success" <- ifelse(nba_raw_data_v3$"2PA" == 0, 0, nba_raw_data_v3$"2P" / nba_raw_data_v3$"2PA")
nba_raw_data_v3$FT_Success <- ifelse(nba_raw_data_v3$FTA == 0, 0, nba_raw_data_v3$FT / nba_raw_data_v3$FTA)

any(is.na(nba_raw_data_v3)) # Not found any NA Value
View(nba_raw_data_v3)

# Detect outlier (Minutes Played, because we do not think that NBA players with less minutes played will be selected as Allstar)

summary(nba_raw_data_v3$MP)
table(cut(nba_raw_data_v3$MP,breaks=seq(1,1257,by=60)))

hist(nba_raw_data_v3$MP,
     xlab = "hwy",
     main = "Histogram of MP",
     breaks = seq(1,1257,by=2)
)

lower_bound <- quantile(nba_raw_data_v3$MP, 0.3)

outlier_row <- which(nba_raw_data_v3$MP > lower_bound)

nba_raw_data_v4 <- nba_raw_data_v3[outlier_row, ]
View(nba_raw_data_v4)

# Merge two table

nba_player_feature_data <- nba_player_feature_data %>%
  select(Player, Pos, Age) %>%
  distinct(Player, .keep_all = TRUE)

nba_structured_table <- merge(nba_player_feature_data, nba_raw_data_v4, by = "Player")
View(nba_structured_table)

###################### Data Clustering ######################

levels(nba_structured_table$Pos)

## C ##
nba_players_center_v1 <- nba_structured_table[nba_structured_table$Pos == "C",]
View(nba_players_center_v1)

# Decline some columns for efficient data analysis
nba_players_center_v1[, -c(1,2,3,4,5,6)] -> nba_players_center_v2
View(nba_players_center_v2)

## Scale the data and interpret the number of correct clusters for determining the predicted allstars
nba_players_center_v2.scaled <-  scale(nba_players_center_v2)
View(nba_players_center_v2.scaled)


# Elbow Method
fviz_nbclust(nba_players_center_v2.scaled, kmeans, method = "wss", k.max=12)
fviz_nbclust(nba_players_center_v2.scaled, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 5, linetype = 2)


# for Hierarchical Clustering (Ward's Clustering) 
set.seed(1234)
dist.res <- dist(nba_players_center_v2.scaled, method = "euclidean")
center_cust_Clustering <- hclust(dist.res, method = "ward.D")
plot(center_cust_Clustering, labels = FALSE, hang = -1) # Visualization of hclust
rect.hclust(center_cust_Clustering, h=20) # Visualization of hclust - draw rectangle

# Assign clusters to origin of data points based on the cluster number
center_cust_cluster <- cutree(center_cust_Clustering, k=5) 
table(center_cust_cluster)

# Integrating segments into dataset
center_cust_clusters <- data.frame()
center_cust_clusters = data.frame(nba_players_center_v1, center_cust_cluster)
View(center_cust_clusters)

# View the segmentation graph
ggplot(center_cust_clusters, aes(x=PTS, y=TRB , color = factor(center_cust_clusters$center_cust_cluster))) + geom_point()

# list the predicted all-star players
View(center_cust_clusters[center_cust_clusters$center_cust_cluster==2, ])





## PF ##

nba_players_power_forward_v1 <- nba_structured_table[nba_structured_table$Pos == "PF",]
View(nba_players_power_forward_v1)

# Decline some columns for efficient data analysis
nba_players_power_forward_v1[, -c(1,2,3,4,5,6)] -> nba_players_power_forward_v2
View(nba_players_power_forward_v2)

## Scale the data and interpret the number of correct clusters for determining the predicted allstars
nba_players_power_forward_v2.scaled <-  scale(nba_players_power_forward_v2)
View(nba_players_power_forward_v2.scaled)


# Elbow Method
fviz_nbclust(nba_players_power_forward_v2.scaled, kmeans, method = "wss", k.max=12)
fviz_nbclust(nba_players_power_forward_v2.scaled, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 3, linetype = 2)


# for Hierarchical Clustering (Ward's Clustering) 
set.seed(1234)
dist.res <- dist(nba_players_power_forward_v2.scaled, method = "euclidean")
power_forward_cust_Clustering <- hclust(dist.res, method = "ward.D")
plot(power_forward_cust_Clustering, labels = FALSE, hang = -1) # Visualization of hclust
rect.hclust(power_forward_cust_Clustering, h=40) # Visualization of hclust - draw rectangle

# Assign clusters to origin of data points based on the cluster number
power_forward_cust_cluster <- cutree(power_forward_cust_Clustering, k=3) 
table(power_forward_cust_cluster)

# Integrating segments into dataset
power_forward_cust_clusters <- data.frame()
power_forward_cust_clusters = data.frame(nba_players_power_forward_v1, power_forward_cust_cluster)
View(power_forward_cust_clusters)

# View the segmentation graph
ggplot(power_forward_cust_clusters, aes(x=MP, y=PTS , color = factor(power_forward_cust_clusters$power_forward_cust_cluster))) + geom_point()

# list the predicted all-star players
View(power_forward_cust_clusters[power_forward_cust_clusters$power_forward_cust_cluster==3, ])






## PG ##

nba_players_point_guard_v1 <- nba_structured_table[nba_structured_table$Pos == "PG",]
View(nba_players_point_guard_v1)

# Decline some columns for efficient data analysis
nba_players_point_guard_v1[, -c(1,2,3,4,5,6)] -> nba_players_point_guard_v2
View(nba_players_point_guard_v2)

## Scale the data and interpret the number of correct clusters for determining the predicted allstars
nba_players_point_guard_v2.scaled <-  scale(nba_players_point_guard_v2)
View(nba_players_point_guard_v2.scaled)


# Elbow Method
fviz_nbclust(nba_players_point_guard_v2.scaled, kmeans, method = "wss", k.max=12)
fviz_nbclust(nba_players_point_guard_v2.scaled, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 5, linetype = 2)


# for Hierarchical Clustering (Ward's Clustering) 
set.seed(1234)
dist.res <- dist(nba_players_point_guard_v2.scaled, method = "euclidean")
point_guard_cust_Clustering <- hclust(dist.res, method = "ward.D")
plot(point_guard_cust_Clustering, labels = FALSE, hang = -1) # Visualization of hclust
rect.hclust(point_guard_cust_Clustering, h=15) # Visualization of hclust - draw rectangle

# Assign clusters to origin of data points based on the cluster number
point_guard_cust_cluster <- cutree(point_guard_cust_Clustering, k=5) 
table(point_guard_cust_cluster)

# Integrating segments into dataset
point_guard_cust_clusters <- data.frame()
point_guard_cust_clusters = data.frame(nba_players_point_guard_v1, point_guard_cust_cluster)
View(point_guard_cust_clusters)

# View the segmentation graph
ggplot(point_guard_cust_clusters, aes(x=MP, y=PTS , color = factor(point_guard_cust_clusters$point_guard_cust_cluster))) + geom_point()

# list the predicted all-star players
View(point_guard_cust_clusters[point_guard_cust_clusters$point_guard_cust_cluster==3, ])









## SF ## 

nba_players_small_forward_v1 <- nba_structured_table[nba_structured_table$Pos == "SF",]
View(nba_players_small_forward_v1)

# Decline some columns for efficient data analysis
nba_players_small_forward_v1[, -c(1,2,3,4,5,6)] -> nba_players_small_forward_v2
View(nba_players_small_forward_v2)

## Scale the data and interpret the number of correct clusters for determining the predicted allstars
nba_players_small_forward_v2.scaled <-  scale(nba_players_small_forward_v2)
View(nba_players_small_forward_v2.scaled)


# Elbow Method
fviz_nbclust(nba_players_small_forward_v2.scaled, kmeans, method = "wss", k.max=12)
fviz_nbclust(nba_players_small_forward_v2.scaled, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 6, linetype = 2)


# for Hierarchical Clustering (Ward's Clustering) 
set.seed(1234)
dist.res <- dist(nba_players_small_forward_v2.scaled, method = "euclidean")
small_forward_cust_Clustering <- hclust(dist.res, method = "ward.D")
plot(small_forward_cust_Clustering, labels = FALSE, hang = -1) # Visualization of hclust
rect.hclust(small_forward_cust_Clustering, h=14) # Visualization of hclust - draw rectangle

# Assign clusters to origin of data points based on the cluster number
small_forward_cust_cluster <- cutree(small_forward_cust_Clustering, k=6) 
table(small_forward_cust_cluster)

# Integrating segments into dataset
small_forward_cust_clusters <- data.frame()
small_forward_cust_clusters = data.frame(nba_players_small_forward_v1, small_forward_cust_cluster)
View(small_forward_cust_clusters)

# View the segmentation graph
ggplot(small_forward_cust_clusters, aes(x=MP, y=PTS , color = factor(small_forward_cust_clusters$small_forward_cust_cluster))) + geom_point()

# list the predicted all-star players
View(small_forward_cust_clusters[small_forward_cust_clusters$small_forward_cust_cluster==5, ])




## SG ##

nba_players_shoot_guard_v1 <- nba_structured_table[nba_structured_table$Pos == "SG",]
View(nba_players_shoot_guard_v1)

# Decline some columns for efficient data analysis
nba_players_shoot_guard_v1[, -c(1,2,3,4,5,6)] -> nba_players_shoot_guard_v2
View(nba_players_shoot_guard_v2)

## Scale the data and interpret the number of correct clusters for determining the predicted allstars
nba_players_shoot_guard_v2.scaled <-  scale(nba_players_shoot_guard_v2)
View(nba_players_shoot_guard_v2.scaled)


# Elbow Method
fviz_nbclust(nba_players_shoot_guard_v2.scaled, kmeans, method = "wss", k.max=12)
fviz_nbclust(nba_players_shoot_guard_v2.scaled, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 6, linetype = 2)


# for Hierarchical Clustering (Ward's Clustering) 
set.seed(1234)
dist.res <- dist(nba_players_shoot_guard_v2.scaled, method = "euclidean")
shoot_guard_cust_Clustering <- hclust(dist.res, method = "ward.D")
plot(shoot_guard_cust_Clustering, labels = FALSE, hang = -1) # Visualization of hclust
rect.hclust(shoot_guard_cust_Clustering, h=17) # Visualization of hclust - draw rectangle

# Assign clusters to origin of data points based on the cluster number
shoot_guard_cust_cluster <- cutree(shoot_guard_cust_Clustering, k=6) 
table(shoot_guard_cust_cluster)

# Integrating segments into dataset
shoot_guard_cust_clusters <- data.frame()
shoot_guard_cust_clusters = data.frame(nba_players_shoot_guard_v1, shoot_guard_cust_cluster)
View(shoot_guard_cust_clusters)

# View the segmentation graph
ggplot(shoot_guard_cust_clusters, aes(x=MP, y=PTS , color = factor(shoot_guard_cust_clusters$shoot_guard_cust_cluster))) + geom_point()

# list the predicted all-star players
View(shoot_guard_cust_clusters[shoot_guard_cust_clusters$shoot_guard_cust_cluster==5, ])



