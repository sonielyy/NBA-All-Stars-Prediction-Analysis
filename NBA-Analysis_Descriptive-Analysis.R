# Install Packages for Working with R programming

install.packages("readr") # for Read CSV
library(readr)

install.packages("ggplot2") # for Visuals
library(ggplot2)

# Set working directory for importing CSV
setwd("C:/Users/PC/Documents/R_WorkFiles/R_Work_Files/R_Work_Files")
nba_raw_total_data <- read_csv("nba_2023-2024_current_stats.csv") # Import Total Player Data


# Display the Data First

str(nba_raw_total_data) # Data types of variables
colnames(nba_raw_total_data) # Column names
head(nba_raw_total_data) # First 6 rows
tail(nba_raw_total_data) # Last 6 rows
dim(nba_raw_total_data) # Dimensions of rows x columns
View(nba_raw_total_data)
# Change the name of some variables for clarity 
colnames(nba_raw_total_data)[5] <- 'Team'
colnames(nba_raw_total_data)[1] <- 'Rank'
colnames(nba_raw_total_data)

# Convert team and position variables into factors

class(nba_raw_total_data$Team)
class(nba_raw_total_data$Pos)

nba_raw_total_data$Team <- as.factor(nba_raw_total_data$Team)
nba_raw_total_data$Pos <- as.factor(nba_raw_total_data$Pos)

levels(nba_raw_total_data$Team) # Check for the convertion accuracy
levels(nba_raw_total_data$Pos)


# Detection of the NA Columns // Create new version of dataset

nba_total_stats <- nba_raw_total_data
colnames(nba_total_stats)
any(is.na(nba_total_stats$MP)) #False
any(is.na(nba_total_stats$FG)) #False
any(is.na(nba_total_stats$FGA)) #False
any(is.na(nba_total_stats$"FG%")) #True
any(is.na(nba_total_stats$"3P")) #False
any(is.na(nba_total_stats$"3PA")) #False
any(is.na(nba_total_stats$"3P%")) #True
any(is.na(nba_total_stats$"2P%")) #True
any(is.na(nba_total_stats$"FT%")) #True
any(is.na(nba_total_stats$"eFG%")) #True

# Cleaning the Data (Eliminating the Columns has NA Data)
# (we will re-create this columns)

nba_total_v3 <- nba_total_stats[, c("Rank", "Player", "Age", "Team", "Pos", "G", "GS", "MP", "FG", "FGA", "3P", "3PA", "2P", "2PA", "FT", "FTA", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")]
is.na(nba_total_v3)
any(is.na(nba_total_v3)) # Not found any NA Value

# When we examine the dataset, we find that some players' stats recorded as cumulative with the 'TOT' record tag. We will clean.

nba_total_v3[nba_total_v3$Team == "TOT", ]
nba_total_v3[nba_total_v3$Player == "Daniel Theis", ] # As we can see in the example, 'TOT' row includes the players' all stats in the different teams. We will delete the 'TOT' column and apply summarization during the analysis process.

nba_total_v4 <- nba_total_v3[nba_total_v3$Team != "TOT", ]

summary(nba_total_v4$Team)

nba_total_v5 <- droplevels(nba_total_v4)
summary(nba_total_v5$Team)


# Descriptive Statistics


# -- Age & PTS

summary(nba_total_v5$Age)
summary(nba_total_v5$PTS)

range(nba_total_v5$Age)
range(nba_total_v5$PTS)

quantile(nba_total_v5$Age)
quantile(nba_total_v5$PTS)
quantile(nba_total_v5$Age,prob=c(0.25,0.75))

cor(nba_total_v4$Age,nba_total_v4$PTS)
cor(nba_total_v4$FTA, nba_total_v4$FT)

# -- Effects of Important Stats (such as Assists) to PTS

cor(nba_total_v4$MP, nba_total_v4$PTS)
cor(nba_total_v4$FG, nba_total_v4$PTS)
cor(nba_total_v4$"3P", nba_total_v4$PTS)
cor(nba_total_v4$"2P", nba_total_v4$PTS)
cor(nba_total_v4$FT, nba_total_v4$PTS)

cor(nba_total_v4$G, nba_total_v4$GS)
cor(nba_total_v4$ORB, nba_total_v4$PTS)
cor(nba_total_v4$DRB, nba_total_v4$PTS)
cor(nba_total_v4$TRB, nba_total_v4$PTS)
cor(nba_total_v4$AST, nba_total_v4$PTS)
cor(nba_total_v4$STL, nba_total_v4$PTS)
cor(nba_total_v4$BLK, nba_total_v4$PTS)
cor(nba_total_v4$TOV, nba_total_v4$PTS)
cor(nba_total_v4$PF, nba_total_v4$PTS)

levels(nba_total_v4$Pos)

# Correlation matrix

nba_total_v6 <- nba_total_v5[,-c(1,2,4,5)]
nba_cor_stats <- cor(nba_total_v6)
round(nba_cor_stats, 2)

# Visualize corr

install.packages("corrplot")
library(corrplot)

corrplot(nba_cor_stats, type="upper", order="hclust", tl.col = "black", tl.srt = 45)

## Data Visualizations

# -- Data Visualizations for Distribution of Players

range(nba_total_v5$PTS)
max(nba_total_v5$PTS) -> max_pts
hist(nba_total_v5$PTS, breaks = seq(0,1077,3), main = "Total PTS Distribution", xlab = "Points", ylab = "Number of Players",
    col = "lightblue")

range(nba_total_v5$Age)
hist(nba_total_v5$Age, breaks = seq(19,39, 4), main = "Age Distribution", xlab = "Age", ylab = "Number of Players",
     col = "orange")

summary(nba_total_v5$Pos)
position_counts <- table(nba_total_v5$Pos)
position_percentages <- round(100 * position_counts / sum(position_counts), 1)
labels <- paste(names(position_percentages), " - ", position_percentages, "%", sep="")
pie(table(position_counts),
    col=c("blue","red",  "yellow", "green", "orange"),
    main="Player Distribution By Positions", labels=labels)
?pie


# -- Data Visualizations for Finding Correlation

with(nba_total_v5, plot(PTS, Age))

#using lattice package
library(lattice)
xyplot(PTS ~ Age, nba_total_v5)
xyplot(PTS ~ Age, nba_total_v5, col = "violet", pch = 1, main = "Distribution of Players by Age and PTS")

