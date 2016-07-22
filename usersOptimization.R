# Escape Project

# Improvement on Itinerary Generation Algorithm 

# We have at our disposal data on 50 users divided into 9 variables, 7 of which are designed to allow us to form clusters among users:

library(readr)
library(randomNames)
library(stringr)
library(cluster)
library(dplyr)

users <- read_csv("D:/iXperience/Projects/escape/users.csv")

#----------------------------------

# Model Analysis: Is the model worth implementing?

# Data setup

users <- users[, -which(colnames(users) == "hometown")]

users[, 1] <- NULL
class(users) <- "data.frame"

for (i in 6:9) {
  users[, i] <- as.factor(users[, i])
}
users$sex <- as.factor(users$sex)

colnames(users)[10:14] <- c("tablemountain", "longstreet", "lionshead", "biscuitmill", "mamaafrica")

# reorganize column order
users <- cbind(users[, 1:5], users[, 15:16], users[, 6:14])

#---------------------------------------------------

# Plan: start with a k-means clustering algorithm and find the peak number of clusters
# We exclude the ratings from the clustering, since these are the reason we want to create a clustering of users
# We also exclude username and userid which are insignificant because they are unique to each user
# State must also be excluded because it contains NAs for non-us citizens


# In order t run a kmeans algorithm, we must use numeric values only. We thus convert every column to its factor values

clusterusers <- users[, c(3:6, 8:11)]
clusterusers$city <- as.factor(clusterusers$city)
clusterusers$homecountry <- as.factor(clusterusers$homecountry)
clusterusers[, 2:8] <- sapply(2:8, function(n) { clusterusers[, n] <- as.numeric(clusterusers[, n]) })

# All done! We can now run the kmeans algorithm for various number of clusters

val <- c()
for (i in 1:20) {
  y <- kmeans(x = clusterusers, centers = i)
  val <- c(val, y$betweenss / y$totss)
}
val

# We plot the elements of val side to side to identify the peak
plot(x = 1:20, y = val, type = "b", main = "Variance Covered vs # of Clusters", xlab = "Number of Clusters", ylab = "Percent Variance Covered")

# It seems that i=5 is the optimal number of clusters. 
optimal.cluster <- kmeans(x = clusterusers, centers = 5)
clusters <- optimal.cluster$cluster

#----------------------------------

# Attempt to use agglomerative clustering on the dataset

users.dist <- dist(clusterusers)
users.hclust <- hclust(users.dist)
plot(users.hclust)

# We can once again identify five clusters. We distinguish them using cutree.
agg.clusters <- cutree(users.hclust, k = 5)

# Let us append the cluster belongings to the original dataframe
users$stdcluster <- clusters
users$aggcluster <- agg.clusters

#-----------------------------------------------------------

# Which cluster is the most efficient? We can use the accuracy of rating prediction of each of them
# Given any point x in the dataset, it is now associated to a cluster. Given any event e, we can compute the squared difference between 
# the average rating of the corresponding cluster for that event and the rating of x with the same difference with other clusters
# The more accurate the clustering, the more significant the difference should be between these values

# Function carrying the above process for one element at random using the standard clusters
# We take our observations from the original users data in order to have access to the ratings

n <- sample(nrow(users), 1)
n.obs <- users[n, ]
n.cluster <- users %>% filter(stdcluster == n.obs$stdcluster)

n.cluster
# call cluster.diff on n.obs' cluster
n.cluster.results <- cluster.diff(n.obs, n.cluster)

# compare with the results of the function call with the other clusters
other.clusters <- users %>% filter(stdcluster != n.obs$stdcluster) 
indices <- setdiff(1:length(unique(users$stdcluster)), n.obs$stdcluster)
other.clusters.results <-
  sapply(indices, function(i) { cluster.diff(n.obs, users %>% filter(stdcluster == i)) })


#' @param the observation whose rating distance from the cluster average rating we want to compute
#' @param cluster 
#' @return value that indicates the average distance between the ratings of the observation and the cluster
#' @description takes in an observation and returns the sum of the squared differences between the observation's fields and those of the given cluster's observations
cluster.diff <- function(n.obs, cluster) {
  diff = 0
  
  for (p in 1:nrow(cluster)) {
    for (event in 12:16) {
      curr.cluster <- cluster[p, ]
      diff = diff + (n.obs[event] - curr.cluster[event])**2
    }
  }
  
  return(diff / nrow(cluster))
}

# Conclusion
n.cluster.results
other.clusters.results

# The cluster to which the point belongs does indeed produce the smallest output through the cluster.diff
# function. Let's get more solid data and run the same test for every point in the dataset

# Processus: run through every single point in the dataset, get its cluster, run cluster.diff on it,
# then run it on every other cluster along with the same point.
# Return true if the point's cluster produces a smaller output than the majority (3 or 4) of the other 
# clusters, and false if it doesn's. We should ultimately be able to compute an accuracy rate.

multiple.clusters <- sapply(1:nrow(users), function(n) {
  n.obs <- users[n, ]
  n.cluster <- users %>% filter(stdcluster == n.obs$stdcluster)
  n.cluster.results <- cluster.diff(n.obs, n.cluster)
  
  other.clusters <- users %>% filter(stdcluster != n.obs$stdcluster) 
  indices <- setdiff(1:length(unique(users$stdcluster)), n.obs$stdcluster)
  other.clusters.results <-
    sapply(indices, function(i) { cluster.diff(n.obs, users %>% filter(stdcluster == i)) })
  
  # majority check
  sum(as.vector(n.cluster.results) < other.clusters.results) > 2
})

# we sum the boolean vector and divide over by the number of values
sum(multiple.clusters) / length(multiple.clusters)
# 58% success rate. Not bad given that we are looking at one cluster out of 5, so that the chance success rate
# would be 20%

#------------------------------

# Let's apply the same procedure, this time around using the clusters produced by agglomeration

agg.multiple.clusters <- sapply(1:nrow(users), function(n) {
  n.obs <- users[n, ]
  n.cluster <- users %>% filter(aggcluster == n.obs$aggcluster)
  n.cluster.results <- cluster.diff(n.obs, n.cluster)
  
  other.clusters <- users %>% filter(aggcluster != n.obs$aggcluster) 
  indices <- setdiff(1:length(unique(users$aggcluster)), n.obs$aggcluster)
  other.clusters.results <-
    sapply(indices, function(i) { cluster.diff(n.obs, users %>% filter(aggcluster == i)) })
  
  # majority check
  sum(as.vector(n.cluster.results) < other.clusters.results) > 2
})

# Results
sum(agg.multiple.clusters) / length(agg.multiple.clusters)
# We get a 66.7% accuracy rate! This is a significant improvement over the previous cluster group, and 
# is definitely high enough to gain our interest when it comes to suggesting events to elements of a cluster
# based on the other points in the cluster's previous ratings.

# Conclusion:
# The agglomeration clustering algorithm produced a success rate that is significantly higher than chance, 
# which in concrete term means that we can quite confidently rely on the interests of the cluster 
# to which a user belong when trying to choose the optimal event that should be suggested to that user.
# Because of the very limited size of the dataset at the moment, along with the potential for improvement
# of the model, we can only be optimistic as to what predictive power we may hope to acquire over time.
View(users)

#----------------------------

#' @param userid : the id of the user for whom we are trying to generate a customized suggestion
#' @param evenset : dataframe containing events from which we wish to choose the best fit for the user
#' @return the event row that was rated the highest by the cluster to which the user belongs
findBestEvent(userid, eventset, city) {
  
  library(readr)
  library(randomNames)
  library(stringr)
  library(cluster)
  library(dplyr)
  
  # Connect to server
  con <- dbConnect(SQLite(), "escapeDB.sqlite")
  
  # Read in users, users_event table, get user
  users <- dbReadTable(con, "users")
  users_event <- dbReadTable(con, "users_event")
  user <- users[which(users$userid == userid), ]
  
  # TODO: might be more efficient to generate k-means algorithm only occasionally (ex: whenever a new user is added) and to store
  # the clustering labels as a variable within the users database
  
  # Generate the standardized clusterusers table to be able to call the k-means algorithm
  clusterusers <- users[, c(3:6, 8:11)]
  clusterusers$city <- as.factor(clusterusers$city)
  clusterusers$homecountry <- as.factor(clusterusers$homecountry)
  clusterusers[, 2:8] <- sapply(2:8, function(n) { clusterusers[, n] <- as.numeric(clusterusers[, n]) })
  
  # generate k-means agglomeration clustering algorithm with k = 5
  users.dist <- dist(clusterusers)
  users.hclust <- hclust(users.dist)
  
  agg.clusters <- cutree(users.hclust, k = 5)
  
  # filter users by selecting those users which belong to the same cluster as the original user
  users$aggcluster <- agg.clusters
  users <- users %>% filter(aggcluster == user$aggcluster)
  
  # get ranked list of evenset
  # assumes structure of users_event: userid, cityname, eventid, eventrating 
  custom_event <- users_event %>% filter(userid %in% users$userid & userid != user$userid) # only users in the cluster are left in users dataframe
  custom_event <- custom_event %>% filter(eventid %in% evenset$eventid & cityname == city) %>% arrange(desc(eventrating))
  
  # we should now have the best liked event among the cluster elements as the highest ranked in the evenset dataframe
  return(custom_event[1, ])
}
