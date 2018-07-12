#====================================================================================================================================================#
#                                                         Clustering for mixed data types in R
#                                                   --------------------------
# Description: 
#              
#====================================================================================================================================================#
#__________________________________________________________________________________________
#
#     Authors:        Roopali Lalwani
#     Description :   Clustering for mixed data types: The program uses Gower distance to calculate a distance matrix between records/observations 
#                     and then uses PAM to perform clusters
#
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

# Detaching all loaded packages (except for base packages) 

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

# list all packages we'd need
list_of_packages <- c('cluster', 'ISLR', 'Rtsne', 'dplyr')

# cluster - for gower similarity and pam
# ISLR - for College data
# Rtsne - for visualization


# install packages, if not already installed
new_packages<- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "http://cran.us.r-project.org")

# Loading all reguired packages
lapply(list_of_packages, require, character.only = TRUE)


# Setting path to that of this document using rstudioapi package
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
path <- paste0(path, "/")
setwd(path)


# Clustering categorical data by running a few alternative algorithms is the purpose of this kernel. 
# K-means is the classical unspervised clustering algorithm for numerical data. 
##But computing the euclidean distance and the means in k-means algorithm doesn’t fare well with categorical data. 
##So instead, I will be running the categorical data through the following algorithms for clustering -
  
# By applying one-hot encoding, the data will be converted to numeric data and then it will be run thru k-means.
# 
# The data will be run through k-modes algorithm that uses modes of categorical attributes instead of the means in k-means for clustering of categorical data.
# 
##is designed for categorical variables - actually the categorical data is converted to booleans in this approach.



##1. hot encoding and then running k-means

## encoding a categorical variable as binary

## category has 4 levels - A, B, C, D, ENCODING WILL BE DONE AS
## A- 1000
## B - 0100
## C - 0010
## D - 0001

## THIS MAKES SENSE WHEN THE RESULTANT # OF COLUMNS WON'T BE TOO LARGE -- LET'S SAY YOU HAVE 500 LEVELS - NOT FEASIBLE

## 2. Using K-modes



## 3. I had also tried daisy function from cluster package in R which uses Gower distance for clustering

# 
# Gower’s distance can be used to measure how different two records are.
# The records may contain combinations of logical, numerical, categorical or text data. 
## The distance is always a number between 0 (identical) and 1 (maximally dissimilar). 
# 
# The concept of Gower distance is actually quite simple. For each variable type, a particular distance metric that works well for that type is used and scaled to fall between 0 and 1. 
# Then, a linear combination using user-specified weights (most simply an average) is calculated to create the final distance matrix. The metrics used for each data type are described below:
#   
#   quantitative (interval): range-normalized Manhattan distance
# ordinal: variable is first ranked, then Manhattan distance is used with a special adjustment for ties
# nominal: variables of k categories are first converted into k binary columns and then the Dice coefficient is used
# 
# pros: Intuitive to understand and straightforward to calculate
# cons: Sensitive to non-normality and outliers present in continuous variables, so transformations as a pre-processing step might be necessary. Also requires an NxN distance matrix to be calculated, which is computationally intensive to keep in-memory for large samples

#n records in data, 
# n*n distance matrix 


data(College)
View(College)

str(College)


college_clean <- College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps,
         isElite = cut(Top10perc,
                       breaks = c(0, 50, 100),
                       labels = c("Not Elite", "Elite"),
                       include.lowest = TRUE)) %>%
  mutate(isElite = factor(isElite)) %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, isElite)

head(college_clean)
str(college_clean)

#In short, Gower’s distance (or similarity) first computes distances between pairs of
#variables over two data sets and then combines those distances to a single value per record-pair.

gower_dist <- daisy(college_clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))


summary(gower_dist)

## now we have a customized distance matrix, we need am algorithm which uses customized distance matrix - PAM or k-mediods is one such method

## kmeans - each cluster is represented by  means of observations within the cluster (centroid)

## K-medoids clustering or PAM (Partitioning Around Medoids), 
## in which, each cluster is represented by one of the observations in the cluster. PAM is less sensitive to outliers compared to k-means.

# Choose k random entities to become the medoids
# Assign every entity to its closest medoid (using our custom distance matrix in this case)
# For each cluster, identify the observation that would yield the lowest average distance if it were to be re-assigned as the medoid. If so, make this observation the new medoid.
# If at least one medoid has changed, return to step 2. Otherwise, end the algorithm.

## bend in the elbow curve
wssplot <- function(data, nc=15, seed=100)
{
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    
    kms <- kmeans(data, centers=i,iter.max=1000, algorithm="Hartigan-Wong")
    algo <- "Hartigan-Wong"
    
    
    
    wss[i] <- sum(kmeans(data, centers=i,iter.max=1000,algorithm=algo)$withinss)
    
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within Groups Sum of Squares")
  
  return(algo)
  
}

wssplot(gower_mat)


## 3 CLUSTERS WOULD BE THE BEST

pam_results <- pam(gower_dist, diss = TRUE, k = 3)

str(pam_results)
pam_results$medoids

## gives the indexes of mediods - observations which represent the cluster


## check the observations which are mediods


# Another benefit of the PAM algorithm with respect to interpretation is that the medoids
# serve as exemplars of each cluster. From this, we see that Saint Francis University is the medoid of the Private/Not Elite cluster, 
# Barnard College is the medoid for the Private/Elite cluster, and Grand Valley State University is the medoid for the Public/Not Elite cluster.

college_clean[pam_results$medoids,]


## now let's map the cluster index to each observation

college_clean <- college_clean %>% mutate(cluster_index = pam_results$clustering)

### cluster visualization

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_results$clustering),
         name = college_clean$name)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


# Note that, in practice, you should get similar results most of the time, using either euclidean or Manhattan distance.
# If your data contains outliers, Manhattan distance should give more robust results, 
# whereas euclidean would be influenced by unusual values.
