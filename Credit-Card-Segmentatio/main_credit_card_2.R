# First, I have cleared the environment using code in R by using
rm(list = ls())

# Then, I set mydirectory using code
setwd("D:/Online_corses/Edwisor/Edwisor/Projects/credit-card")

# View the directory
getwd()

library(tidyverse) ## manipulating and visualizing data (plyr, purrr, ggplot2, knitr...)
library(readr) ## read in csv files faster
library(kableExtra) ## make nice tables with wrapper for kable()
library(cluster)    ## clustering algorithms and gap statistic
library(factoextra) ## visualization of clustering algorithm results
library(GGally) ## create matrix of variable plots
library(NbClust) ## clustering algorithms and identification of best K
library(caret) ## find correlated variables
library(DataExplorer) ##  help with different tasks throughout data exploration process.
library(dplyr) ## provides a set of tools for efficiently manipulating datasets
library(kdensity)## Handles univariate non-parametric density
library(reshape2)## to transform data between wide and long formats.
library(purrr) ##  fills the missing pieces in R's functional programming tools 
library(mlr) ## Machine Learning in R Interface to a large number of classification and regression techniques, including machine-readable parameter descriptions.
library(dendextend) ##  a set of functions for extending 'dendrogram' objects in R and to visualize and compare trees of 'hierarchical clusterings'.
library(ggforce) ## providing missing functionality to ggplot2 through the extension system


cc_data <- read.csv("credit-card-data.csv",header=TRUE)#load the dataset


glimpse(cc_data) ## show variable names, variable class, and examples of data in each column

summary(cc_data) ## get min, max, median, mean, # of NAs for each variable

#CHECKING How many NA VALUES are exits in dataset
sum(is.na(cc_data))

#CHECKING IF THERE ARE  ANY NA VALUES with visualization.
plot_missing(cc_data)

#there is 313 NA in MINIMUM_PAYMENTS . replacing it with median of MINIMUM_PAYMENTS
cc_data$MINIMUM_PAYMENTS[which(is.na(cc_data$MINIMUM_PAYMENTS))]<- median(cc_data$MINIMUM_PAYMENTS, na.rm=TRUE)
summary(cc_data)


#there is one NA in Credit_limit . replacing it with mean of credit_limit
cc_data$CREDIT_LIMIT[which(is.na(cc_data$CREDIT_LIMIT))] <- median(cc_data$CREDIT_LIMIT, na.rm=TRUE) 
summary(cc_data)


#CHECKING IF THERE ARE STILL ANY NA VALUES
plot_missing(cc_data)
# NO MISSING VALUES NOW
str(cc_data)


#removing first column  i.e. drop CUST_ID since it큦 useless.
cc_data <- cc_data[, -1]
length(cc_data)

str(cc_data)


# EXPLORATORY DATA ANALYSIS
# Histograms
plot_histogram(cc_data)

# Nearly all variables are skewed and/or have some outliers. 
# Therefore, I will keep them for this analysis.

#let큦 see how are distributed the frequency variables
frequency = data.frame(cc_data$BALANCE_FREQUENCY, cc_data$PURCHASES_FREQUENCY, cc_data$ONEOFF_PURCHASES_FREQUENCY,
                       cc_data$PURCHASES_INSTALLMENTS_FREQUENCY, cc_data$CASH_ADVANCE_FREQUENCY)
# We have data on Cash_advance_frequency that is wrong. I will clean the dataset later.
# There are also many outliers(the black dots), but I will keep then for now
boxplot(frequency, outline = TRUE, xlab='Frequency')


#let큦 see how are distributed the numeric variables
numeric_variables = select(cc_data,'BALANCE',
                           'PURCHASES',
                           'ONEOFF_PURCHASES',
                           'INSTALLMENTS_PURCHASES',
                           'CASH_ADVANCE',
                           'CREDIT_LIMIT',
                           'PAYMENTS',
                           'MINIMUM_PAYMENTS')

# There are also many outliers(the black dots), but I will keep them for now
boxplot(numeric_variables, outline = TRUE, xlab='Distribution')


#let큦 see how are distributed the numeric variables of transactions
transaction = select(cc_data, 'CASH_ADVANCE_TRX',
                     'PURCHASES_TRX')

# There are also many outliers(the black dots), but I will keep them for now
boxplot(transaction, outline = TRUE, xlab='Distribution of transactions')


#As I can see, There are many outliers. But, I can't simply drop the outliers as they may contain useful information.
#So, I'll treat them as extreme values

#let큦 see how is distributed the tenure
kde = kdensity(cc_data$TENURE)

# it shows that most of the distribution of TENURE is 12 months as a customer
plot(kde, main = 'Number of Months as a Customer')


# Correlations
#Lets take a look at how the  variables are correlated

ggplot(data = melt(cor(cc_data)), aes(x=Var1, y=Var2, fill=value)) + 
   geom_tile(color = "white")+
   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1,1), space = "Lab", 
                        name="Pearson\nCorrelation")+
   theme_minimal()+ # minimal theme
   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                    size = 12, hjust = 1))+
   geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)



# Data Cleaning
# here I am cleaning the CASH_ADVANCE_FREQUENCY because some of the data given is wrong i.e. more than frequency 1
# which is not valid in frequency.
# Lets clean the data (inputing values and eliminating wrong data) before the segmentation
cc_data = cc_data[!(cc_data$CASH_ADVANCE_FREQUENCY>1),]
# we have 8 records for which the frequency is higher that 1. I will eliminate these  records   


str(cc_data)


#  Hieracical clustering
# First I try hieracical clustering. Since all variables are categorical I use the eucldean distance.

fit_hc_clust = hclust(dist(scale(cc_data), method = "euclidean"), method = "ward")

# Regrading the dendrogramm 4 clusters seams to be a good size
plot(fit_hc_clust, labels = FALSE, sub = "", xlab = "", ylab = "Eclidean distance")
rect.hclust(fit_hc_clust, k = 4)

# So, I cut the dendrogram for 4 clusters.
hc_cluster = cutree(fit_hc_clust, k = 4)

# The PCA plots the data in two-dimensional space. Overall, there are no clear clusters in the data. 
# However, the generated clusters look quite noisy since they are overlapping.

hc_pc = prcomp(scale(cc_data))

fviz_pca_ind(hc_pc, habillage = hc_cluster)

# Let큦 take a look at the silhouette plot. It shows if an observation is associated with the right (1) or wrong (-1) cluster.
# The average silhouette width is quite low.
# Many observations probably in the wrong clusters.

hc_sil = silhouette(hc_cluster, dist(scale(cc_data), method = "euclidean"), lable = FALSE)

fviz_silhouette(hc_sil, print.summary = FALSE) + theme_minimal()


# K-Means
# Second, I try K-Meams. 
# Regarding the wss plot 4 clusters seem to be a proper number of clusters.
fviz_nbclust(scale(cc_data), kmeans, method = "wss", k.max = 10)


# Therefore, I fit K-Means with 4 clusters.
fit_km = kmeans(scale(cc_data), centers = 4)


# This PCA plot looks better then the plot before.
fviz_pca_ind(hc_pc, habillage = fit_km$cluster)



# Let큦 take a look at this silhouette plot. Overall, 
# the result is better than before. However, especially cluster 1 and 3 
# have still some observations which are still in the wrong cluster.
# But it큦 the best solution for now which I will use for interpretation.

hc_sil = silhouette(fit_km$cluster, dist(scale(cc_data), method = "euclidean"), lable = FALSE)

fviz_silhouette(hc_sil, print.summary = FALSE) + theme_minimal()


# Interpretation
# In order to iterpretate the clusters grouped boxplots will be used.
c = cc_data

c$cluster = fit_km$cluster

c_plots = melt(c, id.var = "cluster")

c_plots$cluster = as.factor(c$cluster)

c_plots %>%
   ggplot(aes(x = variable, y = value)) +
   geom_boxplot(aes(fill = cluster), outlier.size = 1) +
   facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 1) +
   labs(x = NULL, y = NULL) +
   theme_minimal()


c_plots %>%
   ggplot(aes(x = variable, y = value)) +
   geom_boxplot(aes(fill = cluster), outlier.size = 1) +
   facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 2) +
   labs(x = NULL, y = NULL) +
   theme_minimal()


c_plots %>%
   ggplot(aes(x = variable, y = value)) +
   geom_boxplot(aes(fill = cluster), outlier.size = 1) +
   facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 2, page = 3) +
   labs(x = NULL, y = NULL) +
   theme_minimal()



# From the analysing the box plot for all 4 cluster, the clusters can be interpreted as follows (marketing wise) from my point of view:
   
# Cluster 1: Frequent user, with (probably) lower income that spends his money mostly on consumer goods.
# Cluster 2: Frequent user, with (probably) higher income that spends his money mostly on consumer goods.
# Cluster 3: Mid to rare users, with (probably) mid to high income which spends his money more for higher priced products with longterm use.
# Cluster 4: Rare user, with (probably) mid to low income which spends his money more on consumer goods

