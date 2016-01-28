###
### Capstone project - Model exploration
###

library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
#### Helper Functions - User Defined Functions
#
#### user defined function for selecting numeric values and filtering NA values
#### Arguments: 
####        factsDF:  data frame with multiple variables as columns 
####        maxNACnt: threshold count of NAs values for the variable, if below this, the variable
####                  would be included for calculating correlation matrix
####  Returns:
####        numDFforCor - data frame of numeric variables for correlation
####
numericNAvars <- function(factsDF, maxNACnt){
  ### identify the numeric variables and count of NAs of the variables
  numNAvarDF <- factsDF %>% select(which(sapply(., is.numeric))) %>%
    summarise_each(funs(sum(is.na(.)))) %>% 
    gather(bvar,NAcnt) %>%
    filter( NAcnt > 0 & NAcnt < maxNACnt)
  ## from the data frame, select the numeric variables that have NAs below threshold   
  numDFforCor <-  factsDF %>% select(one_of(as.character(numNAvarDF$bvar))) %>%
    na.omit()
  rm(numNAvarDF)
  return(numDFforCor)
}

## Altman Z Score
## Arguments:
##    currentAssetsTotal:
##    currentLiabilitiesTotal:
##    assetsTotal:
##    retainedEarnings:
##    incomeBeforeInterestAndTax:  equivalent for EBIT
##    stockholderEquityTotal: equivalent for MarketCapAtEndOfPeriod
##    liabilitiesTotal:
##    revenueTotal
##  Returns:
##    zScore : computed Altman Z-score
##
altmanZscore <- function(currentAssetsTotal, currentLiabilitiesTotal, assetsTotal,
                         retainedEarnings, incomeBeforeInterestAndTax,
                         stockholderEquityTotal,liabilitiesTotal,revenueTotal ){
  workingCapital <- currentAssetsTotal - currentLiabilitiesTotal
  zScore <- (1.2 * (workingCapital / assetsTotal) +
               1.4 * (retainedEarnings / assetsTotal) +
               3.3 * (incomeBeforeInterestAndTax / assetsTotal) +
               0.6 * (stockholderEquityTotal / liabilitiesTotal) +
               0.99 * (revenueTotal / assetsTotal) )
  return (zScore)
}
#### End of User Defined Functions

#### Load financial facts
cash14 <- read.csv("cashflow_statement.csv")
balsh14 <- read.csv("balance_sheet.csv")
income14 <- read.csv("income_statement.csv")
ratios14 <- read.csv("ratios.csv")

### Load filings information
filings14 <- read.csv("filings.csv")
### get a random sample of 
sampleSize <- 3500


ratios.sample.pre <- ratios14 %>% filter(kind == "ttm") %>%
  select(which(sapply(., is.numeric))) %>%
#  select(assetTurnover,inventoryTurnover,receivablesTurnover) %>%
  na.omit() %>% 
  sample_n( sampleSize, replace = FALSE)

## scale the data
medians <- apply(ratios.sample.pre,2,median)
mads <- apply(ratios.sample.pre,2,mad)
ratios.sample <- as.data.frame(scale(ratios.sample.pre,center=medians,scale=mads))
#ratios.sample <- as.data.frame(scale(ratios.sample.pre,center=T,scale=T))

#ratios.sample <- as.data.frame(scale(ratios.sample.pre))

### scale and calculate distance using daisy function
library(fastcluster)
library(cluster)
ratios.dist <- dist(ratios.sample)
ratios.fit.WD <- hclust(ratios.dist,method = "ward.D")
plot(ratios.fit.WD)

groups <- cutree(ratios.fit.WD, k=7) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(ratios.fit.WD, k=7, border="red")

ratios.fit.WD2 <- hclust(ratios.dist,method = "ward.D2")
plot(ratios.fit.WD2)

groups.WD2 <- cutree(ratios.fit.WD2, k=5) # cut tree into k clusters
# draw dendogram with red borders around the kclusters 
rect.hclust(ratios.fit.WD2, k=5, border="red")

table(groups.WD2) ## count grouped by clusters  

### items in different cluster
ratios.sample$inventoryTurnover[groups.WD2 == "2"]
ratios.sample$receivablesTurnover[groups.WD2 == "2"]

## now try different clustering model method
ratios.fit.cen <- hclust(ratios.dist,method = "median")
plot(ratios.fit.cen)

groups.cen <- cutree(ratios.fit.cen, k=5) # cut tree into k clusters
# draw dendogram with red borders around the kclusters 
rect.hclust(ratios.fit.cen, k=5, border="red")

table(groups.cen)
##
library(fpc)
cluster.WD2.cen.stats <- cluster.stats(ratios.dist,groups.WD2,groups.cen)

#### plot to show clusters on sample data set
ggplot(aes(x = inventoryTurnover, y = receivablesTurnover ),data=ratios.sample) + 
  geom_point(color = groups.WD2) +
  coord_cartesian(ylim = c(-0.5,0.5), xlim=c(-0.1,0.5))
## plot for centroid
ggplot(aes(x = inventoryTurnover, y = assetTurnover),data=ratios.sample) + 
  geom_point(color = groups.WD2) +
  coord_cartesian(ylim = c(-0.5,0.5), xlim=c(-0.5,0.5))

#ggplot(aes(x = receivablesTurnover, y = inventoryTurnover),data=ratios14) + 
#  geom_point()
### now, do k-means clustering 
wss <- (nrow(ratios.sample)-1)*sum(apply(ratios.sample,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ratios.sample, 
                                     centers=i)$withinss)
##  A plot of the within groups sum of squares by number of clusters 
#  extracted can help determine the appropriate number of clusters.
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
set.seed(1)
k <- 6
kmcRatios <- kmeans(ratios.sample, centers = 6)
##
table(kmcRatios$cluster)
ggplot(aes(x = receivablesTurnover, y = inventoryTurnover),data=ratios.sample) + 
  geom_point(color = kmcRatios$cluster)+
  coord_cartesian(ylim = c(-0.2,1), xlim=c(-0.1,1))

### do silhouette plot
groups.WD2 <- cutree(ratios.fit.WD2, k=5)
sil1 <- silhouette(cutree(ratios.fit.WD2, k=5),ratios.dist)

###
ggplot(aes(x = seq(inventoryTurnover), y = inventoryTurnover),
       data=subset(ratios.sample,kmcRatios$cluster != 5)) + 
  geom_point(color = "red")+
  coord_cartesian(ylim = c(-10,10000), xlim=c(-10,40000))

ratios.sample %>% filter(kmcRatios$cluster != 5) %>% select(inventoryTurnover,receivablesTurnover)

summary(ratios.sample$inventoryTurnover)

### turnoverRatios outside the norms
### Asset Turnovers : max 10 http://csimarket.com/screening/index.php?s=at
### Receivable Turonver: http://csimarket.com/screening/index.php?s=rt
ratios14 %>% filter(assetTurnover > 100 | inventoryTurnover > 1000 | receivablesTurnover > 1000 ) %>% 
    select(cikNumber,assetTurnover,inventoryTurnover,receivablesTurnover) %>% summarise(n=n())

summary(subset(ratios14$grossProfitMargin,ratios14$grossProfitMargin > 100))

length(subset(ratios14$grossProfitMargin,ratios14$grossProfitMargin > 500))