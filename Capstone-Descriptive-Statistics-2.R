###
### Capstone project - Data descriptive statistics
###

library(tidyr)
library(dplyr)
library(ggplot2)

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
descStats <- function(factsDF){
  facts.stats <- t(factsDF %>% select(which(sapply(., is.numeric))) %>% 
                     summarise_each(funs(sum(is.na(.)),n())) %>%
                     gather(variable, value) %>%
                     separate(variable, c("var", "stat"), sep = "\\_") %>%
                     spread(var, value) %>%
                     select( -stat,-cikNumber) 
  )
  colnames(facts.stats) <- c("TotalCount","NAcount")
  return(facts.stats)
}
### user defined function for descriptive statistics of numeric variables of data frame
### arguments:
###       factsDF: data frame with variables
### returns:
###       sunStatsDF: data frame containing summary statistics of each variable, a row per variable
sumStats <- function(factsDF){
  sumStatsDF <- as.data.frame(matrix(ncol=7, nrow=1))
  names(sumStatsDF) <- c("Min.","1st Qu.", "Median", "Mean",  "3rd Qu.","Max.", "NA's") 
  tmpDF <- factsDF %>% select(which(sapply(., is.numeric)))
  for (nme in names(tmpDF)) {
    srt <- as.data.frame(t(as.matrix(summary(tmpDF[[nme]]))))
    ifelse(ncol(srt) ==7,sumStatsDF <- rbind(sumStatsDF,srt),NA)
  }
  sumStatsDF <- cbind(names(tmpDF),sumStatsDF) 
  colnames(sumStatsDF)[1] <- "Variable"
  return(sumStatsDF)
}

#### End of User Defined Functions

#### Load financial facts
cash14 <- read.csv("cashflow_statement.csv")
balsh14 <- read.csv("balance_sheet.csv")
income14 <- read.csv("income_statement.csv")
ratios14 <- read.csv("ratios.csv")

### Load filings information
filings14 <- read.csv("filings.csv")


### Data set statistics
print("Data set statistics")
dset.stats <- sapply(list(balsh14,cash14,income14,ratios14),dim)
colnames(dset.stats) <- c("Balancesheet","Cashflow","IncomeStatement","Ratios")
rownames(dset.stats) <- c("Row count","# of variables")
print(dset.stats)

### reference: http://www.statmethods.net/stats/descriptives.html

### Descriptive statistics on the data
### Descriptive statistics - by SIC code
### Get the assignedSic code to rations, by joining ratios with filings
ratios14NSic <- ratios14 %>% 
  left_join(.,filings14, by = c("accessionNumber" = "accessionNumber", "cikNumber" = "cikNumber")) %>%
  select(accessionNumber:dividendCoverage,assignedSic)

### explore the data now
descStats(ratios14NSic)
## NA count of all variables ratios


balsh14NSic <- balsh14 %>% 
  left_join(.,filings14, by = c("accessionNumber" = "accessionNumber", "cikNumber" = "cikNumber")) %>%
  select(accessionNumber:tangibleAssetsNet,assignedSic)

descStats(balsh14NSic)

### Cashflow & Sic code
cash14NSic <- cash14 %>% 
  left_join(.,filings14, by = c("accessionNumber" = "accessionNumber", "cikNumber" = "cikNumber")) %>%
  select(accessionNumber:cashAndCashEquivalentsChanges,-duration,assignedSic)
descStats(cash14NSic)
### Income statement & Sic code
income14NSic <- income14 %>% 
  left_join(.,filings14, by = c("accessionNumber" = "accessionNumber", "cikNumber" = "cikNumber")) %>%
  select(accessionNumber:incomeNetApplicableToCommonShares,-duration,assignedSic)

### Summary statistics - Ratios
ratio.stats <- sumStats(ratios14NSic)
print(ratio.stats)
## Summary statistics - Balancesheet
balancesheet.stats <- sumStats(balsh14)
print(balancesheet.stats)
## Summary statistics - Cashflow
cash.stats <- sumStats(cash14NSic)
print(cash.stats)
## Summary statistics - Income Statement
income.stats <- sumStats(income14NSic)
print(income.stats)

### histogram for ratio variables
####### Histograms of ratios distributions
## current, quick, cash ratios
library(reshape2)
ratios.melt <- melt(ratios14[,5:7])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='black',breaks=seq(-2,20,by=0.1)) + 
  facet_wrap(~variable)
## ProfitMargins..
ratios.melt <- melt(ratios14[,8:12])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='black', fill='blue',breaks=seq(-1,2,by=0.1)) + 
  facet_wrap(~variable)
### returnOnSales...returnOnCapitalEmployed
ratios.melt <- melt(ratios14[,13:16])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='blue',breaks=seq(-1,1,by=0.1)) + 
  facet_wrap(~variable)
## debt ratios
ratios.melt <- melt(ratios14[,17:20])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='blue',breaks=seq(-1,2,by=0.1)) + 
  facet_wrap(~variable)
### assetTurnover, 
ratios.melt <- melt(ratios14[,22:23])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='blue',breaks=seq(-1,3,by=0.05)) + 
  facet_wrap(~variable)
## receivablesTuronver, inventoryTurnover
ratios.melt <- melt(ratios14[,24:25])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='blue',breaks=seq(-1,20,by=0.1)) + 
  facet_wrap(~variable)
## cashflow,sales
ratios.melt <- melt(ratios14[,26:27])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='blue',breaks=seq(-2,5,by=0.1)) + 
  facet_wrap(~variable)
## debtCoverage
ratios.melt <- melt(ratios14[,28:30])
ggplot(ratios.melt, aes(value)) +
  geom_histogram( colour='blue', fill='blue',breaks=seq(-2,5,by=0.1)) + 
  facet_wrap(~variable)



