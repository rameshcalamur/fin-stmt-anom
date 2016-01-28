
###
### Capstone project - Accounting Anomalies, Benford law/distribution tests
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
#### Arguments: 
####        factsDF:  data frame with multiple variables as columns 
####        
####  Returns:
####        facts.stats - statistics on NA count
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


#### End of User Defined Functions

#### Load financial facts
cash14 <- read.csv("cashflow_statement.csv")
balsh14 <- read.csv("balance_sheet.csv")
income14 <- read.csv("income_statement.csv")
ratios14 <- read.csv("ratios.csv")

### Load filings information
filings14 <- read.csv("filings.csv")


### Accounting anomalies - Balancesheet
### By definition, balancesheet should balance "Total Assets" ==  "Liabilities" + "Equity" 
bAccAnom <- balsh14 %>% mutate( accAnomaly = (assetsTotal != liabilitiesAndStockholdersEquity )) 

bAccAnom %>% ggplot( aes(x=log(abs(assetsTotal)),y=log(abs(liabilitiesAndStockholdersEquity))))+
  geom_point(aes(color= accAnomaly)) + 
  xlab("Total Assets")+
  ylab("Liabilities + Equity")+
  ggtitle("Accounting Anomaly - Balancesheet (scaled)")

cat("Number of balancesheet anomalies:",sum(bAccAnom$accAnomaly,na.rm = TRUE))

#### Outliers 5 Standard Deviations from median
####


assets.mad <- as.numeric(balsh14 %>% summarise(mad(assetsTotal,na.rm = TRUE)))

balsh14 %>% filter((log(abs(assetsTotal)/assets.mad)) > 5) %>%
  summarize( outCount = n())


### Benford' law - indicators for anomalies
### Calculate Mean Absolute Deviation for Benford distribution of Balancesheet 
### Benford data set applies to each company statements (row) and numeric columns

library(benford.analysis)
n <-  nrow(balsh14)

balsh14.benford.MAD <- numeric(length = n)
for (i in 1:n){
  balsh14.stmtdat <- as.numeric(balsh14[i,5:ncol(balsh14)])
  balsh14.stmtdat[is.na(balsh14.stmtdat)] <- 0.01
  balsh14.benford.MAD[i] <- MAD(benford(as.numeric(balsh14.stmtdat)))
}  
## summary stats of MAD statistic of Benford distro of balancesheet 
summary(balsh14.benford.MAD)
benf.MAD.crit <- 0.018 ## Empirical MAD value upper threshold for non-conforming 
qplot(seq_along(balsh14.benford.MAD),balsh14.benford.MAD,color = balsh14.benford.MAD > 0.018,
      ylab = "Mean Absolute Deviation",
      xlab = "Index", 
      main = "Benford distribution of Balancesheet data  - MAD test") +
  geom_hline(yintercept = 0.018,color="blue")

### Kolmogorov-Smirnov Test for Benford distribution
### Table Reference: http://www.real-statistics.com/statistics-tables/kolmogorov-smirnov-table/
library(BenfordTests)
n <-  nrow(balsh14)
#balsh14.benford.ks <- numeric(length = n)
#for (i in 1:n){
#  balsh14.stmtdat <- as.numeric(balsh14[i,5:ncol(balsh14)])
#  balsh14.stmtdat[is.na(balsh14.stmtdat)] <- 0.01
#  balsh14.benford.ks[i] <- ks.benftest(balsh14.stmtdat)$statistic[[1]]
#}  
### critical test at 5% level
#balsh14.ks.crit.value <- 1.36 / sqrt(ncol(balsh14)-4)
#summary(balsh14.benford.ks)
#sum(balsh14.benford.ks < balsh14.ks.crit.value)
#summary(balsh14.benford.ks)
#### End KS test

#### Begin Chi Squared Benford test
#### using Table from http://www.itl.nist.gov/div898/handbook/eda/section3/eda3674.htm
#### Chi Squared critical value: Degrees of freedom: 42, 
balsh14.chisq.crit.value <- 58.124 # degress of freedom: 42, significance level 0.05

balsh14.benford.chisq <- numeric(length = n)
for (i in 1:n){
  balsh14.stmtdat <- as.numeric(balsh14[i,5:ncol(balsh14)])
  balsh14.stmtdat[is.na(balsh14.stmtdat)] <- 0.01
  balsh14.benford.chisq[i] <- chisq.benftest(balsh14.stmtdat)$statistic[[1]]
}  

### The obtained value is greater than the critical value, so we can indeed say that 
### the data do not fit Benford’s Law
sum(balsh14.benford.chisq > balsh14.chisq.crit.value)
qplot(seq_along(balsh14.benford.chisq),balsh14.benford.chisq,
      color = balsh14.benford.chisq > balsh14.chisq.crit.value,
      ylab = "Chi Squared",
      xlab = "Index", 
      main = "Benford distribution of Balancesheet data  - Chi Squared test") +
  geom_hline(yintercept = balsh14.chisq.crit.value,color="blue")

#### Benford chart
benDist <- pbenf(digits = 1) ## baseline Benford distribution
datDist <- signifd.analysis(balsh14.stmtdat,graphical_analysis=FALSE)$summary["freq",]
benDatDist <- as.data.frame(cbind(benDist,dataFreq = datDist))
ggplot(aes(x=seq_along(benDist),y=benDist),data=benDatDist)+
  geom_line(color="black", size = 1)+
  geom_line(y=datDist,color="red")+
  scale_x_continuous(breaks=seq(1,10,1))+
  xlab("Digit")+
  ylab("Frequency")+
  ggtitle("Benford distribution - Balancesheet Anomaly sample")
#### End Chi Squared Benford test
### Benford test on all of Balancesheet records
balshNumValues <- as.vector(as.matrix(balsh14[,5:ncol(balsh14)]))
balshNumValues <- balshNumValues %>% na.omit()
balsh.datDist <- signifd.analysis(balshNumValues,graphical_analysis=FALSE)$summary["freq",]
balsh.benDatDist <- as.data.frame(cbind(benDist,dataFreq = balsh.datDist))
ggplot(aes(x=seq_along(benDist),y=benDist),data=balsh.benDatDist)+
  geom_line(color="black", size = 1)+
  geom_line(y=balsh.datDist,color="red")+
  scale_x_continuous(breaks=seq(1,10,1))+
  xlab("Digit")+
  ylab("Frequency")+
  ggtitle("Benford distribution - All Balancesheet values")
