########################
##### Trial Design ##### 
########################

## Required Packages
#install.packages("clinfun")
library(clinfun)

### design param
intFrac <- c(1) # This defines where interim analyses for efficacy will take place - just having 1 means no interim analyses
intPow <- c(1) # This defines where interim analyses for futility will take place - just having 1 means no interim analyses
alpha <- 0.05
power <- 0.9
HR <- 0.74
tails <- 2


### Recruitment Estimates
nSite <- 40		## Number of Sites
rpm <- 0.5		## Recruitment per month
openRate <- 1	## Rate of opening sites
maxTime <- 60	## Length of recruitment
folUp <- 24		## Follow-up per patient
penal <- 0.5	## This is a penalising factor that just states that each site will only recruit at half it's anticipated rate in the first month
rec.forcast(nSite,rpm,openRate,maxTime) ## This gives an estiamted recruitment rate but isn't important


### Survival function - This example estimates a survival function based on the exponential distribution but others can be used
t <- c(0:(maxTime+folUp)) 	## Time
lam <- -log(0.5)/24			## Hazard Rate (the example here is for 24 months median survival)
S_t <- exp(-lam*t)			## Survival funtions (Exponential Distirbution)

## Survival Design
design <- survivalDesign(alpha,power,tails,HR,intFrac,intPow,nSite,rpm,openRate,maxTime,penal,folUp,S_t,plotDes=T,results=T)
design[[1]]		## This will give you the sample size and the number of patients recruited and expected events that accumulate with the given recruitement and follow-up estiamtes

