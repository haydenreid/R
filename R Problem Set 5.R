setwd("C:/Users/Hayde/OneDrive - The University of Texas at Dallas/All OneDrive Files/UT Dallas/Fall 2018/BUAN 6356/BoxFiles")

library(data.table)
library(dplyr)
library(partykit)
library(plm)
library(party)
library(DBI)
library(RSQLite)
library(tidyverse)
library(ggplot2)
library(dplyr)


################################################################################
################################# Question 5.1 #################################

# Q5.1(a): I will describe the data generating process by variable:
#          z & w: z & w hold 500 normally distributed values. 
#          x: x takes all 500 normally distributed values, multiplies them all by 5, then adds 30. This makes the values
#          represent income, and for my random values they range from 32.88 to 68.88, creating a reasonable range for 
#          income. 
#          y: y is created using a linear combination of both z and w, which is then rounded to the nearest integer. 
#          these values range from 660 to 1500 in dt1, and represent a reasonable range for SAT scores. 
#          logically, the values are limited to be no lower than 200 and no higher than 1600, to represent real life SAT
#          scores. 
#          Finally, the values are put into a data table, giving an id to each row, along with an income value, an SAT
#          score, and a group number. 
#          The groups are divided based on income and SAT, using different linear combinations of z and w to skew each 
#          value higher or lower depending on the group. Group 2 has the highest income and SAT scores, followed by
#          group 1, and finally the lowest scores and income in group 1. 
# Q5.1(b): In the pooled OLS model, we can identify the pattern of SAT scores increasing with increased income across the groups,
#          because as we move from one group to the next there is a stark difference in income and SAT score. However, 
#          when the fixed effects model is used the groups are separated to identify patterns within the groups, and as such
#          there is many factors other than income that might influence each group's SAT score after controlling for income. In
#          this case, the income coefficient is negative, indicating that within each group there is a negative relationship 
#          between income and SAT scores (i.e. when income is increased, SAT score decreases) which contradicts the results
#          from the pooled model. 
#          when subsetting the data by groups and running models individually, we can see the same relationship as with the
#          fixed effects model: when income goes up SAT scores go down. Again, this can probably be explained by the groups 
#          having similar incomes and therefore other factors are more likely to influence SAT scores because income is relatively
#          the same among each group. 
# Q5.1(c): From the recursive partitioning model using income, we can understand the different income groups and thier SAT scores to predict
#          how a future student might do before taking the SAT based on income. We can also learn that  there is actually a 
#          negative effect of income on SAT score after a certain threshold, which looks to be income above ~$65k and below ~$73k. These students
#          have the highest SAT scores amongst all the groups in the model, and adding additional income from this group onwards has an associated
#          decrease in SAT scores. 
#          From the recursive partitioning model using only the groups, it can be identified that group 3 has the lowest SAT score, followed by 
#          group 1, and finally group 2 has the highest score. This would point towards the groups being predefined using some other factor that is
#          likely to have an effect on SAT score. This model actually contradicts the previous, because the groups also have higher incomes as 
#          SAT scores increase, whereas in the previous model increases in income did not have the same relationship. 
#          From the final model using both income and group, we can see that R decides the largest split to be in the groups, and even that group 3
#          sees the largest difference in SAT score from the other groups. After separating groups, we can see the different ranges of incomes within
#          each group, which has a further distribution of SAT scores. All groups, it seemed that as income went down, SAT scores went up, much like
#          in the models that were subsetted for each individual group. 
# Q5.1(d): Using the AIC values for the three different models (that is, just income, income + group, and just group), the best model is the 
#          model using both variables. 
# Q5.1(e): After plotting the data using kmeans, I can determine via the elbow plot that the optimal number of groups is 2. 
# Q5.1(f): Using a table to compare the groups from the original data definition and the kmeans groups, not only is the number of groups wrong
#          but also the distribution is pretty even among the table, meaning the groups do not match. 
#          Using hierarchical clustering, there are 4 generated groups instead of 2. Comparing the 4 groups to the original 3, it seems the 
#          hierarchical clustering model does a worse job of identifying the real clusters with an even distribution across all 4 clusters that are 
#          not in the same patter as the original groups. Both clustering approaches are not optimal in identifying the groups. 
# Q5.1(g): When using the kmeans clusters as groups, the models now show a positive relationship between income and SAT scores, which is contrary
#          to the original models with the predefined groups. Using hierarchical clusters as groups, there are varying relationships after subsetting
#          the data to the various clusters, but using the within groups model, each group maintains a positive relationship between income and SAT
#          score, which we know to be true. 
# Q5.1(h): After running the kmeans estimation using only income, the comparison between the kmeans clusters and the original groups are almost
#          identical. In addition, the same relationships are represented in each of the models when using the kmeans clusters as groups. 
# Q5.1(i): Scaling the variables beforehand gives equal weights to both sat and income, and provides the most accurate result compared to the real
#          groups. The table that compares the original groups and the kmeans clusters are only off by 4 data points, which is extremely accurate. 

n <- 500
set.seed(75080)

#NOTE: I had to change my tables from data.table 's to data.frame 's because my home computer was not merging the tables correctly.
#      The results appear to be the same, but I thought I should note it just in case. 

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.frame('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.frame('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.frame('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge.data.frame(dt1    ,dt2, all=TRUE)
dtable <- merge.data.frame(dtable ,dt3, all=TRUE)
dtable$group <- as.factor(dtable$group)
dtable$iid <- 1:1500

dtable <- as.data.table(dtable)

pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+group-1,data=dtable) #fixed effects
model1 <- lm(sat~income,data=dtable[group=='1'])
model2 <- lm(sat~income,data=dtable[group=='2'])
model3 <- lm(sat~income,data=dtable[group=='3'])

summary(pooled)
summary(within)
summary(model1)
summary(model2)
summary(model3)

ptable <- pdata.frame(dtable,index=c('group','iid'))
plm(sat~income,model='within',data=ptable)
lm(sat~income+group-1,data=dtable)
plm(sat~income,model='pooling',data=ptable)

#(c)
tree1 <- ctree(sat~income,data=dtable)
plot(tree1)
tree2 <- ctree(sat~group-1,data=dtable)
plot(tree2)
tree3 <- ctree(sat~income+group-1,data=dtable)
plot(tree3)

#(d)
glm1 <- glmtree(sat~income+group-1,data=dtable)
glm2 <- glmtree(sat~income,data=dtable)
glm3 <- glmtree(sat~group-1,data=dtable)
AIC(glm1)
AIC(glm2)
AIC(glm3)


#(e)

kmeans.wss <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}
eratio <- function(wss) {
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
wss <- dtable %>% select(sat,income) %>% kmeans.wss
plot.wss(wss)
eratio(wss)

model <- kmeans(dtable%>%select(sat,income),centers=2,nstart=10)
model$centers
groupsk <- model$cluster
groupsk
table(groupsk,dtable$group)
dtable$kmeans <- as.factor(groupsk)

#(f)
hclust.wss <- function(data,model=hclust(dist(data)),maxclu=10) {
  # Within sum of squares function for hierarchical clustering
  wss <- rep(NA,maxclu)
  for(i in 1:maxclu){
    gps <- cutree(model,i) # get groups for i clusters
    means <- data[,lapply(.SD,mean),by=gps] # get means
    demeaned <- data-means[gps,2:(ncol(data)+1)] # difference data from means
    wss[i] <- sum(demeaned^2) # sum squared distanaces
  }
  return(wss)
}

wss <- dtable %>% data.table %>% select(sat,income) %>% hclust.wss
plot.wss(wss)
eratio(wss)
dtable$hier <- cutree(hclust(dist(dtable %>% select(income,sat))),k=4)
dtable$hier <- as.factor(dtable$hier)
table(dtable$hier,dtable$group)

#(g)
pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+kmeans-1,data=dtable) #fixed effects
model1 <- lm(sat~income,data=dtable[kmeans=='1'])
model2 <- lm(sat~income,data=dtable[kmeans=='2'])

within2 <- lm(sat~income+hier-1,data=dtable) #fixed effects
model3 <- lm(sat~income,data=dtable[hier=='1'])
model4 <- lm(sat~income,data=dtable[hier=='2'])
model5 <- lm(sat~income,data=dtable[hier=='3'])
model6 <- lm(sat~income,data=dtable[hier=='4'])

summary(pooled)
summary(within)
summary(model1)
summary(model2)

summary(within2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)


#(h)
kmeans.wss <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}
eratio <- function(wss) {
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
wss <- dtable %>% select(income) %>% kmeans.wss
plot.wss(wss)
eratio(wss)

model <- kmeans(dtable%>%select(income),centers=3,nstart=10)
model$centers
groupsk <- model$cluster
groupsk
table(groupsk,dtable$group)
dtable$kmeans <- as.factor(groupsk)

pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+kmeans-1,data=dtable) #fixed effects
model1 <- lm(sat~income,data=dtable[kmeans=='1'])
model2 <- lm(sat~income,data=dtable[kmeans=='2'])
model3 <- lm(sat~income,data=dtable[kmeans=='3'])

summary(pooled)
summary(within)
summary(model1)
summary(model2)
summary(model3)

#(i)
kmeans.wss <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}
eratio <- function(wss) {
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}
plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}

wss <- dtable %>% select(scale(income),scale(sat)) %>% kmeans.wss
plot.wss(wss)
eratio(wss)

model <- kmeans(dtable%>%select(scale(income),scale(sat)),centers=3,nstart=10)
model$centers
groupsk <- model$cluster
groupsk
table(groupsk,dtable$group)
dtable$kmeans <- as.factor(groupsk)

pooled <- lm(sat~income,data=dtable)
within <- lm(sat~income+kmeans-1,data=dtable) #fixed effects
model1 <- lm(sat~income,data=dtable[kmeans=='1'])
model2 <- lm(sat~income,data=dtable[kmeans=='2'])
model3 <- lm(sat~income,data=dtable[kmeans=='3'])

summary(pooled)
summary(within)
summary(model1)
summary(model2)
summary(model3)











