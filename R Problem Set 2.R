# Problem Set 2
# Hayden Reid hjr160230

setwd("C:/Users/Hayde/OneDrive - The University of Texas at Dallas/All OneDrive Files/UT Dallas/Fall 2018/BUAN 6356/BoxFiles")

rm(list=ls())

library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)

################################################################################
################################# Question 2.1 #################################


# Q2.1(i): For every 1% increase in campaign expend from candidate A, there will be an increase of b1/100 (percentage) in voteA
# Q2.1(ii): H0: b1 + b2 = 0 , expenditure increases from candidate A results in a voteA increase while expend increases
#           by candidate B results in a decrease in voteA. If these two coefficients are the same, voteA will have no net change. 
# Q2.1(iii): Estimate equation: voteA = 45.088 + 6.081(log(expendA)) - 6.616(log(expendB)) + 0.152(prtystrA)
#            R^2 = 0.7925, N = 173
#            Both expenditures by A and B affect the outcome. A 1% increase in expediture by A increases voteA by .061%, while a 
#            1% increase in expenditure by B decreases voteA by .066%. These values can be used to test the hypothesis from part ii.
# Q2.1(iv): The t statistic for the null hypothesis is ~ -1. This value is less than 1.96, so we cannot reject the null. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
vote1 <- con %>% dbReadTable('VOTE1') %>% data.table
con %>% dbReadTable('VOTE1_labels')
con %>% dbDisconnect

model1 <- lm(voteA~log(expendA)+log(expendB)+prtystrA, data=vote1)
summary(model1)
model2 <- lm(voteA~log(expendA)+I(log(expendB)-log(expendA))+prtystrA, data=vote1)
summary(model2)



################################################################################
################################# Question 2.2 #################################


# Q2.2(i): H0: b5 = 0
#          the model estimates b5 = -.0033. This means for a decrease in rank (i.e. moving up in rank) by 25, there is an estimated 
#          8.25% increase in median salary. Additonally, the abosolute t value for rank under this model is 9.54, which falls outside
#          1.96, meaning rank is significant. Therefore H0 is rejected. 
# Q2.2(ii): The t values in the model determine the individual significance for each variable. For GPA, the t value is 2.749,
#           which is greater than 1.96. GPA is independently significant on the 5% interval. 
#           For LSAT, the t value is 1.17, which is smaller than 1.96, implying that LSAT is not independently significant. 
#           Using an anova test for a model with LSAT and GPA and one without yields a high F value, which means the two values
#           are jointly significant. 
# Q2.2(iii): Adding clsize and faculty to the model only increase R^2 by .0023, meaning they do not explain much variation in salary.
#            By adding these two variables to the model, R does not rate these two variables significant at any level. Finally, 
#            the t values for both variables are lesser than 1.96. 
# Q2.2(iv): Other factors that might influence the rank of the law school could be number of publications produced by the faculty, 
#           court cases won by faculty or graduated students, or % of students that get law related jobs after graduation.

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
law85 <- con %>% dbReadTable('LAWSCH85') %>% data.table
con %>% dbReadTable('LAWSCH85_labels')
con %>% dbDisconnect

model1 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, data=law85)
model1
summary(model1)
model1 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, data=law85[!is.na(law85$LSAT)&!is.na(law85$GPA)&!is.na(law85$libvol)&!is.na(law85$cost)&!is.na(law85$rank)])
model0 <- lm(log(salary)~log(libvol)+log(cost)+rank, data=law85[!is.na(law85$LSAT)&!is.na(law85$GPA)&!is.na(law85$libvol)&!is.na(law85$cost)&!is.na(law85$rank)])
anova(model0,model1)
model2 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank+clsize+faculty, data=law85)
model2
summary(model2)


################################################################################
################################# Question 2.3 #################################


# Q2.3(i): Estimated model: log(price) = 4.766 + .00038(sqrft) + .02888(bdrms)
#          Given the estimated coefficiencts produced by the model, theta is estimated as .086 or 8.6%. This means an additional bedroom
#          that is 150 square feet increases price by 8.6%. 
# Q2.3(ii): Note: I will treat theta = o1 for the equations
#           b2 = o1 - 150b1
#           log(price) = b0 + b1(sqrft - 150bdrms) + o1bdrms
#           estimated model: log(price) = 4.766 + .00038(sqrft - 150bdrms) + .0858bdrms
# Q2.3(iii): standard error for o1 = .02677
#            to construct a confidence interval I can take the estimate coefficient of o1 +/- 2 * the std error
#            condfidence interval for o1 is from 3.2% to 13.9% (I can also use the confint command)

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
hprice1 <- con %>% dbReadTable('HPRICE1') %>% data.table
con %>% dbReadTable('HPRICE1_labels')
con %>% dbDisconnect

model1 <- lm(log(price)~sqrft+bdrms, data=hprice1)
summary(model1)
150*.0003794 + .0288845
z <- hprice1$sqrft - (150*hprice1$bdrms)
model2 <- lm(log(price)~z+bdrms, data=hprice1)
model2
summary(model2)
.0858 + 1.96*.02677
.0858 - 1.96*.02677
confint(model2, 'bdrms', level=0.95)


################################################################################
################################# Question 2.4 #################################


# Q2.4(i): H0: b2 = b3
#          log(wage) = 5.50 + .075educ + .015exper + .013tenure
# Q2.4(ii): H1: b2 =/= b3
#           o1 = b2-b3
#           log(wage) = b0 + b1educ + b3(exper + tenure) + o1exper
#           in this model, the coefficient for exper is the estimated o1, about .002. given that this is extremely close to 
#           0, and is not statistically different enough to reject the null at the 5% level. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage2 <- con %>% dbReadTable('WAGE2') %>% data.table
con %>% dbReadTable('WAGE2_labels')
con %>% dbDisconnect

model1 <- lm(log(wage)~educ+exper+tenure, data=wage2)
summary(model1)
z <- wage2$exper + wage2$tenure
model2 <- lm(log(wage)~educ+z+exper, data=wage2)
summary(model2)


################################################################################
################################# Question 2.5 #################################


# Q2.5(i): there are 2017 single person households. 
# Q2.5(ii): nettfa = -43.04 + .799inc + .843age, N = 2017, R^2 = .1193
#          For every unit increase in family income, there is a .799 units ($799) increase  in net financial wealth. For every unit increase in age, 
#          there is a .843 units ($843) increase in net financial wealth. It is unsurprising that an increase in family income would result in an 
#          increase in net financial assets. It is also unsurprising that an increase in age by one year results in an increase in financial assets
#          of $843. It is likely that more tenured employees earn higher salaries at their jobs.
# Q2.5(iii): The intercept in this model explains that a person age 0 (a baby) with 0 income has net assets of -$43k. Given that this model is of 1
#            person households, this does not make very much sense. However, it may explain that having a child decreases net assets of a family in some way.
# Q2.5(iv): The p value for b2 in this model is ~.0000, which is less than .01 level of significance, so the null hypothesis is rejected at the 1% level. 
# Q2.5(v): The estimated coefficient for the new model is not much different from the old model. This is likely due to the impact income has on net financial
#          assets being almost the same when paired with age and when not paired with age, meaning together the variables have the same impact as when income
#          is by itself. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
ksubs <- con %>% dbReadTable('401KSUBS') %>% data.table
con %>% dbReadTable('401KSUBS_labels')
con %>% dbDisconnect

sum(ksubs$fsize==1)
f1 <- subset(ksubs, ksubs$fsize==1)
model1 <- lm(nettfa~inc+age, data=f1)
model1
summary(model1)
model2 <- lm(nettfa~inc, data=f1)
model2
summary(model2)


################################################################################
################################# Question 2.6 #################################


# Q2.6(i): Prior to running the model, I would expect the sign of b1 to be positive if the incinerator depresses housing prices.
#          This would mean the farther from the incinerator the higher the prices would be. 
#          log(price) = 8.047 + 0.365log(dist) , N = 182, R^2 = .1803 
#          The model suggest that a home 1% farther from the incinerator is associated with a .365% increase in price. 
# Q2.6(ii): log(price) = 7.59 + .055log(dist) - .039log(intst) + .319log(area) + .077log(land) + .043rooms + .167baths - .004age, N = 182, R^2 = .7475
#           The new model has a much smaller coefficient for the distance from the incinerator, explaining that given the sum of 
#           these other variables affecting house prices the distance from the incinerator is not statistically significant. 
# Q2.6(iii): log(price) = -3.318 + .185log(dist) + 2.073log(intst) - .119(log(intst))^2 +.359log(area) + .091log(land) + .038rooms + .15baths - .003age, 
#            N = 182, R^2 = .7775
#            With (log(intst))^2 added, log(dist) once again becomes statistically significant. In the previous linear functional form, the model did not capture
#            the non-linear relationship between log(dist) and intst, but the new  non-linear equation did. 
# Q2.6(iv): log(price) = -5.92 + .87log(dist) - .036(log(dist))^2 + 1.934log(intst) - .111(log(intst))^2 +.355log(area) + .088log(land) + .038rooms + .151baths - .003age, 
#           N = 182, R^2 = .7777
#           When added to the model, the square of log(dist) is insignificant at all levels. Also, the significance of log(dist) is once again reduced and statistically insignificant. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
kielmc <- con %>% dbReadTable('KIELMC') %>% data.table
con %>% dbReadTable('KIELMC_labels')
con %>% dbDisconnect

sub1 <- subset(kielmc, kielmc$year==1981)
model1 <- lm(log(price)~log(dist),data=sub1)
model1
summary(model1)
model2 <- lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age,data=sub1)
model2
summary(model2)
model3 <- lm(log(price)~log(dist)+log(intst)+I((log(intst))^2)+log(area)+log(land)+rooms+baths+age,data=sub1)
model3
summary(model3)
model4 <- lm(log(price)~log(dist)+I((log(dist)^2))+log(intst)+I((log(intst))^2)+log(area)+log(land)+rooms+baths+age,data=sub1)
model4
summary(model4)


################################################################################
################################# Question 2.7 #################################


# Q2.7(i): log(wage) = .126 + .091educ + .041exper - .001exper^2, N = 526, R^2 = .3003
# Q2.7(ii): In this model, exper^2 is significant at the 1% level. 
# Q2.7(iii): The approximate return to the fifth year of experience is ~3.39%, and ~1.25% for the 20th year. 
# Q2.7(iv): At about 28.8 years exper lowers predicted log(wage). There are 121 people with at least 29 years of experience in this dataset. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage1 <- con %>% dbReadTable('WAGE1') %>% data.table
con %>% dbReadTable('WAGE1_labels')
con %>% dbDisconnect

model1 <- lm(log(wage)~educ+exper+I(exper^2),data=wage1)
model1
summary(model1)
100*(0.0409731+(2*5*-0.0007121))
100*(0.0409731+(2*20*-0.0007121))
100*(0.0409731+(2*28.8*-0.0007121))
sum(wage1$exper>=29)


################################################################################
################################# Question 2.8 #################################


# Q2.8(i): Holding exper equal, the equation can be algebraically rearranged
#          log(wage) = b1educ + b3educ*exper
#          log(wage) = b1educ + educ(b3*exper)
#          log(wage) = educ(b1 + b3*exper)
#          log(wage)/educ = b1 + b3*exper
# Q2.8(ii): H0: b3 = 0, HA: b3 =/= 0
# Q2.8(iii): log(wage) = 5.949 + .044educ - .021exper + .003educ*exper, N = 935, R^2 = .1394
#            The coefficient for educ*exper is .003 and has a p value of .0365, which is less than .05 and is therefore
#            statistically significant at the 5% level. 
# Q2.8(iv): log(wage) = 5.949 + .076educ - .021exper + .003(educ(exper-10)), N = 935, R^2 = .1349
#           The 5% confidence interval for o1 (theta 1) is .063 to .089 

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage2 <- con %>% dbReadTable('WAGE2') %>% data.table
con %>% dbReadTable('WAGE2_labels')
con %>% dbDisconnect

model1 <- lm(log(wage)~educ+exper+I(educ*exper), data = wage2)
model1
summary(model1)
model2 <- lm(log(wage)~educ+exper+I(educ*(exper-10)), data = wage2)
model2
summary(model2)
confint(model2,'educ',level=0.95)


################################################################################
################################# Question 2.9 #################################


# Q2.9(i): Estimated model: sat = 997.981 + 19.814hsize - 2.131hsize^2, N = 4137, R^2 = .00765
#          The quadratic term is statistically significant at the .1% level given that the p value is less than .001. 
# Q2.9(ii): if f(x) = b1hsize + b2hsize^2, then f'(x) set = 0 will identify the maximum SAT point on the parabola. 
#           0 = b1 + 2*b2*hsize, using the estimated values from the model the hsize where sat is maximum is 4.65 
#           (or 465 students in the class). This means that a class of 465 students will yield the highest SAT compared
#           to other class sizes. 
# Q2.9(iii): The model only captures seniors with SAT data, so students who have yet to take the test or 
#            are still waiting on their scores to be reported are not captured in this dataset. 
# Q2.9(iv): log(sat) = 6.896 + .02hsize - .002hsize^2, N = 4137, R^2 = .00777
#           using the same method but with different b1 and b2 values the class size at which SAT score is maximized
#           Under the new model, the estimated optimal class size is 469, which is very close to the previous model. 

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
gpa2 <- con %>% dbReadTable('GPA2') %>% data.table
con %>% dbReadTable('GPA2_labels')
con %>% dbDisconnect

model1 <- lm(sat~hsize+I(hsize^2),data=gpa2)
model1
summary(model1)
-19.81/(2*-2.131)
model2 <- lm(log(sat)~hsize+I(hsize^2),data=gpa2)
model2
summary(model2)
0.019603/(2*0.002087 )


################################################################################
################################# Question 2.10 ################################


# Q2.10(i): Estimated model: log(price) = -1.297 + .168log(lotsize) + .7log(sqrft) + .037bdrms, N = 88, R^2 = .643
# Q2.10(ii): plugging in the given values yields log(price) = 5.993 and using the exp function yields the dollar price for 
#            the house of $400,615
# Q2.10(iii): price = -21.77 + .002lotsize + .123sqrft + 13.852bdrms
#             In the log-linear model, the R^2 value is .643, meaning the log attributes of the house explain ~64% of the variation in price
#             In the linear model, the R^2 value is .6724, meaning the attributes of the house explain ~67% of the variation in price.
#             This points toward the linear model being better to explain the variation in price. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
hprice1 <- con %>% dbReadTable('HPRICE1') %>% data.table
con %>% dbReadTable('HPRICE1_labels')
con %>% dbDisconnect

model1 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=hprice1)
model1
summary(model1)
-1.29704+(0.16797*log(20000))+0.70023*(log(2500))+ 0.03696*4
model2 <- lm(price~lotsize+sqrft+bdrms, data=hprice1)
model2
summary(model2)


