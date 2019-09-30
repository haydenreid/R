#Problem Set 1 - Hayden Reid hjr160230
setwd("C:/Users/Hayden/OneDrive - The University of Texas at Dallas/All OneDrive Files/UT Dallas/Fall 2018/BUAN 6356/BoxFiles") #my personal working directory

library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage1 <- con %>% dbReadTable('wage1') %>% data.table
con %>% dbReadTable('wage1_labels')
con %>% dbDisconnect
# index variable.name  type format                  variable.label
# 1      0          wage float  %8.2g         average hourly earnings
# 2      1          educ  byte  %8.0g              years of education
# 3      2         exper  byte  %8.0g      years potential experience
# 4      3        tenure  byte  %8.0g     years with current employer
# 5      4      nonwhite  byte  %8.0g                  =1 if nonwhite
# 6      5        female  byte  %8.0g                    =1 if female
# 7      6       married  byte  %8.0g                   =1 if married
# 8      7        numdep  byte  %8.0g            number of dependents
# 9      8          smsa  byte  %8.0g              =1 if live in SMSA
# 10     9      northcen  byte  %8.0g =1 if live in north central U.S
# 11    10         south  byte  %8.0g   =1 if live in southern region
# 12    11          west  byte  %8.0g    =1 if live in western region
# 13    12      construc  byte  %8.0g  =1 if work in construc. indus.
# 14    13       ndurman  byte  %8.0g  =1 if in nondur. manuf. indus.
# 15    14      trcommpu  byte  %8.0g  =1 if in trans, commun, pub ut
# 16    15         trade  byte  %8.0g    =1 if in wholesale or retail
# 17    16      services  byte  %8.0g        =1 if in services indus.
# 18    17      profserv  byte  %8.0g     =1 if in prof. serv. indus.
# 19    18       profocc  byte  %8.0g    =1 if in profess. occupation
# 20    19       clerocc  byte  %8.0g    =1 if in clerical occupation
# 21    20       servocc  byte  %8.0g     =1 if in service occupation
# 22    21         lwage float  %9.0g                       log(wage)
# 23    22       expersq   int  %9.0g                         exper^2
# 24    23       tenursq   int  %9.0g                        tenure^2

#Note: calculations/work/code will be shown prior to the interpretation of the results

#Question 1.1 Solution

summary(wage1$educ) 
mean(wage1$wage)
cpi76 <- 56.9
cpi10 <- 218.056
cpiRatio <- cpi10/cpi76
mean(wage1$wage)*cpiRatio
table(wage1$female)

# Q1.1(i): Average years of education: 12.56, Lowest: 0 years, Highest: 18 years
# Q1.1(ii): Average hourly wage: ~$5.91/hr. This seems low.
# Q1.1(iii): CPI in 1976: 56.9, CPI in 2010: 218.056 
# Q1.1(iv): 1976 hourly wage converted to 2010 $'s: $22.64/hr. The hourly wage now seems reasonable. 
# Q1.1(v): there are 252 females and 274 males in the 'wage1' dataset. 

# End of Question 1.1

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
meap01 <- con %>% dbReadTable('meap01') %>% data.table
con %>% dbReadTable('meap01_labels')
con %>% dbDisconnect

# index variable.name  type format                                variable.label
# 1      0         dcode float  %9.0g                                 district code
# 2      1         bcode   int  %9.0g                                 building code
# 3      2         math4 float  %9.0g       % students satisfactory, 4th grade math
# 4      3         read4 float  %9.0g    % students satisfactory, 4th grade reading
# 5      4         lunch float  %9.0g % students eligible for free or reduced lunch
# 6      5        enroll   int  %9.0g                             school enrollment
# 7      6        expend float  %9.0g                             total spending, $
# 8      7         exppp float  %9.0g         expenditures per pupil: expend/enroll
# 9      8       lenroll float  %9.0g                                   log(enroll)
# 10     9       lexpend float  %9.0g                                   log(expend)
# 11    10        lexppp float  %9.0g                                    log(exppp)

#Question 1.2 Solution

summary(meap01$math4)
sum(meap01$math4==100)
mean(meap01$math4==100)
sum(meap01$math4==50)
mean(meap01$math4)
mean(meap01$read4)
cor(meap01$math4, meap01$read4)
mean(meap01$exppp)
sd(meap01$exppp)
(6000-5500)/5500
log(6000)-log(5500)

# Q1.2(i): Largest value: 100, Smallest value: 0. This makes sense given that schools can never have more
#          than 100% or less than 0% satisfactory scores in any subject. 
# Q1.2(ii): 38 schools have a perfect pass rate on the math test. This is 2.08% of schools in this dataset.
# Q1.2(iii): 17 schools have a pass rate of 50%. 
# Q1.2(iv): Mean pass rate for the math test is 70.9% ,mean pass rate for the reading test is 60.06%. 
#           The reading test is harder to pass. 
# Q1.2(v): Correlation between the math and reading pass rates is 84%. There is a strong relationship
#          between the pass rates of math and reading. 
# Q1.2(vi): Average expenditure per pupil: ~$5194.87. 
#           Standard deviation: ~$1091.89 per pupil. Yes, there is a wide variation in per pupil spending. 
# Q1.2(vii): School A spends 9.1% more than school B. This is very close to the difference in the natural 
#            logs of 8.7%. 

# End of Question 1.2

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
four01k = con %>% dbReadTable('401k') %>% data.table
con %>% dbReadTable('401K_labels')
con %>% dbDisconnect

# index variable.name  type format                  variable.label
# 1     0         prate float  %7.0g     participation rate, percent
# 2     1         mrate float  %7.0g            401k plan match rate
# 3     2       totpart float  %7.0g         total 401k participants
# 4     3        totelg float  %7.0g    total eligible for 401k plan
# 5     4           age  byte  %7.0g                age of 401k plan
# 6     5        totemp float  %7.0g  total number of firm employees
# 7     6          sole  byte  %7.0g = 1 if 401k is firm's sole plan
# 8     7       ltotemp float  %9.0g                   log of totemp

#Question 1.3 Solution

mean(four01k$prate)
mean(four01k$mrate)
model1    <- lm(prate~mrate, data=four01k)
summary(model1)
b0 <- 83.075
b1 <- 5.861
b0 + b1*(3.5)
predict(model1,data.frame(mrate=3.5))

# Q1.3(i): The average participation rate is ~87.36%. The average match rate is ~.73.
# Q1.3(ii): b0 (intercept) is ~83.08 and b1 is ~5.86. prate = 83.08 + 5.86(mrate). R^2 is .0747 
#           and the sample size is 1534 (degree of freedom + 2(# of predictor variables))
# Q1.3(iii): the intercept defines the participation rate as 83.08% when the match rate is 0. The 
#            coefficient explains that every match rate increase of 1 (or 100%) is associated with a 5.86% increase
#            in the participation rate. 
# Q1.3(iv): Predicted prate when mrate is 3.5: ~103.59%. This is an unreasonable prediction because
#           a prate of over 100% is not possible. The linear model is not bound by a parameter that limits
#           prate to 100, so it will continue to predict values despite logical boundaries. Additionally,
#           even if a match rate is very high, it is never certain that all eligible employees will 
#           partcipate. 
# Q1.3(v): 7.47% of the variation of the prate is explained by the mrate. In my opinion this is not a lot
#          given that the higher the R^2 value the higher the correlation variables, and 7.47 of 100 is 
#          relatively low. 

#End of Question 1.3

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
ceosal2 = con %>% dbReadTable('ceosal2') %>% data.table
con %>% dbReadTable('ceosal2_labels')
con %>% dbDisconnect

# index variable.name  type format                 variable.label
# 1      0        salary   int  %9.0g      1990 compensation, $1000s
# 2      1           age  byte  %9.0g                       in years
# 3      2       college  byte  %9.0g         =1 if attended college
# 4      3          grad  byte  %9.0g =1 if attended graduate school
# 5      4        comten  byte  %9.0g             years with company
# 6      5        ceoten  byte  %9.0g      years as ceo with company
# 7      6         sales float  %9.0g      1990 firm sales, millions
# 8      7       profits   int  %9.0g         1990 profits, millions
# 9      8        mktval float  %9.0g market value, end 1990, mills.
# 10     9       lsalary float  %9.0g                    log(salary)
# 11    10        lsales float  %9.0g                     log(sales)
# 12    11       lmktval float  %9.0g                    log(mktval)
# 13    12      comtensq   int  %9.0g                       comten^2
# 14    13      ceotensq   int  %9.0g                       ceoten^2
# 15    14      profmarg float  %9.0g          profits as % of sales

#Question 1.4 Solution

mean(ceosal2$salary)
mean(ceosal2$ceoten)
mean(ceosal2$comten)
sum(ceosal2$ceoten==0)
max(ceosal2$ceoten)
model2 <- lm(log(salary)~ceoten, data=ceosal2)
summary(model2)
.009724*100

# Q1.4(i): Average salary is $865,864.40 and average tenure is ~7.95 years as CEO 
#          and ~22.5 years with their company.
# Q1.4(ii): There are 5 CEOs in their first year as CEO. The longest tenure as CEO is 37 years. 
# Q1.4(iii): log(salary) = 6.51 + .97(ceoten) + u, sample size is 177 and R^2 is .013
#            Estimated salary increase after one more year as CEO is ~.97%

#End of Question 1.4

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
wage2 = con %>% dbReadTable('wage2') %>% data.table
con %>% dbReadTable('wage2_labels')
con %>% dbDisconnect

# index variable.name  type format                variable.label
# 1      0          wage   int  %9.0g              monthly earnings
# 2      1         hours  byte  %9.0g          average weekly hours
# 3      2            IQ   int  %9.0g                      IQ score
# 4      3           KWW  byte  %9.0g knowledge of world work score
# 5      4          educ  byte  %9.0g            years of education
# 6      5         exper  byte  %9.0g      years of work experience
# 7      6        tenure  byte  %9.0g   years with current employer
# 8      7           age  byte  %9.0g                  age in years
# 9      8       married  byte  %9.0g                 =1 if married
# 10     9         black  byte  %9.0g                   =1 if black
# 11    10         south  byte  %9.0g           =1 if live in south
# 12    11         urban  byte  %9.0g            =1 if live in SMSA
# 13    12          sibs  byte  %9.0g            number of siblings
# 14    13       brthord  byte  %9.0g                   birth order
# 15    14         meduc  byte  %9.0g            mother's education
# 16    15         feduc  byte  %9.0g            father's education
# 17    16         lwage float  %9.0g           natural log of wage

# Question 1.5 Solution

mean(wage2$wage)
mean(wage2$IQ)
sd(wage2$IQ)
model1 <- lm(wage~IQ, data=wage2)
summary(model1)
8.3*15
model1 <- lm(log(wage)~IQ, data=wage2)
summary(model1)
15*.88

#Q1.5(i): Average monthly salary is ~$957.95 and average IQ of the sample is ~101.28. Sample standard 
#         deviation for IQ is ~15.05
#Q1.5(ii): Estimate simple regression model: wage = 116.99 + 8.3(IQ), n = 935, R^2 = ~.096
#          For an increase in IQ of 15 there is a predicted increase of $124.50 in monthly earnings
#          IQ explains ~9.55% of the variation in monthly wage. I would not consider this most of the variation.
#Q1.5(iii): Estimate simple regression model: log(wage) = 5.89 + .88(IQ), n = 935, R^2 = ~.099
#           For an increase in IQ of 15 there is a predicted increase of 13.2% in monthly earnings. 

#End of Question 1.5

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
meap93 = con %>% dbReadTable('meap93') %>% data.table
con %>% dbReadTable('meap93_labels')
con %>% dbDisconnect

# index variable.name  type format                  variable.label
# 1      0       lnchprg float  %9.0g  perc of studs in sch lnch prog
# 2      1        enroll   int  %9.0g               school enrollment
# 3      2         staff float  %9.0g         staff per 1000 students
# 4      3        expend   int  %9.0g             expend. per stud, $
# 5      4        salary float  %9.0g          avg. teacher salary, $
# 6      5      benefits   int  %9.0g        avg. teacher benefits, $
# 7      6      droprate float  %9.0g       school dropout rate, perc
# 8      7      gradrate float  %9.0g    school graduation rate, perc
# 9      8        math10 float  %9.0g    perc studs passing MEAP math
# 10     9         sci11 float  %9.0g perc studs passing MEAP science
# 11    10       totcomp float  %9.0g               salary + benefits
# 12    11      ltotcomp float  %9.0g                    log(totcomp)
# 13    12       lexpend float  %9.0g                   log of expend
# 14    13       lenroll float  %9.0g                     log(enroll)
# 15    14        lstaff float  %9.0g                      log(staff)
# 16    15        bensal float  %9.0g                 benefits/salary
# 17    16       lsalary float  %9.0g                     log(salary)

# Question 1.6 Solution

model1 <- lm(math10~log(expend), data=meap93)
summary(model1)
11.16/10

#Q1.6(i): I think there would be diminishing returns on the effect that each additional dollar spent would have 
#         on pass rate. Initially schools lacking resources would have great effects, but once they have the 
#         tools needed for student success more money will likely not affect pass rates much at all. 
#Q1.6(ii): For every 1% increase in expend there is a b1 increase in math10. For a 10% increase in expend,
#          there would be a 10*b1 increse in math10. Since math10 is expressed as a percentage, math10 would 
#          increase by (10*b1)/100, or b1/10. 
#Q1.6(iii): Estimate equation: math10 = -69.34 + 11.16(log(expend)), n = 408, R^2 = .0297
#Q1.6(iv): An estimated 1.116% increase in math10 would result from a 10% incrase in expend. 
#Q1.6(v): Log models represent diminishing returns, meaning the initial increase in expend would likely
#         have a much more significant impact than later increases, thus slowing the increase in pass rates
#         as expenditure increases. 

#End of Question 1.6

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
hprice1 = con %>% dbReadTable('hprice1') %>% data.table
con %>% dbReadTable('hprice1_labels')
con %>% dbDisconnect

# index variable.name  type format               variable.label
# 1      0         price float  %9.0g          house price, $1000s
# 2      1        assess float  %9.0g       assessed value, $1000s
# 3      2         bdrms  byte  %9.0g              number of bdrms
# 4      3       lotsize float  %9.0g   size of lot in square feet
# 5      4         sqrft   int  %9.0g size of house in square feet
# 6      5      colonial  byte  %9.0g =1 if home is colonial style
# 7      6        lprice float  %9.0g                   log(price)
# 8      7       lassess float  %9.0g                   log(assess
# 9      8      llotsize float  %9.0g                 log(lotsize)
# 10     9        lsqrft float  %9.0g                   log(sqrft)

# Question 1.7 Solution

model1 <- lm(price~sqrft+bdrms, data=hprice1)
summary(model1)
x <- .1284
y <- 15.1982
y*1000
(y + x*140)*1000
(-19.315 + x*2438 + y*4)*1000
300000-354517
predict(model1,data.frame(sqrft=2438,bdrms=4))

#Q1.7(i): price = -19.315 + .1284(sqrft) + 15.1982(bdrms), R^2 = 0.6319, N = 88
#Q1.7(ii): $15,198.20 estimated increase in price with one more bedroom. 
#Q1.7(iii): $33,174.20 estimated increase in price with a bedroom with 140 square feet. This means that if house
#           A and house B have are the same square feet, but house B has 1 more bedroom, it will cost ~$15k  
#           more for house B. Likewise if house B has an extra bedroom and that bedroom takes up 140 more square
#           feet than house A, house B will cost ~$33k more than house A. 
#Q1.7(iv): ~63.19% of the variation in price is explained by square feet and number of bedrooms. 
#Q1.7(v): The predicted selling price for the house is $354,517
#Q1.7(vi): Given a sale price of $300k, the residual value is ~$54k. This model suggests that the buyer
#          underpaid for the home. However, It can be assumed that since the squarefeet and the number of
#          bedrooms only accounts for 66% of the variation in price that other factors contributed to the 
#          price of the home that were not accounted for in this model. 

#End of Question 1.7

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
ceosal2 = con %>% dbReadTable('ceosal2') %>% data.table
con %>% dbReadTable('ceosal2_labels')
con %>% dbDisconnect

# index variable.name  type format                 variable.label
# 1      0        salary   int  %9.0g      1990 compensation, $1000s
# 2      1           age  byte  %9.0g                       in years
# 3      2       college  byte  %9.0g         =1 if attended college
# 4      3          grad  byte  %9.0g =1 if attended graduate school
# 5      4        comten  byte  %9.0g             years with company
# 6      5        ceoten  byte  %9.0g      years as ceo with company
# 7      6         sales float  %9.0g      1990 firm sales, millions
# 8      7       profits   int  %9.0g         1990 profits, millions
# 9      8        mktval float  %9.0g market value, end 1990, mills.
# 10     9       lsalary float  %9.0g                    log(salary)
# 11    10        lsales float  %9.0g                     log(sales)
# 12    11       lmktval float  %9.0g                    log(mktval)
# 13    12      comtensq   int  %9.0g                       comten^2
# 14    13      ceotensq   int  %9.0g                       ceoten^2
# 15    14      profmarg float  %9.0g          profits as % of sales

#Qustion 1.8 Solution

model1 <- lm(log(salary)~log(sales)+log(mktval), data=ceosal2)
summary(model1)
model1
model1 <- lm(log(salary)~log(sales)+log(mktval)+profits, data=ceosal2)
summary(model1)
model1
model1 <- lm(log(salary)~log(sales)+log(mktval)+profits+ceoten, data=ceosal2)
summary(model1)
model1
.0117*100
cor(ceosal2$profits,log(ceosal2$mktval))

#Q1.8(i): log(salary) = b0 + b1(log(sales)) + b2(log(mktval)) + u
#         log(salary) = 4.62 + .1621(log(sales)) + .1067(log(mktval)), R^2 = .2991, N = 177
#Q1.8(ii): log(salary) = 4.69 + .1614(log(sales)) + .0975(log(mktval)) + .000(profits), R^2 = .2993, N = 177
#          profits cannot be included in logarithmic form because profits could be negative.
#          These variables explain about 30% of the variation in salary, so I would not say
#          they explain most of the variation. 
#Q1.8(iii): log(salary) = 4.56 + .1622(log(sales)) + .1018(log(mktval)) + .000(profits) + .0117(ceoten), 
#           R^2 = .3183, N = 177
#           Holding other factors fixed, the estimated percentage return for one more year as CEO is 1.17%. 
#Q1.8(iv): The sample correlation coefficient between log(mktval) and profits is ~.78. These are highly
#          correlated. This suggests that the difference in impact between the two variables is difficult
#          to estimate.

#End of Question 1.8

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
attend = con %>% dbReadTable('attend') %>% data.table
con %>% dbReadTable('attend_labels')
con %>% dbDisconnect

# index variable.name  type format               variable.label
# 1      0        attend  byte  %8.0g   classes attended out of 32
# 2      1       termGPA float  %9.0g                 GPA for term
# 3      2        priGPA float  %9.0g cumulative GPA prior to term
# 4      3           ACT  byte  %8.0g                    ACT score
# 5      4         final  byte  %8.0g             final exam score
# 6      5       atndrte float  %9.0g     percent classes attended
# 7      6         hwrte float  %9.0g   percent homework turned in
# 8      7         frosh  byte  %8.0g               =1 if freshman
# 9      8          soph  byte  %8.0g              =1 if sophomore
# 10     9        missed  byte  %9.0g     number of classes missed
# 11    10       stndfnl float  %9.0g            (final - mean)/sd

#Question 1.9 Solution

min(attend$atndrte)
max(attend$atndrte)
mean(attend$atndrte)
min(attend$priGPA)
max(attend$priGPA)
mean(attend$priGPA)
model1 <- lm(atndrte~priGPA+ACT, data=attend)
x <- 17.261
y <- -1.717
75.7 + (x*3.65) + (y*20)
sum(as.numeric(attend$priGPA==3.65,attend$ACT==20))
filter(attend, priGPA == 3.65 & ACT == 20)
A <- 75.7 + (x*3.1) + (y*21)
B <- 75.7 + (x*2.1) + (y*26)
A-B

#Q1.9(i): Attendance Rate: min = 6.25%, max = 100%, average = 81.71%
#         Cumulative GPA prior to term: min = 0.857, max = 3.93, average = 2.59
#         ACT Score: min = 13, max= 32, average = 22.51
#Q1.9(ii): Estimated model: atndrte = 75.7 + 17.261priGPA - 1.717ACT, R^2 = .2906, N = 680
#          The intercept reprsents the predicted attendance rate for a student with a prior GPA of 0 and
#          an ACT score of 0. Realistically this is not useful because it is highly unlikely for a student 
#          to have a 0 in each category. 
#Q1.9(iii): The slope coefficient for priGPA is unsurprising. An increase in GPA of 1 point has an associated
#           increase in attendance rate of 17.26%, which is reasonable. However, the slope coefficient for 
#           ACT score is surprising because it is negative. The coefficeint explains that an incrase in ACT 
#           score of 1 results in a decrease of attendance of 1.72%. Meaning relatively "smarter" students
#           would have a lower attendance rate. 
#Q1.9(iv): A student with a prior GPA of 3.65 and an ACT score of 20 has a predicted attendance rate of 104.36%.
#          This doesn't make logical sense because a student cannot attend more than 100% of thier classes.
#          There is one student with a prior GPA of 3.65 and an ACT score of 20, however their attendance 
#          rate is 87.5%. 
#Q1.9(v): The difference in predicted attendance rates between student A and student B is ~25.85%.

#End of Question 1.9

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
HTV = con %>% dbReadTable('HTV') %>% data.table
con %>% dbReadTable('HTV_labels')
con %>% dbDisconnect

# index variable.name  type format                  variable.label
# 1      0          wage float  %9.0g               hourly wage, 1991
# 2      1          abil float  %9.0g abil. measure, not standardized
# 3      2          educ  byte  %9.0g highest grade completed by 1991
# 4      3            ne  byte  %9.0g        =1 if in northeast, 1991
# 5      4            nc  byte  %9.0g        =1 if in nrthcntrl, 1991
# 6      5          west  byte  %9.0g             =1 if in west, 1991
# 7      6         south  byte  %9.0g            =1 if in south, 1991
# 8      7         exper  byte  %9.0g            potential experience
# 9      8      motheduc  byte  %9.0g           highest grade, mother
# 10     9      fatheduc  byte  %9.0g           highest grade, father
# 11    10      brkhme14  byte  %9.0g       =1 if broken home, age 14
# 12    11          sibs  byte  %9.0g              number of siblings
# 13    12         urban  byte  %9.0g       =1 if in urban area, 1991
# 14    13          ne18  byte  %9.0g             =1 if in NE, age 18
# 15    14          nc18  byte  %9.0g             =1 if in NC, age 18
# 16    15       south18  byte  %9.0g          =1 if in south, age 18
# 17    16        west18  byte  %9.0g           =1 if in west, age 18
# 18    17       urban18  byte  %9.0g     =1 if in urban area, age 18
# 19    18        tuit17 float  %9.0g         college tuition, age 17
# 20    19        tuit18 float  %9.0g         college tuition, age 18
# 21    20         lwage float  %9.0g                       log(wage)
# 22    21       expersq   int  %9.0g                         exper^2
# 23    22         ctuit float  %9.0g                 tuit18 - tuit17

#Question 1.10 Solution

range(HTV$educ)
mean(HTV$educ==12)
mean(HTV$educ)
mean(HTV$motheduc)
mean(HTV$fatheduc)
model1 <- lm(educ~motheduc+fatheduc, data=HTV)
model1
summary(model1)
model1 <- lm(educ~motheduc+fatheduc+abil, data=HTV)
model1
summary(model1)
abil2 <- (HTV$abil)^2
model1 <- lm(educ~motheduc+fatheduc+abil+abil2, data=HTV)
model1
summary(model1)
(-.4)/(2*.102)
mean(HTV$abil < -1.961)
8.24+(.19*12.18)+(.11*12.45)
plotbasic<-ggplot(HTV,aes(x=abil))+geom_line(aes(y=predict(model1,color='blue')))
plotbasic
basicqplot <- function(x) 11.9 + .4*x + .051*x^2
curve(basicqplot, xlab = "Ability", ylab = "Education")
ggplot(HTV,aes(x=abil,y=educ))+geom_point()+geom_line(aes(y=predict(model1)),color="red",size=1)
model0 <- lm(educ~abil+I(abil^2), data=HTV)                                                      
ggplot(HTV,aes(x=abil,y=educ))+geom_point()+geom_line(aes(y=predict(model0)),color="red",size=1)
ggplot(HTV,aes(x=abil,y=educ))+geom_point()

#Q1.10(i): The highest grade completed ranges from 6 to 20 in this dataset, yielding a range of 14. 
#          ~41.16% of men in the dataset  have a 12th grade education.
#          On average, men in the dataset have an education grade level of 13
#          while mothers education averages 12.18 and fathers education level averages 12.45. On average,
#          men in the dataset have a higher level of education than both thier parents. 
#Q1.10(ii): Estimated regression model: educ = 6.96 + 0.3motheduc + .19fatheduc, R^2 = 0.2493, N = 1230
#           ~25% of education level sample variation is explained by parents eduction under this model. 
#           For every 1 year increase in education for the mother, there is an associated .30 increase
#           in a 1991 working man's education level. 
#Q1.10(iii): New model: educ = 8.45 + .19motheduc + .11fatheduc + 0.5abil, R^2 = 0.4275, N = 1230
#            In the model in part ii, education level of the parents explained ~25% of the variation in 
#            education level. Under the new model with ability added, education level of the parents and the 
#            working man's ability explain ~43% of the variation in education level. This large of a difference
#            suggests that cognitive ability does explain a significant amount of the variation in education
#            level.
#Q1.10(iv): New model: educ = 8.24 + .19motheduc + .11fatheduc + .4abil + .051abil^2, R^2 = 0.4444, N = 1230
#           Using the equation f(x) = .4(x) + .051(x)^2 where educ is f(x) and x is abil, 
#           I can find the value that minimizes f(x) by first finding:
#           f'(x) = .4 + .102x and solving for the intercept. x = (-.4)/(2*.102)
#           yielding abil* = -1.961. The second derivative is f''(x) = .102, which is positive. 
#           Only 6.67% of men have a cognitive ability value of less than -1.961. This means that ~93% of men
#           in the dataset fit into the estimated model. 
#Q1.10(v): Given the average values for motheduc and fatheduc, the equation is
#          educ = 8.24+(.19*12.18)+(.11*12.45)+.4abil+.051abil^2 or educ = 11.9 + .4abil + .051abil^2
#          Above I have graphed the original model including all variables as well as the quadratic function 
#          defining the relationship between ability and education. 





