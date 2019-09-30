# Problem Set 4
# Hayden Reid hjr160230

setwd("C:/Users/Hayde/OneDrive - The University of Texas at Dallas/All OneDrive Files/UT Dallas/Fall 2018/BUAN 6356/BoxFiles")

rm(list=ls())

library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(sandwich) 
library(lmtest) 
library(tseries) 
library(broom)
library(forecast)
library(TSA)
library(vars)
library(plm)
library(margins)
library(lmtest)


################################################################################
################################# Question 4.1 #################################

# Q4.1(i): The best BIC value I could find for a model estimating housing price is 957.44 with the model
#          price = b0 + b1lotsize:colonial + b2bdrms:colonial + b3bdrms:sqrft + b4I(bdrms^2) + b5bdrms:lotsize + b6I(lotsize^2)
#          even though the log model's BIC is smaller, the R^2 value for the non-log model explains more variation in the housing price.
#          The best AIC value I could find for a model estimating housing price is 930.19 with the model
#          price = b0 +b1bdrms:sqrft + b2bdrms:colonial + b3I(bdrms^2)+ b4bdrms:lotsize + b5I(lotsize^2)
#          much like with the BIC model the log model returned a smaller AIC than the non-log model, however the R^2 for the non log model
#          is better, so between the AIC and the R^2 the non log model is a better fit. 

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
hprice <- con %>% dbReadTable('HPRICE1') %>% data.table
con %>% dbReadTable('HPRICE1_labels')
con %>% dbDisconnect

summary(hprice)
cor(hprice)

model1 <- lm(price~bdrms+lotsize+sqrft+colonial,data=hprice)
BIC(model1)

BIC(step(lm(price~(bdrms+lotsize+sqrft+colonial)^2+bdrms+lotsize+sqrft+colonial+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),data=hprice)))
summary(lm(price~lotsize:colonial+bdrms:colonial+bdrms:sqrft+I(bdrms^2)+bdrms:lotsize+I(lotsize^2),data=hprice))

BIC(step(lm(lprice~(bdrms+llotsize+lsqrft+colonial)^2+bdrms+llotsize+lsqrft+colonial+I(bdrms^2)+I(llotsize^2)+I(lsqrft^2),data=hprice)))
summary(lm(lprice~I(llotsize^2)+I(lsqrft^2)+colonial+bdrms:lsqrft+I(bdrms^2)+bdrms:lotsize,data=hprice))

AIC(step(lm(price~(bdrms+lotsize+sqrft+colonial)^2+bdrms+lotsize+sqrft+colonial+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),data=hprice)))
summary(lm(price~bdrms:sqrft+bdrms:colonial+I(bdrms^2)+bdrms:lotsize+I(lotsize^2),data=hprice))

AIC(step(lm(lprice~(bdrms+llotsize+lsqrft+colonial)^2+bdrms+llotsize+lsqrft+colonial+I(bdrms^2)+I(llotsize^2)+I(lsqrft^2),data=hprice)))
summary(lm(price~I(llotsize^2)+I(lsqrft^2)+colonial+bdrms:lsqrft+I(bdrms^2)+bdrms:llotsize,data=hprice))


################################################################################
################################# Question 4.2 #################################


# Q4.2(i): The best BIC for GPA is 6724.251 with the model
#          colgpa = b0 + b1tothrs:hsrank + b2tothrs:athlete + b3athlete:hsrank + b4althlete:verbmath + b5tothrs:hsize + b6tothrs:hsperc + b7I(sat^2) + b8hsizesq + b9sat:hsperc + b10sat:tothrs
#          b11I(hsrank^2) + b12hsize:hsrank + b13white + b14hsrank:hsperc + b15female
#          The best AIC for GPA is 6572.39 with the model
#          colgpa = b0 + b1tothrs:hsrank + b2tothrs:athlete + b3athlete:hsrank + b4athlete:verbmath + b5tothrs:hsize + b6tothrs:hsperc + b7I(sat^2) + b8hsizesq + b9sat:hsperc + b10sat:tothrs 
#          + b11I(hsrank^2) + b12hsize:hsrank + b13white + b14hsrank:hsperc + b15female


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
gpa <- con %>% dbReadTable('GPA2') %>% data.table
con %>% dbReadTable('GPA2_labels')
con %>% dbDisconnect

BIC(step(lm(colgpa~sat+tothrs+athlete+verbmath+hsize+hsrank+hsperc+female+white+(sat+tothrs+athlete+verbmath+hsize+hsrank+hsperc)^2+I(sat^2)+I(tothrs^2)+I(verbmath^2)+hsizesq+I(hsrank^2),data=gpa)))
summary(lm(colgpa~tothrs:hsrank + tothrs:athlete + athlete:hsrank + athlete:verbmath + tothrs:hsize + tothrs:hsperc + I(sat^2) + hsizesq + sat:hsperc + sat:tothrs + I(hsrank^2) + hsize:hsrank + white + hsrank:hsperc + female,data=gpa))

AIC(step(lm(colgpa~sat+tothrs+athlete+verbmath+hsize+hsrank+hsperc+female+white+(sat+tothrs+athlete+verbmath+hsize+hsrank+hsperc)^2+I(sat^2)+I(tothrs^2)+I(verbmath^2)+hsizesq+I(hsrank^2),data=gpa)))
summary(lm(colgpa~tothrs:hsrank+tothrs:athlete+athlete:hsrank+athlete:verbmath+tothrs:hsize+tothrs:hsperc+I(sat^2)+hsizesq+sat:hsperc+sat:tothrs+I(hsrank^2)+hsize:hsrank+white+hsrank:hsperc+female,data=gpa))

################################################################################
################################# Question 4.3 #################################

# Q4.3(i): The best BIC I could find was 781.26 for the model 
#          lsalary = b0 + b1nl + b2scndbase + b3frstbase + b4so + b5percblck + b6blckph + b7hisppb + b8black + b9whtepw + b10hispan + b12perchisp + b13blckpb + b14hispph + b15triples
#          + b16games + b17rbisyr + b18runs + b19yrsallst + b20gamesyr + b21years + b22allstar
#          The best AIC I could find was 693.88 for the model
#          lsalary = b0 + b1 nl + b2scndbase + b3frstbase + b4so + b5percblck + b6blckph + b7hisppb + b8black + b9whtepw + b10hispan + b11perchisp + b12blckpb + b13hispph + b14triples
#          + b15games + b16rbisyr + b17runs + b18yrsallst + b19gamesyr + b20years + b21allstar

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
mlb <- con %>% dbReadTable('MLB1') %>% data.table
con %>% dbReadTable('MLB1_labels')
con %>% dbDisconnect

BIC(step(lm(lsalary~teamsal+nl+years+games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+frstbase+scndbase+shrtstop+thrdbase+catcher+yrsallst+hispan+black+whitepop+blackpop+hisppop+pcinc+gamesyr+hrunsyr+atbatsyr+allstar+slugavg+rbisyr+sbasesyr+runsyr+percwhte+percblck+perchisp+blckpb+hispph+whtepw+blckph+hisppb,data=mlb)))

AIC(step(lm(lsalary~teamsal+nl+years+games+atbats+runs+hits+doubles+triples+hruns+rbis+bavg+bb+so+sbases+fldperc+frstbase+scndbase+shrtstop+thrdbase+catcher+yrsallst+hispan+black+whitepop+blackpop+hisppop+pcinc+gamesyr+hrunsyr+atbatsyr+allstar+slugavg+rbisyr+sbasesyr+runsyr+percwhte+percblck+perchisp+blckpb+hispph+whtepw+blckph+hisppb,data=mlb)))


################################################################################
################################# Question 4.4 #################################


# Q4.4(i): Estimated Equation: log(rent) = -.569 + .262(y90) + .041log(pop) + .571log(avginc) + .005pctstu
#          In this model, rent has an associated 26% growth since 1980 and is statistically significant at all tested levels. 
#          The estimated coefficient on pctstu is .005, explaining that a 1% incrase in pctstu is associated with a .5% increase in
#          rent. 
# Q4.4(ii): Since we are not correcting for autocorrelation, the standard errors are highly likely to be wrong. 
# Q4.4(iii): after differencing the model, the estimated coefficient on pctstu is almost doubled, which means instead of a 1% increase in pctstu            
#            being associated with a .5% increase in rent, it (in the new differenced model) is associated with a 1.1% increase in rent. In both           
#            models, the variable is statistically significant. The relative size of the student population does seem to affect rental prices. 
# Q4.4(iv): Both the first differenced model and the fixed effects model return the same standard errors and estimated coefficients. 



con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
rental <- con %>% dbReadTable('RENTAL') %>% data.table
con %>% dbReadTable('RENTAL_labels')
con %>% dbDisconnect

#(i)
rental <- rental %>% pdata.frame(index=c('city','year'))
model1 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,model="pooling",data=rental)
summary(model1)
#(iii)
model2<- plm(log(rent)~log(pop)+log(avginc)+pctstu,model="fd",data=rental)
summary(model2)
#(iv)
model3 <- plm(log(rent)~y90+log(pop)+log(avginc)+pctstu,model="within",data=rental)
summary(model3)


################################################################################
################################# Question 4.5 #################################


# Q4.5(i): if past executions of convicted murderers have a deterrent effect on the murder rate, then I expect b1 to have negative sign, meaning
#          that more executions is associated with a lower murder rate. For unemployment rate, I would guess that a higher unemployment rate
#          would be associated with a higher murder rate. 
# Q4.5(ii): Estimated equation: mrdrte = -4.889 + .115exec + 2.288unem, R^2 = .093, n = 51, N = 102, T = 2
#           after estimating the model, there is no evidence of a detterrent affect coming from the executions. 
# Q4.5(iii): Estimated Fixed Effects (first differenced) Model: mrdrte = .413 - .104exec - .067unem, n = 51, T = 2, N = 102
#            With the new model, there is evidence of a detterrent effect. A 1 unit increase in exec (that is, 1 more total execution) is 
#            associated with a -.1% decrease in the murder rate. This is not a strong deterrent effect. 
# Q4.5(iv): the robust standard errors for the estimated model for the intercept, exec and unem are 6.229, .17, and 1.318, respectively.
# Q4.5(v): The state with the most executions in 1993 is Texas with 34. This is 23 executions higher than Virginia with 11. 
# Q4.5(vi): Estimated Equation: mrdrte = .413 - .104exec - .067unem, n = 51, T = 2, N = 102
#           Usual Standard Errors: .211, .105, and .16 for the intercept, exec, and unem, respectively. 
#           Robust Standard Errors: .194, .077, and .142 for the intercept, exec, and unem, respectively.
#           the standard errors are quite similar across both models, however the robust standard errors are smaller than that of the usual. 
# Q4.5(vii): When only using years 90 and 93, the deterrent effect (exec) is satistically significant at the 5% level and has a value of -.103
#            When using all three years (87,90,93) the deterrent effect is not statistically significant but has a slighly larger effect on
#            mrdrte. 

con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
murder <- con %>% dbReadTable('MURDER') %>% data.table
con %>% dbReadTable('MURDER_labels')
con %>% dbDisconnect

# murder %>% subset(year==90|year==93)
#(ii)
sub <- murder[murder$year %in% c("90","93"),]
sub <- sub %>% pdata.frame(index=c('id','year'))
model1 <- plm(mrdrte~exec+unem,model="pooling",data=sub)
summary(model1)
#(iii)
model2 <- plm(mrdrte~exec+unem,model="fd",data=sub)
summary(model2)
coeftest(model2, vcov.=vcovHC)
#(iv)
coeftest(model1, vcov.=vcovHC)
#(v)
murder[year==93][order(exec)]
#(vi)
sub2 <- murder[murder$year %in% c("90","93"),]
# sub2 <- murder[year==90|year==93]
sub2 <- sub2[(sub$state != 'TX'),]
sub2 <- sub2 %>% pdata.frame(index=c('id','year'))
model3 <- plm(mrdrte~exec+unem,model="fd",data=sub2)
summary(model3)
coeftest(model3, vcov.=vcovHC)
#(vii)
pmurder <- murder %>% pdata.frame(index=c('id','year'))
model4 <- plm(mrdrte~exec+unem,model="within",data=murder)
summary(model4)
coeftest(model4, vcov.=vcovHC)

# professor parker included dummy variables for year in his model, so below is the estimated model with those dummy variables if needed. 
model5 <- plm(mrdrte~exec+unem+as.factor(year),data=murder,model="within")
summary(model5)
coeftest(model4, vcov.=vcovHC)

################################################################################
################################# Question 4.6 #################################


# Q4.6(i): Using year 97 as the base, the estimated model is: 
#          log(fare) = 6.209 + .36concen - .902log(dist) + .103(log(dist))^2 + .021(y1998) + .038(y1999) + .1(y2000), n = 1149, T=4, N = 4596
#          if concen increases by .1, there is an associated increase of 100*.1*.36 or 3.6% increase in fare. 
# Q4.6(ii): Usual OLS confidence interval: lower limit = .301, upper limit = .419
#           Using the robust standard errors, the lower limit = .246, and the upper limit is = .474
#           the robust standard errors fall in a larger range, meaning the values are statistically different from 0 over a larger range. 
# Q4.6(iii): by setting up the equation: 0 = b2 + 2b3log(dist), I can solve for the turning point value. 
#            The value at which the relationship between log(fare) and dist becomes positive is 79.758, which falls outside the values in
#            the dataset. 
# Q4.6(iv): The fixed effects estimate of the coefficient on concen is .169, whereas in the pooled OLS model it was .36
# Q4.6(v): two characteristics captured by ai are amount of luggage on the plane and traffic between stops. These might be correlated with 
#          concen. 
# Q4.6(vi): Yes, I am convinced that a higher concentration on a route increases airfare. While highly statistically significant in both models,
#           I believe the best estimate is from the fixed effects model with an estimated b1 value of .169



con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
airfare <- con %>% dbReadTable('AIRFARE') %>% data.table
con %>% dbReadTable('AIRFARE_labels')
con %>% dbDisconnect
#(i) & (ii)
airfare <- airfare %>% pdata.frame(index=c('id','year'))
model1 <- plm(log(fare)~concen+log(dist)+I((log(dist))^2)+as.factor(year),model="pooling",data=airfare)
summary(model1)
#(iii)
.902/(2*.103)
exp(4.379)
#(iv)
model2 <- plm(log(fare)~concen+log(dist)+I((log(dist))^2)+as.factor(year),model="within",data=airfare)
summary(model2)


################################################################################
################################# Question 4.7 #################################


# Q4.7(i): Using the logit model, when the candidate is white (that is, white = 1), there is an associated 14% increase in the probability of loan approval. 
#          Using the linear model, there is an estimated 20% increase in loan approval probability if the candidate is white. 
# Q4.7(ii): After adding the other explanatory variables, the coefficient and estimates for the white variable are still highly statistically
#           significant, explaining discrimination against nonwhites. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
loanapp <- con %>% dbReadTable('LOANAPP') %>% data.table
con %>% dbReadTable('LOANAPP_labels')
con %>% dbDisconnect
#(i)
model1 <- glm(approve~white,family="binomial",data=loanapp)
summary(model1)
margins(model1)
model2 <- lm(approve~white,data = loanapp)
summary(model2)
#(ii)
model3 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family="binomial",data=loanapp)
summary(model3)



################################################################################
################################# Question 4.8 #################################


# Q4.8(i): Of the 9822 men in the dataset, 8822 of them (89.82%) are employed.
#          9.9% of men in the dataset abuse alcohol. 
# Q4.8(ii): Estimated equation: employ = .901 - .028abuse, R^2 = .0008, N = 9822
#           the robust standard errors are .003 and .011 for the intercept and abuse, respectively.
#           The relationship explains that when alcohol is abuse (that is, abuse = 1), employment is less likely, which makes sense. 
#           the term for abuse is statistically significant at the 5% level. 
# Q4.8(iii): In the logit model, the sign remains the same but the statistical significance of the abuse variable is increased to the 1% level. 
#            In the logit model, the marginal decrease is 2.589%, while the linear model the marginal decrease is 2.83%. These values are fairly similar. 
# Q4.8(iv): The fitted values for abuse = 0 is .901 and for abuse = 1 is .873
#           these values are identical in both the logit and the LPM.  
# Q4.8(v): The estimated coefficient remains almost exactly the same, and assuming a comparison between the robust standard errors, the 
#          satistical significance of abuse changes from the 5% level to the 10% level after adding the additional explanatory variables. 
# Q4.8(vi): In the logit model, the marginal effect of abuse is an associated decrease of 1.938% when the person abuses alcohol (abuse = 1)
#           In the linear model, the marginal effect of abuse is an associated decrease of 2.025% when the person abuses alcohol. 
#           These values are close. 
# Q4.8(vii): I don't believe it is obvious that an overall health indicator variable should be included as a control because alcoholism likely
#            is a large contributor to overall health, and health concerns can be strictly due to alcohol abuse alone. 
# Q4.8(viii): Abuse might be endogenous because it might be determined by other factors in the model, rather than being a determinant itself. 
#             mothalc and fathalc are not good instrumental variables because they do not meet the requirements of an instrumental variable,
#             mainly because mother and father's alcohol abuse might directly affect the employment of their child in some way. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
alcohol <- con %>% dbReadTable('ALCOHOL') %>% data.table
con %>% dbReadTable('ALCOHOL_labels')
con %>% dbDisconnect

#(i)
mean(alcohol$employ==1)
mean(alcohol$abuse==1)
#(ii)
model1 <- lm(employ~abuse,data=alcohol)
summary(model1)
coeftest(model1, vcov.=vcovHC)
#(iii)
model2 <- glm(employ~abuse,family = binomial(link="logit"),data=alcohol)
summary(model2)
coeftest(model2, vcov.=vcovHC)
margins(model2)
#(iv)
predict(model1,data.frame(abuse=c(0,1)))
predict(model2,data.frame(abuse=c(0,1)),type="response")
#(v)
model3 <- lm(employ~abuse+age+agesq+educ+educsq+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3,data=alcohol)
summary(model3)
coeftest(model3, vcov.=vcovHC)
#(vi)
model4 <- glm(employ~abuse+age+agesq+educ+educsq+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3,family = "binomial"(link="logit"),data=alcohol)
summary(model4)
margins(model4)


################################################################################
################################# Question 4.9 #################################


# Q4.9(i): Using 1972 as the base, when the year is 1982, there is an associated 19.26% decrease in the fertility rate in comparison to 1972. 
# Q4.9(ii): Holding other factors fixed, there is an associated 36% increase in fertility rate when the woman is black (that is, black = 1)
# Q4.9(iii): Fitting a linear model, the R^2 value is .1295, while computing the R^2 for the poisson model, R^2 is .1209. This points to the 
#            linear model being a better fit. 




con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
fertil <- con %>% dbReadTable('FERTIL1') %>% data.table
con %>% dbReadTable('FERTIL1_labels')
con %>% dbDisconnect
#(i) & (ii)
model1 <- glm(kids~educ+age+agesq+black+east+northcen+west+farm+othrural+town+smcity+y74+y76+y78+y80+y82+y84,family=poisson,data=fertil)
summary(model1)
#(iii)
model2 <- lm(kids~educ+age+agesq+black+east+northcen+west+farm+othrural+town+smcity+y74+y76+y78+y80+y82+y84,data=fertil)
summary(model2)

fitted(model1)
kids_hat <- fitted(model1)
rpois <- cor(kids_hat,fertil$kids)
rpois^2

