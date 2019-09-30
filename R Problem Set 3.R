# Problem Set 3
# Hayden Reid hjr160230

setwd("C:/Users/Hayde/OneDrive - The University of Texas at Dallas/All OneDrive Files/UT Dallas/Fall 2018/BUAN 6356/BoxFiles")

rm(list=ls())

library(data.table)
library(DBI)
library(RSQLite)
library(tidyverse)
library(sandwich) # For White correction
library(lmtest) # More advanced hypothesis testing tools
library(tseries) # Time series package
library(broom)
library(forecast)
library(TSA)
library(vars)


################################################################################
################################# Question 3.1 #################################


# Q3.1(i): H0: b13 = 0
#          this hypothesis explains that the catcher variable will have no associated affect on log(salary).
#          After generating the model, the coefficient of the catcher variable is insignificant at the 5% level, therefore
#          we cannot reject the null. It may be important to note that the catcher variable is significant at the 10% level. 
# Q3.1(ii): H0: b13 = 0, b12 = 0, b11 = 0, b10 = 0, b9 = 0
#           to test this, I created a restricted model that does not include the various positions. Then the anova function compares the two models.
#           After computing the anova function, the F statistic is considered insignificant at the 5% level, so we fail to reject the null. 
# Q3.1(iii): The results are consistent across both models. In both cases we fail to reject the null that the player's position has any statistically significant
#            effect on log(salary).


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
mlb1 <- con %>% dbReadTable('MLB1') %>% data.table
con %>% dbReadTable('MLB1_labels')
con %>% dbDisconnect

model1 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher,data=mlb1)
summary(model1)
model2 <- lm(log(salary)~years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar,data=mlb1)
summary(model2)
anova(model1,model2)


################################################################################
################################# Question 3.2 #################################


# Q3.2(i): For these variables, I expect hsize hsperc to have negative coefficiets because a larger class size and a higher (worse) percentile would result in
#          a lower collective GPA. I also expect SAT to have a positive coefficient because a higher SAT score likely would correlate with a higher GPA. I am unsure
#          about the gender and athlete variables. 
# Q3.2(ii): colgpa = 1.24 - .057hsize + .005hsize^2 - .013hsperc + .002sat + .155female + .169athlete, N = 4137, R^2 = .2925
#            The estimate coefficient value for atheletes is .169, meaning that athletes, on average, have a GPA .169 points higher than non-athletes.
#            The athlete variable is statistically significant at all levels in this model.
# Q3.2(iii): colgpa = 3.048 - .053hsize + .005hsize^2 - .017hsperc + .058female + .005athlete, N = 4137, R^2 = .1885
#            Without the SAT variable, the coefficient for athlete drops from .169 to .005 and loses its statistical significance. This estimate is different because
#            the model no longer controls for the collective sat score, and may be explain by collinearity between sat and athlete. 
# Q3.2(iv): colgpa = 1.396 - .057hsize + .005hsize^2 - .013hsperc + .002sat + .175female*athlete + .013male*athlete - .155male*nonath, N = 4137, R^2 = .2925
#           With female nonathletes as the base, the null hypthesis is H0: b5 = 0, meaning that a female athlete should have the same gpa as a female nonathlete,
#           all else held equal. In the model, female*athlete is significant at the 5% level, so we reject the null. 
# Q3.2(v): With male*sat as the base, the new model indicates there is no significance between gender and sat at the 5% level. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
gpa2 <- con %>% dbReadTable('GPA2') %>% data.table
con %>% dbReadTable('GPA2_labels')
con %>% dbDisconnect

model1 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+female+athlete,data=gpa2)
model1
summary(model1)
model2 <- lm(colgpa~hsize+I(hsize^2)+hsperc+female+athlete,data=gpa2)
model2
summary(model2)
gpa2$male <- abs((gpa2$female - 1))
gpa2$nonath <- abs((gpa2$athlete - 1))
model3 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+I(female*athlete)+I(male*athlete)+I(male*nonath),data=gpa2)
summary(model3)
model4 <- lm(colgpa~hsize+I(hsize^2)+hsperc+sat+I(female*athlete)+I(male*athlete)+I(male*nonath)+I(female*sat),data=gpa2)
summary(model4)


################################################################################
################################# Question 3.3 #################################


# Q3.3(i): The expected sign of B1 is positive, indicating a higher likelihood of loan approval if the candidate is white. 
# Q3.3(ii): approve = .708 + .201white, N = 1989, R^2 = .04893
#           the model indicates that white is statistically signifcant at all confidence levels. The coefficient is practically large, indicating that simply being
#           white has an associated 20% increase in the likelihood of loan approval. 
# Q3.3(iii): approve = .937 + .129white + .002hrat - .005obrat - .147loanprc - .007unem - .004male + .046married - .007dep + .002sch + .01cosign + .133chist
#             - .242pubrec - .057mortlat1 - .114mortlat2 - .031vr, N = 1989, R^2 = .1656
#           under the new model, the coefficient is .08 less than before. However, the variable is still statistically significant at all levels and is indicative
#           of discrimination still being present even with these additional variables. 
# Q3.3(iv): approve = 1.18 - .146white + .002hrat - .012obrat + .008I(white*obrate) - .152loanprc - .008unem - .006male + .046married - .008dep + .002sch + .018cosign 
#           + .13chist - .24pubrec - .063mortlat1 - .127mortlat2 - .031vr, N = 1989, R^2 = .1709
#           Whne adding the variable obrat*white, the interaction term is highly significant. 
# Q3.3(v): By setting obrat = 32, I created a variable o1 = obrat - 32 to substitute into the equation from part iv. The coefficient for white is now .113, indicating
#          that when obrat is 32, there is a .113 increase in approval score if the candidate is also white. The 95% confidence interval is from .073 to .152


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
loanapp <- con %>% dbReadTable('LOANAPP') %>% data.table
con %>% dbReadTable('LOANAPP_labels')
con %>% dbDisconnect

model1 <- lm(approve~white,data=loanapp)
model1
summary(model1)
model2 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data=loanapp)
model2
summary(model2)
model3 <- lm(approve~white+hrat+obrat+I(white*obrat)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data=loanapp)
model3
summary(model3)
model3 <- lm(approve~white+hrat+I(obrat-32)+I(white*(obrat-32))+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,data=loanapp)
model3
summary(model3)
confint(model3, 'white', level=0.95)


################################################################################
################################# Question 3.4 #################################


# Q3.4(i): After adjusting for heteroskedasticity, the standard errors increase by a significant amount for each variable and the significance
#          test points toward the variables being less signiciant when they are adjusted for heteroskedasticity. After the adjustment, lotsize is no longer significant
#          at any level and the significance of sqrft is reduced from the 0% level to the .1% level. 
# Q3.4(ii): In the log model, adjusting for heteroskedasticity also increases the standard errors for each variable, however the difference is much less
#           significant than the original (non-log) model. This indicates that there is a lesser degree of heteroskedasticity in the log model.
#           The adjustment also influences the significance of some variables. After the adjustment, log(lotsize) is
#           reduced in significance from the 0% level to the .1% level, but log(sqrft) remains the same level of significance. 
# Q3.4(iii): After comparing the adjustment differences of the log model and the regular model, the data indicates that using the log model reduces
#           heteroskedasticity (even before the adjustment is made). 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
hprice1 <- con %>% dbReadTable('HPRICE1') %>% data.table
con %>% dbReadTable('HPRICE1_labels')
con %>% dbDisconnect

model1 <- lm(price~lotsize+sqrft+bdrms,data=hprice1)
model1
summary(model1)
coeftest(model1,vcov.=vcovHC)
model2 <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
summary(model2)
coeftest(model2,vcov.=vcovHC)


################################################################################
################################# Question 3.5 #################################


# Q3.5(i): colGPA = 1.36 + .413hsGPA + .013ACT - .071skipped + .124PC, N = 141, R^2 = .2593
#          the residuals can be obtained by using the residual function. 
# Q3.5(ii): uhat^2 = -.322 + .13colgpa_hat + .003colgpa_hat^2, N = 141, R^2 = .04934
#           the fitted values can be obtained using the fitted.values command on the model. 
# Q3.5(iii): To ensure all fitted values are positive the minimum value can be identified. 
#            With weights 1/h_hat, skipping a lecture is associated with a .077 decrease in colGPA. Likewise, there is an associated .126 increase in colGPA if
#            the student owns a PC. The skipped variable is significant at all levels and the PC variable is statistically significant at the 5% level. 
# Q3.5(iv): When comparing the heteroskedasticity-robust standard errors and the usual standard errors there is not much change in this model. The standard errors
#           change ever so slightly and the stastistical significance for each variable remains the same. 


con <- SQLite() %>% dbConnect('wooldridge.db')
con %>% dbListTables
gpa1 <- con %>% dbReadTable('GPA1') %>% data.table
con %>% dbReadTable('GPA1_labels')
con %>% dbDisconnect

model1 <- lm(colGPA~hsGPA+ACT+skipped+PC, data = gpa1)
model1
summary(model1)
u_hat <- residuals(model1)
colgpa_hat <- predict(model1)
model2 <- lm(I(u_hat^2)~colgpa_hat+I(colgpa_hat^2),data=gpa1)
summary(model2)
h_hat <- fitted.values(model2)
min(h_hat)
model3 <- lm(colGPA~hsGPA+ACT+skipped+PC, weights = I(1/h_hat), data = gpa1)
summary(model3)
coeftest(model3, vcov.=vcovHC)


################################################################################
################################# Question 3.6 #################################

# 3.6(4): 
# 3.6(5): Without differencing, the model shows a spurious relationship between bitcoin price and goldprice, and a less 
#         significant relationship between bitcoin price and the S&P500 price. 
# 3.6(6): After first differencing, bitcoin price is level stationary.
#         After first differencing, gold price is level stationary.
#         After first differencing, S&P500 price is level stationary.
#         After first differencing, dex is level stationary.
#         After first differencing, oil price is level stationary.
#         It is important to note that after identifying a variable as level stationary, in some cases the trend stationary test will become true 
#         but (per professor Parker) if a variable is level stationary we do not have to run the trend test. 
# 3.6(7): With the variables adjusted for the correct differencing, there is no significant relationships
#         between bitcoin price and the other variables. 
# 3.6(8): The data is plotted below. I have plotted the subset of the data and the differenced data because I was not sure what was expected in
#         this question. 
# 3.6(9): The acf and pacf are plotted below. 
# 3.6(10): The best fit arima model is (3,1,2) 
# 3.6(11): The forecast is plotted below. 
# 3.6(12): The periodogram shows a lot of volatility in the bitcoin price, so it is difficult to make a conclusion about seasonality on this data.
#          The most noteworthy spikes happen roughly every four and a half days. 
# 3.6(13): Using 'monday' as a base variable, plotting the residuals of the model accounting for days of the week in a periodogram reveals that
#          the periodogram has changed only slightly from the original for bitcoin price. I do not think this transformation helps us capture 
#          any seasonality in the data. The inferences made on this periodogram are almost identical to the ones made on the original.
# 3.6(14): Granger causality relationships: 
#          oilprice.l1 granger causes changes in bitcoin price
#          the US/Euro exchange rate at l1 granger causes changes in gold price and at the 10% level so does SP500.l2
#          bitcoin price at lag 2 granger causes changes in oil price
#          oilprice and sp500 at lag 1 granger cause changes in the US/Euro exchange rate
# 3.6(15): The forecasts from the ARIMA model were much more volatile than that of the VAR model. The var model has volatility in the earlier days, but
#          is later less volatile.


gold <- read.csv("gold.csv")
colnames(gold)[1] <- "date"
a <- gold[(gold$goldprice != '#N/A'),]
gold <- a
head(gold)
bitcoin <- read.csv("BIT.csv")
colnames(bitcoin)[1] <- "date"
a <- bitcoin[(bitcoin$CBBTCUSD != '.'),]
bitcoin <- a
head(bitcoin)
sp500 <- read.csv("sp.csv")
colnames(sp500)[1] <- "date"
a <- sp500[(sp500$SP500 != '#N/A'),]
sp500 <- a
head(sp500)
oil <- read.csv("oil.csv")
colnames(oil)[1] <- "date"
a <- oil[(oil$oilprice != '.'),]
oil <- a
head(oil)
dex <- read.csv("DEXUSEU.csv")
colnames(dex)[1] <- "date"
a <- dex[(dex$dex != '.'),]
dex <- a
head(dex)

finaltable <- merge(gold, bitcoin, all = FALSE)
finaltable <- merge(finaltable, oil, all = FALSE)
finaltable <- merge(finaltable, sp500, all = FALSE)
finaltable <- merge(finaltable, dex, all = FALSE)
finaltable$date <- as.Date(finaltable$date)
finaltable$CBBTCUSD <- as.numeric(finaltable$CBBTCUSD)
finaltable$SP500 <- as.numeric(finaltable$SP500)
finaltable$oilprice <- as.numeric(finaltable$oilprice)
finaltable$dex <- as.numeric(finaltable$dex)
finaltable$goldprice <- as.numeric(finaltable$goldprice)

plot.ts(finaltable$CBBTCUSD)
plot.ts(finaltable$goldprice)
plot.ts(finaltable$SP500)
plot.ts(finaltable$dex)
plot.ts(finaltable$oilprice)


head(finaltable)
model1 <- lm(CBBTCUSD~oilprice+goldprice+SP500+dex, data=finaltable)
summary(model1)

kpss.test(finaltable$CBBTCUSD,null="Level")
kpss.test(finaltable$CBBTCUSD,null="Trend")
kpss.test(diff(finaltable$CBBTCUSD,null="Level"))

kpss.test(finaltable$goldprice,null="Level")
kpss.test(finaltable$goldprice,null="Trend")
kpss.test(diff(finaltable$goldprice,null="Level"))

kpss.test(finaltable$SP500,null="Level")
kpss.test(finaltable$SP500,null="Trend")
kpss.test(diff(finaltable$SP500,null="Level"))

kpss.test(finaltable$dex,null="Level")
kpss.test(finaltable$dex,null="Trend")
kpss.test(diff(finaltable$dex,null="Level"))

kpss.test(finaltable$oilprice,null="Level")
kpss.test(finaltable$oilprice,null="Trend")
kpss.test(diff(finaltable$oilprice,null="Level"))

model2 <- lm(diff(CBBTCUSD)~diff(oilprice)+diff(goldprice)+diff(SP500)+diff(dex), data=finaltable)
summary(model2)

ftframe <- data.frame(finaltable)
ftframe <- ftframe[format(ftframe$date,'%Y') != "2017" , ]
ftframe <- ftframe[format(ftframe$date,'%Y') != "2018" , ]

plot.ts(ftframe$CBBTCUSD)
plot.ts(diff(ftframe$CBBTCUSD))
plot.ts(diff(ftframe$goldprice))
plot.ts(diff(ftframe$oilprice))
plot.ts(diff(ftframe$dex))
plot.ts(diff(ftframe$SP500))

acf(diff(ftframe$CBBTCUSD))
pacf(diff(ftframe$CBBTCUSD))

auto.arima(ftframe$CBBTCUSD, max.p=10,max.q=10)
model3 <- arima(ftframe$CBBTCUSD,c(3,1,2))
steps <- 30
future <- forecast(model3,h=steps)
plot(future)
periodogram(diff(ftframe$CBBTCUSD))

ftframe$weekday <- format(ftframe$date, "%A")
ftframe$monday <- 0
ftframe[ftframe$weekday == "Monday",]$monday <- 1
ftframe$tuesday <- 0
ftframe[ftframe$weekday == "Tuesday",]$tuesday <- 1
ftframe$wednesday <- 0
ftframe[ftframe$weekday == "Wednesday",]$wednesday <- 1
ftframe$thursday <- 0
ftframe[ftframe$weekday == "Thursday",]$thursday <- 1
ftframe$friday <- 0
ftframe[ftframe$weekday == "Friday",]$friday <- 1

model4 <- lm(diff(CBBTCUSD)~diff(tuesday)+diff(wednesday)+diff(thursday)+diff(friday),data=ftframe)
summary(model4)
residuals(model4)
daysresid <- residuals(model4)
periodogram(daysresid)

diff_jp <- function(x){
  n <- nrow(x)
  return(x[2:n,]-x[1:n-1,])
}
x <- ftframe %>% dplyr::select(CBBTCUSD,goldprice,oilprice,SP500,dex) %>% log %>% diff_jp
VAR(x,p=1,type="both") %>% AIC
VAR(x,p=2,type="both") %>% AIC
VAR(x,p=3,type="both") %>% AIC
VAR(x,p=4,type="both") %>% AIC
VAR(x,p=5,type="both") %>% AIC

model5 <- VAR(x,p=2,type="both")
summary(model5)

future2 <- (predict(model5))
plot(future2)






