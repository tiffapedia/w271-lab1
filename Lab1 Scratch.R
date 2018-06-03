# 1. The failure of an O-ring on the space shuttle Challenger’s booster rockets led to its destruction in 1986. Using data on previous space shuttle launches, Dalal et al. (1989) examine the probability of an O-ring failure as a function of temperature at launch and combustion pressure. Data from their paper is included in the challenger.csv file. Below are the variables: 
# * Flight: Flight number 
# * Temp: Temperature (F) at launch
# * Pressure: Combustion pressure (psi)
# * O.ring: Number of primary field O-ring failures
# * Number: Total number of primary field O-rings (six total, three each for the two booster rockets) 
# 
# The response variable is O.ring, and the explanatory variables are Temp and Pressure. Complete the following: 
# * (a) The authors use logistic regression to estimate the probability an O-ring will fail. In order to use this model, the authors needed to assume that each O-ring is independent for each launch. Discuss why this assumption is necessary and the potential problems with it. Note that a subsequent analysis helped to alleviate the authors’ concerns about independence. 
# * (b) Estimate the logistic regression model using the explanatory variables in a linear form. 
# * (c) Perform LRTs to judge the importance of the explanatory variables in the model. 
# * (d) The authors chose to remove Pressure from the model based on the LRTs. Based on your results, discuss why you think this was done. Are there any potential problems with removing this variable? 
# 
# 2. Continuing Exercise 4, consider the simplified model logit(π) = 0 + 1Temp, where π is the probability of an O-ring failure. Complete the following: 
#   (a) Estimate the model. how the slope changes as a function of x1. 
# * (b) Construct two plots: (1) π vs. Temp and (2) Expected number of failures vs. Temp. Use a temperature range of 31 to 81 on the x-axis even though the minimum temperature in the data set was 53. 
# * (c) Include the 95% Wald confidence interval bands for π on the plot. Why are the bands much wider for lower temperatures than for higher temperatures? 
# * (d) The temperature was 31 at launch for the Challenger in 1986. Estimate the probability of an O-ring failure using this temperature, and compute a corre- sponding confidence interval. Discuss what assumptions need to be made in order to apply the inference procedures. 
# * (e) Rather than using Wald or profile LR intervals for the probability of failure, Dalal et al. (1989) use a parametric bootstrap to compute intervals. Their process was to (1) simulate a large number of data sets (n = 23 for each) from the estimated model of logit(πˆ) = ˆ0 + ˆ1Temp; (2) estimate new models for each data set, say logit(πˆ) = ˆπ + ˆπTemp; and (3) compute ⇡ˆ? at a specific temperature of interest. The authors used the 0.05 and 0.95 observed quantiles from the ⇡ˆ? sim- ulated distribution as their 90% confidence interval limits. Using the parametric bootstrap, compute 90% confidence intervals separately at temperatures of 31 and 72.27 
# * (f) Determine if a quadratic term is needed in the model for the temperature. 

#1b)
#When the independent values have no influence or 0 value
lm0 <- glm(O.ring >0 ~ 1, family=binomial(link='logit'), data=d)
lm1 <- glm(O.ring >0 ~ Temp, family=binomial(link='logit'), data=d)
lm2 <- glm(O.ring >0 ~ Pressure, family=binomial(link='logit'), data=d)
lm3 <- glm(O.ring >0 ~ Temp + Pressure, family=binomial(link='logit'), data=d)
library(stargazer)
lm1b <- glm(O.ring >0 ~ Temp, family=binomial(link='logistic'), data=d)
stargazer(lm0, lm1, lm2, lm3, type = "text")

#1c) Perform LRTs to judge the importance of the explanatory variables in the model. 
#is the model with temperature better than the one with base intercept? reject null hypothesis, pvalue .004804
anova(lm0, lm1)
1 -pchisq(lm1$null.deviance-lm1$deviance, lm1$df.null-lm1$df.residual)
lrtest(lm1, lm0)
#to see if the variable is statistically significant in the multivariate model, run lrt on on each individual
library("lmtest")
lrtest(lm1, lm0)
lrtest(lm3, lm1)
lrtest(lm3, lm2)
anova(lm3, lm2, test = "LRT")
anova(lm3, lm1, test = "LRT")
#comparing model ~ temp + pressure to ~ pressure, temp is significant, adding pressure isnt significant
anova(lm0, lm1, lm2, lm3, test = "Chisq")
#logistic regression model with just temperature has teh most significant contribution

#G^2
deviance(lm3)
deviance(lm1)

lm3$deviance
c(logLik(lm3), logLik(lm2), logLik(lm1), logLik(lm0))
2*log(logLik(lm1)/logLik(lm3))

#Basic Binomial Model, Pre-logistic reg
binom1 <- glm(formula = cbind(d$O.ring, d$Number - d2$O.ring) ~ Temp, family = binomial, data=d2)
binom2 <- glm(formula = cbind(d$O.ring, d$Number - d$O.ring) ~ Temp + Pressure, family = binomial, data=d2)

#Binomial Models
wbm0 <- glm(formula = O.ring/Number ~ 1, family=binomial,  weights = Number, data=d)
wbm1 <- glm(formula = O.ring/Number ~ Temp, family=binomial, weights = Number, data=d)
wbm2 <- glm(formula = O.ring/Number ~ Pressure, family=binomial, weights = Number, data=d)
wbm3 <- glm(formula = O.ring/Number ~ Temp + Pressure, family=binomial, weights = Number, data=d)
wbm4 <- glm(formula = O.ring/Number ~ Temp + I(Temp^2), family=binomial, weights = Number, data=d)
stargazer(wbm0, wbm1, wbm2, wbm3, wbm4, header=FALSE, type='text')
wbm1.b0 <- bm1$coefficients[1]
wbm1.b1 <- bm1$coefficients[2]

#Binary Models
bm0 <- glm(formula = O.ring > 0 ~ 1, family=binomial,  data=d)
bm1 <- glm(formula = O.ring > 0 ~ Temp, family=binomial, data=d)
bm2 <- glm(formula = O.ring > 0 ~ Pressure, family=binomial, data=d)
bm3 <- glm(formula = O.ring > 0 ~ Temp + Pressure, family=binomial, data=d)
stargazer(bm0, bm1, bm2, bm3, header=FALSE, type='text')
bm1.b0 <- bm1$coefficients[1]
bm1.b1 <- bm1$coefficients[2]

$$\begin{aligned} 
logit(\pi_i) = 5.085 - 0.1156 Temp_i
\end{aligned}$$
  
  $$\begin{aligned} 
P(Y_i = 1 | x_i) = \frac{e^{5.085 - 0.1156 x_i}}{1 + e^{5.085 - 0.1156 x_i}} = \pi_i 
\end{aligned}$$
  

exp(5.085-0.1156*31)/(1+exp(5.085-0.1156*31))
exp(5.085-0.1156*31)
15.043-.2322*31

1-(1-exp(15.043-.2322*31)/(1+exp(15.043-.2322*31)))^6

exp(13.08-.238*31+.35*200)/(1+exp(13.08-.238*31+.35*200))


#changing data layout for d
w2 <- aggregate(formula = O.ring ~ Temp, data = d, FUN = sum)
n2 <- aggregate(formula = O.ring ~ Temp, data = d, FUN = length)
d2 <- data.frame(Temp = w2$Temp, O.ring = n2$O.ring, trials = n2$O.ring, proportion = round(w2$O.ring/n2$O.ring,4))

