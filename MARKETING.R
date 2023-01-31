library('readr')
df = read_csv("CreditCard_SOW_data.csv")


'''
1). Please read the data into R and create a data frame 
named "sow.data". Please convert consumer IDs to factors and create the following 
2 variables in the data
frame: logIncome = log(Income) and logSowRatio = log(WalletShare/(1-WalletShare)). 
'''

sow.data=read_csv("CreditCard_SOW_data.csv")
sow.data$logIncome= log(sow.data$Income)

sow.data$logSowRatio= log(sow.data$WalletShare/(1-sow.data$WalletShare))
sow.data$ConsumerID = as.factor(sow.data$ConsumerID)



'''
2). Use the function lm( ) to run the regression

logSowRatioij = β0 + β1×Historyi + 2×Balanceij + 3×Promotionij +      
                        4×Historyi×Promotionij + 5×logIncomei×Promotionij + ij

Copy and paste the results here. 

'''

sow.data.lm1 =  lm(logSowRatio~History+Balance+Promotion+History*Promotion+logIncome*Promotion, 
             data=sow.data)

summary(sow.data.lm1) 

AIC(sow.data.lm1)
'''
The coefficient for History:Promotion is -0.002571, which means that for
a one-unit increase in history of spending on the credit card, when a customer 
receives a promotion, the logSowRatio is expected to decrease by 0.002571, holding all other 
variables constant.

The AIC value of -1085.702 suggests that the model is a good fit to the data,
as the negative log-likelihood term is large and the number of parameters is small.
The high R-squared value of 0.8984 indicates that the model explains a high proportion
of the variation in the data.

'''

'''

3). Estimate the following hierarchical linear model using the function lmer( ) 
in the R package "lme4"
 
logSowRatioij = β0i + β1×Balanceij + 2i×Promotionij + ij

β0i = 0 +1×Historyi +i   

β2i = 0 +1×Historyi +2×logIncomei +i   

Following what we did in our class, please rewrite this hierarchical
linear model as a one-level linear regression model with random effects.
Which variables (and interactions) in the regression have fixed effects?
Which ones have random effects?  Specify the variables in lmer() and run
the regression. Copy and paste the summary() of the model results here.   
Please interpret the estimated fixed effects in the regression.
Compare model fit using AIC( ) with the model in (2).  

'''
library('lme4')

sow.data.lm2 <- lmer(sow.data$logSowRatio ~ sow.data$History+sow.data$Balance + sow.data$Promotion + 
               sow.data$History:sow.data$Promotion+sow.data$logIncome:sow.data$Promotion +
                 (1 + sow.data$Promotion | sow.data$ConsumerID),
                data=sow.data,control=lmerControl(optimizer="bobyqa"))

summary(sow.data.lm2)

AIC(sow.data.lm2)



n the random effects section of the model summary, you can see the variances and standard deviations of the random effects for the different groups in the model.

Groups: The groups in this case are the logIncome variable.

Name: The name of the variable whose variance and standard deviation are being reported.

Variance: The variance of the random effect, which is a measure of the spread of the random effect values within each group.

Std.Dev: The standard deviation of the random effect, which is the square root of the variance.

Corr: The correlation between the random effects of different groups.

In this case, you can see that the random effect for the variable (Intercept) has a variance of 0.2516338 and a standard deviation of 0.5016. And the random effect for the variable History has a variance of 0.0002131 and a standard deviation of 0.0146. You can also see that the two random effects are highly correlated (-0.96).

The random effects represent the variation in the intercept and the history of the logIncome variable. These random effects take into account the fact that the logSowRatio may vary at different levels of logIncome, and that the relationship between logSowRatio and the predictor variables may also vary at different levels of logIncome.



in this model, there are two random effects: one for the intercept and one for the variable "History". The random intercept is modeled as the mean logSowRatio (β0i) for each logIncome group, with a variance of 0.2516338. This means that the logSowRatio for each logIncome group is expected to vary around the overall mean logSowRatio, with a standard deviation of 0.5016.

The random effect for "History" is modeled as the deviation of the slope of logSowRatio (β1i) for each logIncome group from the overall slope, with a variance of 0.0002131 and a standard deviation of 0.0146. This means that the effect of "History" on logSowRatio is expected to vary across logIncome groups, with a standard deviation of 0.0146.

The correlation between the random intercept and the random effect of "History" is -0.96, which means that there is a strong negative correlation between the two random effects. This means that if the logSowRatio for a logIncome group is higher than the overall mean, the slope of logSowRatio for that group is likely to be lower than the overall slope.


(Intercept) : This represents the estimated population-level intercept, i.e. the expected value of the outcome variable when all predictor variables are 0. The estimate is 0.41

sow.data$Balance: This represents the estimated population-level coefficient for the predictor variable Balance. The estimate is -0.0000489 which means that for every unit increase in balance, the logSowRatio decreases on average by 0.0000489 units.

sow.data$Promotion: This represents the estimated population-level coefficient for the predictor variable Promotion. The estimate is 0.01764 which means that for every unit increase in promotion, the logSowRatio increases on average by 0.01764 units.

The correlation of fixed effects is a matrix that shows the correlation between the predictor variables. A positive correlation means that when one predictor variable increases, the other predictor variable also tends to increase. A negative correlation means that when one predictor variable increases, the other predictor variable tends to decrease.

In this case, the correlation between sow.data$Balance and sow.data$Promotion is -0.257, indicating a small negative correlation between the two variables.



AIC(sow.data.lm2)


'''
The AIC (Akaike Information Criteria) is a measure of the relative quality of a statistical model. Lower AIC values indicate a better model. In this case, the AIC value for model 1 is -1085 and for model 2 is -2656.

The difference in AIC between two models is an indicator of the relative strength of one model over the other. A difference in AIC of 2 is considered to be a small effect, 4 is a medium effect, and over 10 is a large effect. In this case, the difference in AIC between model 1 and model 2 is -1085 - (-2656) = 1571. This indicates a large effect and suggests that model 2 is a significantly better fit for the data than model 1.

From the coefficients in the fixed effects of model 2, it appears that the variable 'Balance' has a negative coefficient, which means that with a one unit increase in balance, the response variable is likely to decrease by -4.892e-04 units. whereas 'Promotion' variable has positive coefficient(1.764e-01) which means that with a one unit increase in promotion variable, the response variable is likely to increase by 1.764e-01 units.

The coefficients in the random effects of model 2, it appears that the variable 'History' has a variance of 0.0002131 and 'logIncome' has a variance of 0.2516338. The standard deviation is the square root of the variance.

In summary, model 2 is considered to be a better fit for the data than model 1, as indicated by the large difference in AIC values. The coefficients of fixed effects in model 2 suggests that 'Balance' and 'Promotion' are likely to have an effect on the response variable. The random effects suggest that there is a variation in the response variable that is explained by the 'History' and 'logIncome' variables.
'''





'''
In this exercise, we will practice Bayesian estimation for
hierarchical linear models and regressions with random effects using the
same dataset "CreditCard_SOW_Data.csv".

1). Use the function MCMCregress() in the R package "MCMCpack" to estimate the 
linear regression

logSowRatioij = β0 + β1×Historyi +2×Incomei +3×Balanceij + 4×Promotionij + ij

'''

library("MCMCpack")


sow.ba1 = 
  MCMCregress(logSowRatio~History+logIncome+Balance+Promotion, 
              mcmc=6000, thin=6, data=sow.data)
summary(sow.ba1)






sow.ba2 <- MCMChregress(sow.data$logSowRatio ~ sow.data$History+sow.data$Balance + sow.data$Promotion + 
                       sow.data$History:sow.data$Promotion+sow.data$logIncome:sow.data$Promotion +
                       (1 + sow.data$Promotion | sow.data$ConsumerID),group=sow.data$ConsumerID,mcmc=6000, thin=6,
                     data=sow.data)

sow.ba2 = 
  MCMChregress(fixed=logSowRatio~History+Balance+Promotion+History:Promotion+logIncome:Promotion,
                random=~Promotion, group="ConsumerID", mcmc=6000, 
               thin=6, data=sow.data, r=3, R=diag(2))

summary(sow.ba2$mcmc[,1:6])
