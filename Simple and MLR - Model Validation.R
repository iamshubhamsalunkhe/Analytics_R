rm(list=ls())

## Load the MASS Library

library(MASS)

data <- Boston
?Boston

## Train the model using all predictors

model1 <- lm(medv ~ ., data)

summary(model1)
Rsqd1 <- 0.74
Adj_Rsqd1 <- 0.73

predicted_y1 <- predict(model1, data)

testing_y <- data$medv

error <- testing_y - predicted_y1
error_sqd <- error^2

RMSE1 = sqrt(mean(error_sqd))
RMSE1
RMSE1 <- RMSE1 * 1000
RMSE1

# 2 hypothesis tests

# Ho: There is no linear relationship between my dependent and independent variables
# Ho: b1 =B2= B3 ====== Bp =0
# Ha: There is linear relationship between my dependent and independent variables
# Ha: at least one B != 0
# if p-value: < 2.2e-16 < alpha = 0.05 # Rej Ho

# Ho: an independent variable = 0 i.e. a particular B = 0
# # Ha: B != 0
# if p-value: < alpha = 0.05 # Rej Ho

# Call:
#   lm(formula = medv ~ ., data = data)
# 
# Residuals:
#   Min      1Q    Median      3Q     Max 
# -15.595  -2.730  -0.518   1.777  26.199 
# 
# Coefficients:
#                 Estimate  Std. Error t value Pr(>|t|)    
#   (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
#   crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
#   zn           4.642e-02  1.373e-02   3.382 0.000778 ***
#   indus        2.056e-02  6.150e-02   0.334 0.738288    
#   chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
#   nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
#   rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
#   age          6.922e-04  1.321e-02   0.052 0.958229    
#   dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
#   rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
#   tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
#   ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
#   black        9.312e-03  2.686e-03   3.467 0.000573 ***
#   lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.745 on 492 degrees of freedom
# Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
# F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

# When we want to study the relationship and its effects on a dependent variables and predict 
# the future values, we would want optimum number of independent variables 

# Check the correlation values

library(corrplot)
cor_data <- cor(data)
corrplot(cor_data, method = c("number"), type = "lower")

# considering those independent values where corr > = 0.7 and corr < = -0.7

model2 <- lm(medv ~ lstat + rm, data)

summary(model2)
Rsqd2 <- 0.64
Adj_Rsqd2 <- 0.63

predicted_y2 <- predict(model2, data)

error <- testing_y - predicted_y2
error_sqd <- error^2

RMSE2 = sqrt(mean(error_sqd))
RMSE2
RMSE2 <- RMSE2 * 1000
RMSE2

#lets look at the plots of lstat and medv

plot(data$lstat, data$medv)

plot(log(data$lstat), data$medv)

model3 <- lm(medv ~ log(lstat) + rm, data)

summary(model3)
Rsqd3 <- 0.70
Adj_Rsqd3 <- 0.70

predicted_y3 <- predict(model3, data)

error <- testing_y - predicted_y3
error_sqd <- error^2

RMSE3 = sqrt(mean(error_sqd))
RMSE3
RMSE3 <- RMSE3 * 1000
RMSE3

Rsqd_ <- c(Rsqd1,Rsqd2,Rsqd3)
MSE_ <- c(RMSE1,RMSE2,RMSE3)

Model_Validation <- cbind(Rsqd_,MSE_)
rownames(Model_Validation) <- c("model1 - (all variables)",
                                "model2 - (medv on lstat & rm)",
                                "model3 - (medv on log(lstat) & rm)")
Model_Validation

#############################################################################
Rsqd_ <- c(Rsqd1,Rsqd2,Rsqd3)
Adj_Rsqd_ <- c(Adj_Rsqd1, Adj_Rsqd2, Adj_Rsqd3)
No_of_var <- c('13', '2', '2')

tbl <-  cbind(Rsqd_, Adj_Rsqd_, No_of_var)
rownames(tbl) <- c("model1 - (13 variables)",
                   "model2 - (medv on lstat & rm)",
                   "model3 - (medv on log(lstat) & rm)")

tbl
