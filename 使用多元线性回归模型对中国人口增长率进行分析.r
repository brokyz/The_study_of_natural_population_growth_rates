# 读取中国自然增长率及其相关顺序
dat = read.csv("中国自然增长率及相关数据.csv")
# 删除数据中不需要的列
dat = dat[,-1]
# 更改数据的列标签，使其更便于操作
colnames(dat) = c("y","x2","x3","x4")
dat

# 允许直接使用列表前调用数据
attach(dat)
# 数据散点图
plot(dat)
# 相关系数矩阵
cor(dat)
# 拟合多元线性回归
fit=lm(y~.,data=dat)
fit
# Call:
# lm(formula = y ~ ., data = data)
# Coefficients:
# (Intercept)           x1           x2           x3  
#  15.7197750    0.0003751    0.0497390   -0.0056601 

summary(fit)
# Call:
# lm(formula = y ~ ., data = dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.4613 -0.6229 -0.0797  0.7153  1.2592 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 15.7197750  0.8702058  18.064 1.37e-11 ***
# x2           0.0003751  0.0001061   3.535  0.00300 ** 
# x3           0.0497390  0.0329629   1.509  0.15209    
# x4          -0.0056601  0.0014259  -3.969  0.00123 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.9091 on 15 degrees of freedom
# Multiple R-squared:  0.9363,	Adjusted R-squared:  0.9236 
# F-statistic:  73.5 on 3 and 15 DF,  p-value: 3.379e-09

anova(fit)
# 	Df	Sum Sq	Mean Sq	F value	Pr(>F)
# <int>	<dbl>	<dbl>	<dbl>	<dbl>
# x2	1	164.525046	164.5250460	199.071401	4.592238e-10
# x3	1	4.699165	4.6991652	5.685878	3.074106e-02
# x4	1	13.022146	13.0221462	15.756488	1.233689e-03
# Residuals	15	12.396937	0.8264625	NA	NA

# 相关系数矩阵可视化
library("PerformanceAnalytics")
chart.Correlation(data[,-1], histogram=TRUE, pch=19)


library("car")
vif(lm6.6)

# x2一元回归
fit = lm(y~x2)
fit
# Call:
# lm(formula = y ~ x2)

# Coefficients:
# (Intercept)           x2  
#   1.401e+01   -5.145e-05  
summary(fit)
# Call:
# lm(formula = y ~ x2)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.4976 -1.0325 -0.3228  0.7200  2.4956 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.401e+01  5.388e-01  26.000 3.96e-15 ***
# x2          -5.145e-05  5.339e-06  -9.637 2.66e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.331 on 17 degrees of freedom
# Multiple R-squared:  0.8453,	Adjusted R-squared:  0.8362 
# F-statistic: 92.86 on 1 and 17 DF,  p-value: 2.656e-08

# x3一元回归
fit = lm(y~x3)
fit
# Call:
# lm(formula = y ~ x3)

# Coefficients:
# (Intercept)           x3  
#      8.0310       0.2621 
summary(fit)
# Call:
# lm(formula = y ~ x3)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.1832 -2.1492 -0.4339  1.6051  5.5465 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.03098    0.78533  10.226 1.11e-08 ***
# x3           0.26211    0.07789   3.365  0.00367 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.621 on 17 degrees of freedom
# Multiple R-squared:  0.3998,	Adjusted R-squared:  0.3645 
# F-statistic: 11.32 on 1 and 17 DF,  p-value: 0.003674

# x4一元回归
fit = lm(y~x4)
fit
# Call:
# lm(formula = y ~ x4)

# Coefficients:
# (Intercept)           x4  
#  14.3366203   -0.0006953  
summary(fit)
# Call:
# lm(formula = y ~ x4)

# Residuals:
#    Min     1Q Median     3Q    Max 
# -1.391 -1.063 -0.278  0.692  2.343 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.434e+01  5.305e-01   27.02 2.08e-15 ***
# x4          -6.953e-04  6.729e-05  -10.33 9.55e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.254 on 17 degrees of freedom
# Multiple R-squared:  0.8627,	Adjusted R-squared:  0.8546 
# F-statistic: 106.8 on 1 and 17 DF,  p-value: 9.55e-09

# x2 x4 多元回归
fit = lm(y~x2+x4)
fit
# Call:
# lm(formula = y ~ x2 + x4)

# Coefficients:
# (Intercept)           x2           x4  
#  16.5317284    0.0004048   -0.0061066  
summary(fit)
# Call:
# lm(formula = y ~ x2 + x4)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.7229 -0.7503  0.0721  0.6010  1.4532 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 16.5317284  0.7106604  23.262  9.2e-14 ***
# x2           0.0004048  0.0001084   3.736  0.00180 ** 
# x4          -0.0061066  0.0014495  -4.213  0.00066 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.9447 on 16 degrees of freedom
# Multiple R-squared:  0.9266,	Adjusted R-squared:  0.9175 
# F-statistic: 101.1 on 2 and 16 DF,  p-value: 8.387e-10

fit = lm(y~x2+x3+x4)
fit
# Call:
# lm(formula = y ~ x2 + x3 + x4)

# Coefficients:
# (Intercept)           x2           x3           x4  
#  15.7197750    0.0003751    0.0497390   -0.0056601
summary(fit)
# Call:
# lm(formula = y ~ x2 + x3 + x4)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.4613 -0.6229 -0.0797  0.7153  1.2592 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 15.7197750  0.8702058  18.064 1.37e-11 ***
# x2           0.0003751  0.0001061   3.535  0.00300 ** 
# x3           0.0497390  0.0329629   1.509  0.15209    
# x4          -0.0056601  0.0014259  -3.969  0.00123 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.9091 on 15 degrees of freedom
# Multiple R-squared:  0.9363,	Adjusted R-squared:  0.9236 
# F-statistic:  73.5 on 3 and 15 DF,  p-value: 3.379e-09