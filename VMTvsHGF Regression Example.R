#FDOT VMT DVMT vs HFG EDA and Regression Analysis

# VMT=Vehicle Miles Traveled
# DVMT= Daily Vehicle Miles Traveled
# HGF= Highway Miles of Fuel comsumed

dataset=read.csv('VMTvsHGF.csv')

#Creating Scatter Plot between VMT (x) and HGF (y)

library(ggplot2)
scatter<-ggplot(dataset, aes(VMT, HGF))
scatter+geom_point()+labs(x="Vehicle Miles Traveled", y="Highway Gallons of Fuel")+
  geom_smooth()

# `geom_smooth()` using method = 'loess' and formula 'y ~ x'
# Loess regression is a nonparametric technique that uses local weighted 
# regression to fit a smooth curve through points in a scatter plot. 
# Loess curves are can reveal trends and cycles in data that might be difficult
# to model with a parametric curve. COnfidence interval is at 95%

# Changing Method to create a "straight line" with 95% confidence interval
scatter+geom_point()+labs(x="Vehicle Miles Traveled", y="Highway Gallons of Fuel")+
  geom_smooth(method="lm", colour="Cyan")

#Scatter plot without confidence interval
scatter+geom_point()+labs(x="Vehicle Miles Traveled", y="Highway Gallons of Fuel")+
  geom_smooth(method="lm", colour="Red", se=F)

#Robust linear model used. Need to add MASS package:
library(MASS)
scatter+geom_point()+labs(x="Vehicle Miles Traveled", y="Highway Gallons of Fuel")+
  geom_smooth(method="rlm", colour="Orange", se=F)

# Robust regression is an alternative to least squares regression when data are 
# contaminated with outliers or influential observations, and it can also be used
# for the purpose of detecting influential observations.

#Robust regression with 99% confidence interval
scatter+geom_point()+labs(x="Vehicle Miles Traveled", y="Highway Gallons of Fuel")+
  geom_smooth(method="rlm", colour="Blue", level=0.99, alpha=0.1, fill="Cyan")

#Robust regression with 99% confidence interval
scatter2<-ggplot(dataset, aes(DVMT, HGF))
scatter2+geom_point()+labs(x="Daily Vehicle Miles Traveled", y="Highway Gallons of Fuel")+
  geom_smooth(method="rlm", colour="Blue", level=0.99, alpha=0.1, fill="Cyan")

# Fitting linear Regression for VMT vs HGF

regressor<-lm(formula=HGF~ VMT, data<-dataset) 

#Summary table
summary(regressor)

# Call:
#   lm(formula = HGF ~ VMT, data = data <- dataset)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -194551021 -112049952  -14316203  136858930  263447125 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.897e+09  1.854e+08   21.02 1.38e-15 ***
#   VMT         2.304e-02  1.033e-03   22.30 4.20e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 148600000 on 21 degrees of freedom
# Multiple R-squared:  0.9595,	Adjusted R-squared:  0.9576 
# F-statistic: 497.4 on 1 and 21 DF,  p-value: 4.198e-16

#Residuals Analysis Plots
library(lindia)
gg_diagnose(regressor)
gg_resX(regressor)
library(olsrr)
ols_test_normality(regressor)
ols_plot_resid_hist(regressor)
