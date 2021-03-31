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
