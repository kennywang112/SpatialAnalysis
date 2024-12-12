# Runs a Pearson's correlation
cor(Census.Data$Unemployed, Census.Data$Qualification)
# Runs a Pearson's correlation
cor.test(Census.Data$Unemployed, Census.Data$Qualification)

# Runs a Spearman's correlation
cor.test(Census.Data$Unemployed, Census.Data$Qualification, method="spearman")

# creates a data1 object which does not include the 1st column from the original data
data1 <- Census.Data[,2:5]
# creates correlation matrix
cor(data1)
round(cor(data1),2)

library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(data1, use="p")), fill=value, geom="tile") + scale_fill_gradient2(limits=c(-1, 1))

# Regression
model_1 <- lm(Census.Data$Qualification~ Census.Data$Unemployed)

# confidence intervals 
confint(model_1, level= 0.95)

model_2 <- lm(Census.Data$Qualification~ Census.Data$Unemployed + Census.Data$White_British)
# view the model summary
summary(model_2)

plot(Census.Data$Unemployed, Census.Data$Qualification, xlab="% Unemployed", ylab="% With a Qualification") + 
  abline (model_1) + abline(model_2, col="red")
