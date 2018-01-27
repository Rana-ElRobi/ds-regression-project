# Set orking directory
setwd("/home/rana/Desktop/Ds-Coursera/06-Regression-Modeling/")
data("mtcars")
# Step 1 : Preprocessing  
head(mtcars)
## check attributes 
str(mtcars)

## factories some attributes 
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

#------------------------------------------

# Step 2 : Do Some Analysis 
## check linear regression modelling
fit.linear.mod <- lm(mpg ~ am, mtcars)
### lm results
summary(fit.linear.mod)
## review coefs
beta <- .7245  
SE <- 1.764        
t <- qt(1-0.05/2, df = length(mtcars$mpg) - 2)
c(beta - t*SE, beta + t *SE)

## multi-variant analysis
fit.all.vars <- lm(mpg ~ . , mtcars)

## explore nesesary variables
library(MASS)
aci.step <- stepAIC(fit.all.vars, direction="both", trace=FALSE)
summary(aci.step)
### exploring results

# Step 3 : Compare models
anova(fit.linear.mod, aci.step)
# Step 4 : Find Significance of the transmission type on mpg
## check coefs
coefficients(summary(aci.step))
# Concluded Results 
# Reviewing the p-values in the summary data, we can see that the p-value for am (automatic vs. manual transmission)
# is not significant in the measurement of mpg. 
# This can be proven with the confidence interval formula as done previously in the Exploratory analysis section.

beta1 <- 1.80921138  #From the summary for am 
SE1 <- 1.39630450       #From the summary for am
t1 <- qt(1-0.05/2, df = length(mtcars$mpg) - 2)
c(beta1 - t1*SE1, beta1 + t1 *SE1)

# Figures
#---------
## lm figure
par(mfrow=c(2,2))
plot(fit.linear.mod); 
abline(fit.linear.mod)

## Multivariant figure
par(mfrow=c(2,2))
plot(aci.step); 
abline(aci.step)

#--------------------------------------------------

# generate html page from RMD file
library(knitr)
library(markdown)
markdownToHTML("/home/rana/Desktop/Ds-Coursera/06-Regression-Modeling/ReadME.rmd", "/home/rana/Desktop/Ds-Coursera/06-Regression-Modeling/ReadME.html")  # converts an md file to html


