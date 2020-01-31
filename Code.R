---
title: 'Predictive Policing for Crime in Chicago'
author: "Izabela Litwin"
output:
  pdf_document:
    fig_caption: yes
    toc: yes
    toc_depth: 3
  word_document:
    toc: yes
    toc_depth: '3'
  html_document:
    df_print: paged
    toc: yes
    toc_depth: '3'
---

```{r setup, echo=F, include=F}
knitr::opts_chunk$set(echo = F) 
knitr::opts_chunk$set(warning = F) 
knitr::opts_chunk$set(message = F) 
```

```{r setup2, echo=F, include=F}
library(kableExtra)
library(tidyverse)
library(pander)
library(Hmisc)
library(broom)
library(xtable)
library(rpart)
library(stargazer)
set.seed(15)
```

```{r}
data = readRDS(file="DA-exam-data.RData")
colnames(data)[1] = 'PopulationTotal'
data$CrimeTotal = data$CrimeC + data$CrimeNC
```

# Introduction

The goal of this study is to understand how demographic and geographic factors relate to narcotic-related crime in Chicago. In particular, we are interested in knowing whether narcotic-related crimes depend on demographic and geographic factors and whether cannabis and noncannabis-related crimes are correlated. In this study, I present stable, interpretable, theoretically valid regression models to predict narcotic crime in Chicago that address these research questions. 

# Exploratory Data Analysis

## Part 1: Univariate EDA 

The data analyzed in this report comes from narcotic-related crime reports across 2012 per Census block group. There are 15 variables and 2102 observations. Each observation represents a block group. Within each block group, we have variables from the 2010 US Census and the 2011 American Community Survey that provide additional information. The counts of narcotic-related crimes are split in two: cannabis and non-cannabis related cases.

All variables besides zone and ward are continuous. In my analysis, I decided to treat zone as a categorical variable and ward as continuous because of the large number of categories within it that are based on geographical locations in Chicago (they are neighboring locations/similar to each other). The predictor variables used in this report are TotalCrime, CrimeNC and CrimeC. Other variables are used as response variables.

```{r}
# Summary of Continuous Variables and Outliers
datasummary = data[, names(data) != "zone"]
```

```{r mylatextable8, results = "asis", echo = F, message= F, warning=F}
stargazer(datasummary, title="Summary of Continuous Variables", type="latex", font.size="small", header=FALSE)
```

Based on the histograms, we can say that the distribution of the population is right skewed with a mean of 1255.1 and standard deviation of 546.4. The distributions of income (male), income (female), age (male) and age (female) are also right skewed with respective means, medians and ranges specified in the table below. The distribution of 50 wards is relatively uniform. The distributions of cannabis-related crimes, non-cannabis related and total crimes are right-skewed with respective means of 19.6, 16.6, and 36.15. The distribution of pctBlack is bimodal close to 0 and 1. The distributions of pctWhite and pctAsian are right-skewed with modes close to 0. 

Additionally, population, income.male, income.female, pctAsian have very skewed distributions because of the outliers. There are a few outliers in the data set. For example, the group 682 in ward 35 with population of 1514 and total crime of 144. Observation 2091 has the largest population of 11309. Observation 2034 in ward 25 has the highest pctAsian of 0.9386.

## Part 2: Multivariate EDA

Given the correlation matrix (in the Appendix), we can see that CrimeTotal, CrimeC and CrimeNC are strongly correlated which is understandable. When it comes to other variables, some of them are highly correlated. In particular, pctBlack and pctWhite have a correlation of -0.91, pctBlack and latitude have a correlation of -0.58 and income (female) and income (male) have a correlation of 0.52. 

Based on the plots graphing the three responses (TotalCrime, CrimeNC and CrimeC) against all other predictors with trendlines, we can say that there are few interesting trends in the data set. There seem to be positive relationships between income (male) and CrimeC, income (female) and CrimeC. Also, there seems to be a slight negative relationship between CrimeNC and age for females and males. There appears to be no relationship between the geographical variables and total crime. There is a slight positive relationship between CrimeC and longitude. There seems to be a negative relationship between total crime and pctAsian. There seems to be no relationship between zones and crime. 

Plots analyzing the relationships between all available variables and the three responses (TotalCrime, CrimeNC and CrimeC) are included in the Appendix. 

## Part 3: Geographical Patterns

```{r figs534, fig.height=4, fig.width=6, fig.align='center', fig.cap= "Geographical Patterns for Blocks with High and Low Cannabis Related Crime Rates"}
crhigh = data[order(data$CrimeC, decreasing = T),
              ][1:floor(.05*nrow(data)),][, c("longitude", "latitude")]
ncrhigh = data[order(data$CrimeNC, decreasing = T),
               ][1:floor(.05*nrow(data)),][, c("longitude", "latitude")]

crlow = data[order(data$CrimeC, decreasing = F),
             ][1:floor(.05*nrow(data)),][, c("longitude", "latitude")]
ncrlow = data[order(data$CrimeNC, decreasing = F),
              ][1:floor(.05*nrow(data)),][, c("longitude", "latitude")]

plot(crhigh, pch = 16, col = "orange", 
     ylim = c(min(crhigh[,2], ncrhigh[,2], crlow[,2], ncrlow[,2]), 
              max(crhigh[,2], ncrhigh[,2], crlow[,2], ncrlow[,2])),
     xlim = c(min(crhigh[,1], ncrhigh[,1], crlow[,1], ncrlow[,1]), 
              max(crhigh[,1], ncrhigh[,1], crlow[,1], ncrlow[,1])), 
     xlab = "Longitude", ylab = "Latitude",
     main = "Top Cannabis Related Crime Rate Blocks")
points(crlow, pch = 16, col = "steelblue")
legend("topright", legend=c("5% Highest","5% Lowest"),
       col=c("orange","steelblue"), pch=16, cex=0.8, horiz=F)

```

```{r figs5344, fig.height=4, fig.width=6, fig.align='center', fig.cap= "Geographical Patterns for Blocks with High and Low Noncannabis Related Crime Rates"}
plot(ncrhigh, pch = 16, col = "chocolate", 
     ylim = c(min(crhigh[,2], ncrhigh[,2], crlow[,2], ncrlow[,2]), 
              max(crhigh[,2], ncrhigh[,2], crlow[,2], ncrlow[,2])),
     xlim = c(min(crhigh[,1], ncrhigh[,1], crlow[,1], ncrlow[,1]), 
              max(crhigh[,1], ncrhigh[,1], crlow[,1], ncrlow[,1])), 
     xlab = "Longitude", ylab = "Latitude",
     main = "Top Noncannabis Related Crime Rate Blocks")
points(ncrlow, pch = 16, col = "darkturquoise")
legend("topright", legend=c("5% Highest", "5% Lowest"),
       col=c( "chocolate", "darkturquoise"), pch=16, cex=0.8, horiz=F)

```

To see if there is a geograhical pattern in the data for high and low crime rates, upon identifying the top 5% highest, and the bottom 5% lowest crime rate blocks, I plotted them using longitude and latitude variables. 

For cannabis related crime, I see a similar pattern as for non cannabis related crime rate blocks. Most of high crime blocks are concentrated in the higher latitude (between 41.85 and 42). Most of low crime blocks are concentrated in [41.7,41.8] and [41.9,42]. 

For non cannabis related crime rate blocks, it appears that most of high crime blocks are concentrated in the higher latitude (above 41.85). Most of low crime blocks are concentrated above 41.9.

For both cannabis related and non cannabis related crimes, the distribution of longitude is relatively bell shaped, meaning that most of the crimes are in [-87.75,-87.65] range. The difference between cannabis related and non cannabis related crimes in terms of lattitude is that for low crime blocks for noncanabis related crimes, there is a concentration of blocks above 41.95 (mode) and for canabis related crimes there is a concentration below 41.8 (mode). There seem to be a division between those two around the latitude of 41.85. 

# Initial Modeling & Diagnostics

## Part 4: Multiple Linear Regression Models 

I constructed two candidate multiple linear regression models for the total crime count (cannabis and non-cannabis crimes added together) per block. 

\begin{align*}
CrimeTotal_i = \\
& \beta_0 + \beta_{log(PopulationTotal)}log(PopulationTotal)_i + \\ 
& \beta_{log(income.male)}log(income.male)_i +  \\ 
& \beta_{log(income.female)}log(income.female)_i + \\ 
& \beta_{age.male}age.male_i +\\
& \beta_{age.female}age.female_i + \\
& \beta_{Ward}Ward_i + \\
& \beta_{latitude}latitude_i + \\
& \beta_{longitude}longitude_i + \\
& \beta_{pctWhite}pctWhite_i + \\
& \beta_{pctBlack}pctBlack_i + \\
& \beta_{pctAsian}pctAsian_i + \\
& \beta_{zone}zone_i + \epsilon_i
\end{align*}

In the first model, I decided to include all variables except cannabis and non-cannabis crimes because those would simply produce a perfect, meaningless fit. I decided to log transform the following variables: Population, income.male, income.female because the distributions for those variables were very skewed. pctAsian could be log transformed too but I decided not to do it  because some data points have pctAsian=0, which cannot be log transformed. All of variables are treated as continuous variables besides zone which is treated as a factor. I decided to treat Ward as a factor because there are 50 different divisions of the city of Chicago and if I were to treat is as a factor, I would produce too many coefficients in the regression. I decided to include all variables (excluding CrimeNC and CrimeC) because I wanted to see their significance. 
\begin{align*}
CrimeTotal_i = \\
& \beta_0 + \beta_{latitude}latitude_i + \\
& \beta_{pctAsian}pctAsian_i + \\
& \beta_{zone}zone_i + \epsilon_i
\end{align*}

For the second model, I decided to remove some of the explanatory variables in order to see if others that are correlated with those become significant. In particular, to select the variables that should be used in the second model, I used a stepwise model selection using Schwartz’ Bayesian Information Criterion. I chose the model with the lowest BIC. The graph is included in the Appendix. 
 
```{r}
data = readRDS(file="DA-exam-data.RData")
colnames(data)[1] = 'PopulationTotal'
data$CrimeTotal = data$CrimeC + data$CrimeNC

lm1 = lm(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
           log(income.female) + age.male + age.female + Ward + latitude + 
           longitude + pctWhite + pctBlack + pctAsian + as.factor(zone), 
         data = data)

lm2 = lm(CrimeTotal ~ latitude + pctAsian + as.factor(zone), data = data)
```

```{r mylatextable9, results = "asis", echo = F, message= F, warning=F}
stargazer(lm1, lm2, title="Regression Models", type="latex", font.size="small", header=FALSE)
```

## Part 5: Model Selection 

In order to identify a model with the best prediction performance, I performed 5-fold cross-validation to choose between Models 1 and 2 as predictors. For each fold of cross-validation, I created the test set by randomly selecting n=421 (420 for some folds) for each k = 1,..,5.

```{r}
nfold <- 5
samp <- sample(rep(1:nfold, ceiling(nrow(data)/nfold))[1:nrow(data)])
ASPEModel1 = numeric(5)
ASPEModel2 = numeric(5)

for(k in 1:nfold) {
  testd <- data[which(samp==k), ] 
  traind <- data[-which(samp==k), ] 
  model1 <- lm(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
                 log(income.female) + age.male + age.female + Ward + 
                 latitude + longitude + pctWhite + pctBlack + pctAsian + 
                 as.factor(zone) + CrimeTotal, data = traind)
  predictions = predict(model1, newdata = testd)
  ASPEModel1[k] = mean((testd$CrimeTotal - predictions)^2)
}

for(k in 1:nfold) {
  testd <- data[which(samp==k), ] 
  traind <- data[-which(samp==k), ] 
  model2 <- lm(CrimeTotal ~ latitude + pctAsian + as.factor(zone), 
               data = traind)
  predictions = predict(model2, newdata = testd)
  ASPEModel2[k] = mean((testd$CrimeTotal - predictions)^2)
}

errs = data.frame(k = 1:5, Model1 = ASPEModel1, Model2 = ASPEModel2)
  
kable(errs, caption="Average squared prediction errors") %>%
  kable_styling(latex_options = "hold_position", position = 'center')

means = numeric(2)
sds =  numeric(2)

for (i in 2:3) {
  means[i-1] = mean(errs[,i])
  sds[i-1] = sd(errs[,i])/sqrt(nfold)
}

res = data.frame(Model = 1:2, Mean = means, SD = sds)

kable(res, caption="Average and SD of cross-validation error values") %>%
  kable_styling(latex_options = "hold_position", position = 'center')
```

Model 2 is better than Model 1 because its average cross-validation error and std. deviation are smaller than those for Model 1. For Model 1, the estimated cross-validated prediction error is 172.28 with the standard deviation of 9.13 (measure of uncertainty).

```{r figs14, fig.cap= "Model 1 Residual Diagnostics", fig.height=3.5}
par(mfrow = c(1, 2))
qqnorm((lm1$residuals), main = "Normal Q-Q Plot Model 1")
qqline(lm1$residuals)
hist(lm1$residuals, main = "Residuals Model 1", breaks=50)
```

```{r figs12, fig.cap= "Model 2 Residual Diagnostics",  fig.height=3.5}
par(mfrow = c(1, 2))
qqnorm((lm2$resid), main = "Normal Q-Q Plot Model 2")
qqline(lm2$resid)
hist(lm2$resid, main = "Residuals Model 2", breaks=50)

```

By looking at the diagnostic plots and the distributions of residuals (Figures XXXXX), we can say that those models perform very similarly as the distributions of residuals look nearly identical. Since Model 2 has lower average cross-validation error and std. deviation, Model 2 appears to be a better choice than Model 1.

##  Part 6: Model Diagnostics

```{r figs23, fig.height=8, fig.cap= "Model 2 Diagnostic Plots"}
par(mfrow = c(3, 2))
plot(y=lm2$residuals, x=lm2$fitted.values, xlab = "Fitted values", 
     ylab = "Residuals", 
     pch = ".", main = "Residuals vs. Fitted values")
abline(lm(lm2$residuals ~ lm2$fitted.values))
abline(h=0, col ="red", lty = 2)

plot(y=lm2$residuals, x=data$latitude, xlab = "latitude", ylab = "Residuals", 
     pch = ".", main = "Residuals vs. latitude")
abline(lm(lm2$residuals ~ data$latitude))
abline(h=0, col ="red", lty = 2)

plot(y=lm2$residuals, x=data$pctAsian, xlab = "pctAsian", ylab = "Residuals", 
     pch = ".", main = "Residuals vs. pctAsian")
abline(lm(lm2$residuals ~ data$pctAsian))
abline(h=0, col ="red", lty = 2)

boxplot(lm2$residuals ~ data$zone, xlab = "Zone", ylab = "Residuals",
        pch = ".", main = "Residuals vs. Zone")

plot((lm2$residuals)^2, xlab = "Index", ylab = "Residuals^2", 
     pch = ".", main = "Residuals^2")
abline(h = summary(lm2)[6])
abline(h = as.numeric(summary(lm2)[6])*-1)

```

In order to assess the model fit, I produced the diagnostic plots (Figure XXXXX). From the diagnostic plots, we can say that there are some extreme residuals that may point to a non-linear relationship. There is no relationship between residuals and all predictors and fitted values. From the plot showing the squared residuals, we can infer that the variance is non-constant. The normal Q-Q plot allows us to verify the Gaussian error assumption. It is clear that there are deviations from the Q-Q line implying that the error is not approximately normal.

Possible imporvements to the model include adding varaibles such as log(income.female), pctBlack and pctWhite which make the distribution of residuals look more normal (in the Appendix).

## Part 7: Transformations 
Additionally, in this model income (female) is log transformed because its distribution is skewed. pctAsian distribution is skewed and could be log transformed. However, there are 0 values, so it cannot be log transformed. 

## Part 8: Addressed all Model assumptions? 

I was not able to address all concerns about the model assumptions. It would be better if residuals were more normally distributed (followed the qq line) but what I have right now is good enough. Also, the variance should be constant but is not (some outliers).

# Results

## Part 9: A Relationship Between Crime Rate and Geographic and Demographic Variables

```{r}
data = readRDS(file="DA-exam-data.RData")
colnames(data)[1] = 'PopulationTotal'
data$CrimeTotal = data$CrimeC + data$CrimeNC
```

To see if there seem to be a relationship between total crime rate and geographic and demographic variables, I fit the following 3 models.

* Model 1: Crime vs. all (geographic and demographic) variables

\begin{align*}
CrimeTotal_i = \\
& \beta_0 + \beta_{log(PopulationTotal)}log(PopulationTotal)_i + \\ 
& \beta_{log(income.male)}log(income.male)_i +  \\ 
& \beta_{log(income.female)}log(income.female)_i + \\ 
& \beta_{age.male}age.male_i +\\
& \beta_{age.female}age.female_i + \\
& \beta_{Ward}Ward_i + \\
& \beta_{latitude}latitude_i + \\
& \beta_{longitude}longitude_i + \\
& \beta_{pctWhite}pctWhite_i + \\
& \beta_{pctBlack}pctBlack_i + \\
& \beta_{pctAsian}pctAsian_i + \\
& \beta_{zone}zone_i + \epsilon_i
\end{align*}

* Model 2: Crime vs. demographic variables

\begin{align*}
CrimeTotal_i = \\
& \beta_0 + \beta_{log(PopulationTotal)}log(PopulationTotal)_i + \\ 
& \beta_{log(income.male)}log(income.male)_i +  \\ 
& \beta_{log(income.female)}log(income.female)_i + \\ 
& \beta_{age.male}age.male_i +\\
& \beta_{age.female}age.female_i + \\
& \beta_{pctWhite}pctWhite_i + \\
& \beta_{pctBlack}pctBlack_i + \\
& \beta_{pctAsian}pctAsian_i + \epsilon_i
\end{align*}

* Model 3: Crime vs. geographic variables

\begin{align*}
CrimeTotal_i = \\
& \beta_0 +  \beta_{Ward}Ward_i + \\
& \beta_{latitude}latitude_i + \\
& \beta_{longitude}longitude_i + \\
& \beta_{zone}zone_i + \epsilon_i
\end{align*}

I bootstraped the 5-fold cross-validation analysis for the models that were called Model 1 and Model 2, and Model 1 and Model 3. 

I created 200 bootstrap samples each consisting of n = 2102 rows from the data set selected at random with replacement. I used the "resampling cases" form of the bootstrap. 

For each bootstrap sample, I randomly divided the observations into 5 disjoint sets of equal size. Treating each of the 5 folds as test data and the other 4 as training data, I calculated prediction error for each model. Then I computed the difference between the averages of those prediction errors for both models and called it $E(T_j*)$. 

Then I drew a normal q-q plot of the $T*$ values and added the qqline. After confirming that they look like a sample of normal random variables, I run a t test to test the null hypothesis that $E(T_j*)=0$.

### Testing Model 1 vs. Model 2

```{r}
B <- 200
n <- nrow(data)
boot_indices <- replicate(B, sample(1:n, n, replace=TRUE)) 

get_errors <- function(boot_indices, nfold, dat) {
  n <- length(boot_indices)
  samp <- sample(rep(1:nfold, ceiling(n/nfold))[1:n]) 
  prederr1 <- prederr2 <- prederr3 <- rep(NA, nfold)
  tempdata <- dat[boot_indices, ]
    for(j in 1:nfold) {
      traind <- tempdata[samp!=j, ]
      testd <- tempdata[samp==j, ]
      
      fit1 <- lm(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
                   log(income.female) + age.male +
                   age.female + Ward + latitude + longitude + pctWhite + 
                   pctBlack + pctAsian + 
                   as.factor(zone), data=traind)      
      pred1 <- predict(fit1, newdata=testd)
      prederr1[j] <- mean((pred1-testd$CrimeTotal)^2)
      
      fit2 <- lm(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
                   log(income.female) + age.male +
                   age.female + pctWhite + pctBlack + pctAsian, data=traind) 
      pred2 <- predict(fit2,newdata=testd)
      prederr2[j] <- mean((pred2-testd$CrimeTotal)^2) 

      fit3 <- lm(CrimeTotal ~ Ward + latitude + longitude + as.factor(zone), 
                 data=traind) 
      pred3 <- predict(fit3,newdata=testd)
      prederr3[j] <- mean((pred3-testd$CrimeTotal)^2)       
      
    }
  m12 = mean(prederr1-prederr2)
  m12
}

test_errors <- apply(boot_indices, 2, get_errors, nfold=5, dat=data) 
# print(mean(test_errors > 0))
```

Test error ($E(T_j*)$) is defined as the mean of the difference between the prediction error for Model 1 and prediction error for Model 2.

```{r figs022, fig.align='center', fig.height=3.5, fig.width=7,  fig.cap= "T* Distribution (Model 1 vs. Model 2)"}
par(mfrow = c(1, 2))

hist(test_errors, xlab = "Test Errors", main = "Histogram of Test Errors")
abline(v=0, lty =2 )

qqnorm(test_errors) 
qqline(test_errors)
```

It looks like Model 1 is uniformly better than Model 2. Every difference is negative. $T_j*$ values look like a sample of normal random variables. 

$$H_0: E(T_j*) = 0$$
$$H_A: E(T_j*) \neq 0$$

```{r}

kable(as.matrix(tidy(t.test(test_errors))), caption="T Test") %>%
  kable_styling(latex_options = "hold_position", position = 'center')

```

The q-q plot is remarkably straight, and the t test rejects the null hypothesis that $T_j*$ = 0 at virtually all levels. It looks like Model 1 is actually better at predicting.

### Testing Model 1 vs. Model 3

```{r}
get_errors <- function(boot_indices, nfold, dat) {
  n <- length(boot_indices)
  samp <- sample(rep(1:nfold, ceiling(n/nfold))[1:n]) 
  prederr1 <- prederr2 <- prederr3 <- rep(NA, nfold)
  tempdata <- dat[boot_indices, ]
    for(j in 1:nfold) {
      traind <- tempdata[samp!=j, ]
      testd <- tempdata[samp==j, ]
      
      fit1 <- lm(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
                   log(income.female) + age.male +
                   age.female + Ward + latitude + longitude + pctWhite + 
                   pctBlack + pctAsian + 
                   as.factor(zone), data=traind)      
      pred1 <- predict(fit1, newdata=testd)
      prederr1[j] <- mean((pred1-testd$CrimeTotal)^2)
      
      fit2 <- lm(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
                   log(income.female) + age.male +
                   age.female + pctWhite + pctBlack + pctAsian, data=traind) 
      pred2 <- predict(fit2,newdata=testd)
      prederr2[j] <- mean((pred2-testd$CrimeTotal)^2) 

      fit3 <- lm(CrimeTotal ~ Ward + latitude + longitude + as.factor(zone), 
                 data=traind) 
      pred3 <- predict(fit3,newdata=testd)
      prederr3[j] <- mean((pred3-testd$CrimeTotal)^2)       
      
    }
  m13 = mean(prederr1-prederr3)
  m13
}

test_errors <- apply(boot_indices, 2, get_errors, nfold=5, dat=data) 
# print(mean(test_errors > 0))
```

Test error ($E(T_j*)$) is defined as the mean of the difference between the prediction error for Model 1 and prediction error for Model 3.

```{r figs052, fig.align='center',fig.height=3.5, fig.width=7, fig.cap= "T* Distribution (Model 1 vs. Model 3)"}

par(mfrow = c(1, 2))

hist(test_errors, xlab = "Test Errors", main = "Histogram of Test Errors")
abline(v=0, lty =2 )

qqnorm(test_errors) 
qqline(test_errors)
```

10 test errors (6% of 200 T values) were positive, meaning that only for those 10 instances Model 3 performed better than Model 1. Therefore, for most instances, Model 1 performed better than Model 3. $T_j*$ values look like a sample of normal random variables. 

$$H_0: E(T_j*) = 0$$
$$H_A: E(T_j*) \neq 0$$

```{r}
kable(as.matrix(tidy(t.test(test_errors))), caption="T Test") %>%
  kable_styling(latex_options = "hold_position", position = 'center')
```

The q-q plot is remarkably straight, and the t test rejects the null hypothesis that $T_j*$ = 0 at virtually all levels. It looks like Model 1 is better at predicting.

Conclusions:
* Testing Model 1 vs. Model 2 - the null hypothesis is rejected. Therefore, there seem to be a relationship between total crime rate and demographic variables. 
* Testing Model 1 vs. Model 3 - the null hypothesis is rejected. Therefore, there seem to be a relationship between total crime rate and geographic variables. 

As a result, there seem to be a relationship between total crime rate and geographic and demographic variables. 

## Part 10: Relationship between being above or below the river affect the cannabis-related versus non-cannabis-related crime counts

```{r}
lm5 = lm(CrimeC ~ as.factor(zone), data = data)

lm6 = lm(CrimeNC ~ as.factor(zone), data = data)

```

In order to test whether being above or below the river affect the cannabis-related versus non-cannabis-related crime counts in a block tract differently, I decided to set up the data and regression model so that one model is nested in a more general model. I started with the following two regression models:

$$CrimeC_{i} = \beta_0 + \beta_{ZoneC}Zone_i + \epsilon_i $$
$$CrimeNC_{i} = \beta_0 + \beta_{ZoneNC}Zone_i + \epsilon_i $$

I appended the second dataset onto the first dataset. I generated a dummy variable, Dummy, that equals 1 if the data came from CrimeNC and 0 if the data came from CrimeC. Then I generated the interaction between Zone and Dummy. I used the following formula.

$$Crime_{i} = \beta_0 + \beta_{Dummy}Dummy_i + \beta_{Zone}Zone_i + \beta_{Dummy*Zone}Dummy*Zone_i + \epsilon_i $$
The coefficient for the interaction between the Dummy variable and Zone shows the difference between the two initial slopes:  $\beta_{ZoneNC}$ and  $\beta_{ZoneC}$

We would like to test: 
$$H_0: \beta_{ZoneNC} = \beta_{ZoneC}$$
$$H_A: \beta_{ZoneNC} \neq \beta_{ZoneC}$$

This is equivalent to testing: 

$$H_0: \beta_{Dummy*Zone} = 0$$
$$H_A: \beta_{Dummy*Zone} \neq 0$$

```{r}
dt1  = data[, c('zone', 'CrimeC')]
dt1$c = rep(0, nrow(dt1))
colnames(dt1) = c('Zone', 'Crime', 'Dummy')

dt2  = data[, c( 'zone', 'CrimeNC')]
dt2$c = rep(1, nrow(dt2))
colnames(dt2) = c('Zone', 'Crime', 'Dummy')

dt3 = rbind(dt1, dt2)

lm7 = lm(Crime ~ Dummy + as.factor(Zone) + Dummy*as.factor(Zone), data = dt3)
```

```{r mylatextable3, results = "asis", echo = F, message= F, warning=F}
stargazer(lm5,lm6, lm7, title="Regression Results", type="latex", font.size="small", header=FALSE)
```

The t value is 1.547 and p value is 0.122. By assuming our usual treshold of $\alpha = 0.05$, we fail to reject H0. This indicates that the regression coefficient B_NC is not significantly different from B_C. Therefore, not enough evidence is available to suggest that being above or below the river affect the cannabis-related versus non-cannabis-related crime counts in a block tract differently. 

## Part 11: Ranking of Wards with Highest Crime Rates

```{r}
data = readRDS(file="DA-exam-data.RData")
colnames(data)[1] = 'PopulationTotal'
data$CrimeTotal = data$CrimeC + data$CrimeNC

library(dplyr)
d1 = data %>% 
  select(Ward, CrimeTotal, PopulationTotal) %>%
  group_by(Ward)  %>% 
  summarise(CrimeTotal = sum(CrimeTotal),
            PopulationTotal = sum(PopulationTotal))

d1 = as.data.frame(d1)

highestcrime = d1[order(d1$CrimeTotal, decreasing = T), ][1:5,]

kable(highestcrime, caption="Highest Crime") %>%
  kable_styling(latex_options = "hold_position", position = 'center')
```

Wards 35, 1, 21, 34 and 19 have the highest count of crime. The crime count is included in the table below. 

```{r}
lm8 = lm(CrimeTotal ~ PopulationTotal, d1)
resids = data.frame(Ward = 1:50, Residuals = lm8$residuals, 
                    Population = d1$PopulationTotal, Crime = d1$CrimeTotal)
resids$CrimePerPopulationPerc = resids$Crime/ resids$Population *100
residsranked = resids[order(resids$Population, decreasing = T),]
residsranked$RankingPopulation = 1:50

residsranked = residsranked[order(residsranked$Residuals, decreasing = T),]
residsranked$RankingResiduals = 1:50
rownames(residsranked) = NULL
```


```{r}
kable(residsranked[1:5,], caption="Highest Positive Residuals") %>%
  kable_styling(latex_options = "hold_position", position = 'center')
```

The wards with highest crimes (35, 1, 21, 34 and 19) have the highest positive residuals meaning that the actual value of CrimeTotal was higher than the predicted value of CrimeTotal. For those wards, the model underestimates CrimeTotal. Those Walds have populations in the middle of the range and high number of crimes. Those wards also have the highest crime/person rate. Therefore, correcting by population size is not reasonable. The ranking is included in the Appendix. 

## Part 12: Relationship Between Cannabis and Non-cannabis Related Police Reports

In order to see whether there is a relationship between cannabis and non-cannabis related police reports in each block group, I run the following model. 

$$CrimeC_i= \beta_0+ \beta_{log(CrimeNC)}log(CrimeNC) +\epsilon_i$$

```{r}
data = readRDS(file="DA-exam-data.RData")
colnames(data)[1] = 'PopulationTotal'
data$CrimeTotal = data$CrimeC + data$CrimeNC

lm11 = lm(CrimeC ~ log(CrimeNC), data = data)
```

```{r}
lm12 = lm(CrimeC ~ log(CrimeNC) + log(PopulationTotal) + log(income.male) + 
            log(income.female) + age.male + age.female + Ward + 
            latitude + longitude + pctWhite + pctBlack + pctAsian + 
            as.factor(zone), data = data)
```

```{r mylatextable6, results = "asis", echo = F, message= F, warning=F}
stargazer(lm11, lm12, title="Regression Results", type="latex", font.size="small", header=FALSE)
```

I decided to log transform CrimeNC because there are outliers that make the distribution very skewed (plot of CrimeC vc. CrimeNC).

I was interested in testing the following hypotheses:

$$H_0: \beta_{log(CrimeNC)}=0$$
$$H_A: \beta_{log(CrimeNC)} \neq 0$$

The coefficient for log(CrimeNC) is significant at <2e-16, therefore we reject $H_0$, so there seem to be a relationship between cannabis and non-cannabis related police reports in each block group. 

To see what happens to this relationship when you control for the other variables, I fit the following model. 

\begin{align*}
CrimeC_i = \\
& \beta_0 + \beta_{log(CrimeNC)}log(CrimeNC)_i + \\ 
& \beta_{log(PopulationTotal)}log(PopulationTotal)_i + \\ 
& \beta_{log(income.male)}log(income.male)_i +  \\ 
& \beta_{log(income.female)}log(income.female)_i + \\ 
& \beta_{age.male}age.male_i +\\
& \beta_{age.female}age.female_i + \\
& \beta_{Ward}Ward_i + \\
& \beta_{latitude}latitude_i + \\
& \beta_{longitude}longitude_i + \\
& \beta_{pctWhite}pctWhite_i + \\
& \beta_{pctBlack}pctBlack_i + \\
& \beta_{pctAsian}pctAsian_i + \\
& \beta_{zone}zone_i + \epsilon_i
\end{align*}

I am interested in testing the following hypotheses: 

$$H_0: \beta_{log(CrimeNC)}=0$$
$$H_A: \beta_{log(CrimeNC)} \neq 0$$

When I control for the other variables (transformed), the coefficient for log(CrimeNC) is still significant at < 2e-16. This means that the additional predictors are not strongly related to  log(CrimeNC). In other words, predictor variables are not strongly related, so there is no multicollinearity. CrimeNC is not correlated with all other predictors (besides CrimeTotal). This agrres with the correlation matrix graph in the Appendix. 

# Conclusions/Discussion

## Part 13

There is enough evidence to support our hypothesis that demographic and geographic factors relate to narcotic-related crime in Chicago. Narcotic-related crimes depend on demographic and geographic factors. Cannabis and noncannabis-related crimes are correlated. Nevertheless, the correlation does not imply causation. 

The analysis suggests that there is statistical evidence that narcotic-related crimes depend on demographic and geographic factors. Additionally, there is not enough evidence available to suggest that being above or below the river affect the cannabis-related versus non-cannabis-related crime counts in a block tract differently. 

One of the possible reasons for this finding may be the fact that  

Further analysis needs to be done on the relationship between the narcotic-related crime in Chicago and many other predictor variables that could potentially present a relationship with our response variable. 


\newpage

# Appendix 

```{r figs7,  fig.cap= "Univariate EDA (I)", fig.height=6}
data = readRDS(file="DA-exam-data.RData")
colnames(data)[1] = 'PopulationTotal'
data$CrimeTotal = data$CrimeC + data$CrimeNC
## Marginal distribution of each of the variables
# Histograms
par(mfrow = c(3, 2))
hist(data$PopulationTotal, main = "Histogram of PopulationTotal", 
     xlab = "PopulationTotal", col = 'darkturquoise')
hist(data$income.male, main = "Histogram of income.male", xlab = "income.male",
col = 'chocolate')
hist(data$income.female, main = "Histogram of income.female", 
     xlab = "income.female",
col = 'chocolate')
hist(data$income.male, main = "Histogram of age.male", xlab = "age.male",
col = 'darkturquoise')
hist(data$income.female, main = "Histogram of age.female", xlab = "age.female",
col = 'darkturquoise')
hist(data$Ward, main = "Histogram of Ward", xlab = "Ward",
col = 'orange')
```

```{r figs337,  fig.cap= "Univariate EDA (II)", fig.height=8}
par(mfrow = c(4, 2))
hist(data$CrimeC, main = "Histogram of CrimeC", xlab = "CrimeC",
col = 'steelblue')
hist(data$CrimeNC, main = "Histogram of CrimeNC", xlab = "CrimeNC",
col = 'steelblue')
hist(data$CrimeTotal, main = "Histogram of CrimeTotal", xlab = "CrimeTotal",
col = 'steelblue')
hist(data$pctBlack, main = "Histogram of pctBlack", xlab = "pctBlack",
col = 'orange')
hist(data$pctWhite, main = "Histogram of pctWhite", xlab = "pctWhite",
col = 'orange')
hist(data$pctAsian, main = "Histogram of pctAsian", xlab = "pctAsian",
col = 'orange')
hist(data$zone, main = "Histogram of zone", xlab = "zone",
col = 'darkturquoise')
```

```{r figs6,  fig.cap= "Pairwise correlations", fig.height=5, fig.width=6}
 # Pairwise correlations
# kable(round(cor(data), 2), caption = "Covariance Matrix") %>% 
  # kable_styling(latex_options = "hold_position", font_size = 3, position = 'center')

library(ggcorrplot)
cormatrix = cor(data)
ggcorrplot(cormatrix, hc.order = TRUE, outline.col = "white", colors = c("orange", "white", "steelblue"))

# data[data$PopulationTotal == max(data$PopulationTotal),]
# data[data$CrimeTotal == max(data$CrimeTotal),]
# data[data$pctAsian == max(data$pctAsian),]
```

```{r figs5, fig.height=8.5, fig.cap="Multivariate EDA (CrimeTotal, Continuous)"}
par(mfrow = c(5, 3))
datacts = data[, names(data) != "zone"]
for (i in 1:ncol(datacts)) {
  
  if( datacts[,i] != datacts$CrimeTotal) { 
    plot(datacts[,i], datacts$CrimeTotal, xlab = names(datacts)[i], 
       ylab = "CrimeTotal", pch = '.', col = "orange")
    abline(lm(datacts$CrimeTotal~datacts[,i]))
    }
}
```

```{r figs5532, fig.height=8.5, fig.cap="Multivariate EDA (CrimeNC, Continuous)"}
par(mfrow = c(5, 3))
datacts = data[, names(data) != "zone"]
for (i in 1:ncol(datacts)) {
  if( datacts[,i] != datacts$CrimeNC) {
    plot(datacts[,i], datacts$CrimeNC, xlab = names(datacts)[i], 
       ylab = "CrimeNC", pch = '.', col = "darkturquoise")
    abline(lm(datacts$CrimeNC~datacts[,i]))
  }
}
```

```{r figs5353, fig.height=8.5, fig.cap="Multivariate EDA (CrimeC, Continuous)"}
par(mfrow = c(5, 3))
datacts = data[, names(data) != "zone"]
for (i in 1:ncol(datacts)) {
  if( datacts[,i] != datacts$CrimeC) {
    plot(datacts[,i], datacts$CrimeC, xlab = names(datacts)[i], 
       ylab = "CrimeC", pch = '.', col = "steelblue")
  abline(lm(datacts$CrimeC~datacts[,i]))
  }
}
```


```{r figs4, fig.height=2.5, fig.cap="Multivariate EDA (categorical)"}
par(mfrow = c(1, 3))
boxplot(data$CrimeTotal ~ data[, names(data) == "zone"], 
        ylab = 'CrimeTotal', xlab = 'Zone', pch = '.')
boxplot(data$CrimeNC ~ data[, names(data) == "zone"], 
        ylab = 'CrimeNC', xlab = 'Zone', pch = '.')
boxplot(data$CrimeC ~ data[, names(data) == "zone"], 
        ylab = 'CrimeC', xlab = 'Zone', pch = '.')
```

```{r figs3, fig.height=4.5, fig.cap="Log Transformed Variable Fit"}
par(mfrow = c(2, 3))
plot(log(datacts$CrimeNC), datacts$CrimeTotal, xlab = 'log(CrimeNC)', 
     ylab = "CrimeTotal", pch = '.', col = "steelblue")
plot(log(datacts$income.male), datacts$CrimeTotal, 
     xlab = 'log(income.male)', ylab = "CrimeTotal", pch = '.', col = "steelblue")
plot(log(datacts$PopulationTotal), datacts$CrimeTotal, 
     xlab = 'log(PopulationTotal)', ylab = "CrimeTotal", pch = '.', col = "steelblue")
plot(log(datacts$income.female), datacts$CrimeTotal, 
     xlab = 'log(income.female)', ylab = "CrimeTotal", pch = '.', col = "steelblue")
plot(log(datacts$pctAsian), datacts$CrimeTotal,
     xlab = 'log(pctAsian)', ylab = "CrimeTotal", pch = '.', col = "steelblue")

```

```{r figs2, fig.height=4, fig.align='center', fig.width=6, fig.cap="Regression Subset Selection (Schwartz' BIC)"}
library(leaps)
subsets <- regsubsets(CrimeTotal ~ log(PopulationTotal) + log(income.male) + 
                        log(income.female) + age.male + age.female + Ward + 
                        latitude + longitude + pctWhite + pctBlack + 
                        pctAsian + as.factor(zone), data = data)
plot(subsets)
```

Part 6

```{r}
lm3 = lm(CrimeTotal ~ latitude + pctAsian + as.factor(zone) + 
           log(income.female) + pctBlack + pctWhite, data = data)

```

```{r mylatextable1, results = "asis", echo = F, message= F, warning=F}
stargazer(lm3, title="Alternative Model", type="latex", font.size="small", header=FALSE)
```

```{r figs1, fig.cap="Model 2 and 3 Residual Diagnostics", fig.height=5, fig.width=5.5}
par(mfrow = c(2, 2))
qqnorm((lm2$resid), main = "Normal Q-Q Plot Model 2")
qqline(lm2$resid)
hist(lm2$resid, main = "Residuals Model 2", breaks=50)

qqnorm((lm3$resid), main = "Normal Q-Q Plot Model 3")
qqline(lm3$resid)
hist(lm3$resid, main = "Residuals Model 3", breaks=50)
```

