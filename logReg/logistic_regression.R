## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

setwd("C:/Users/chesed/Documents/Data Science Course/regression/logReg")
##   Load the National Health Interview Survey data:

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

# install.packages("Zelig")
# install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

str(NH11$everwrk) # check stucture of everwrk
levels(NH11$everwrk) # check levels of everwrk
# collapse all missing values to NA
NH11$everwrk <- factor(NH11$everwrk, levels=c("2 No", "1 Yes"))

str(NH11$r_maritl)
table(NH11$r_maritl)  # check values at each level. # Zeros and unknowns include:
 # 0 Under 14 years  
# 3 Married - spouse in household unknown
# also there is a small number in 9 Unknown marital status 
# so I'll omit those three as they won't have meaningful results
NH11$r_maritl <- factor(NH11$r_maritl, levels=c("1 Married - spouse in household", "2 Married - spouse not in household","4 Widowed","5 Divorced","6 Separated", "7 Never married","8 Living with partner"))
summary(NH11$r_maritl)


workd.out <- glm(everwrk~age_p+r_maritl,
               data = NH11, family="binomial") 
coef(summary(workd.out))

workd.out.tab <- coef(summary(workd.out))
workd.out.tab[, "Estimate"] <- exp(coef(workd.out))
workd.out.tab

##   2. Predict the probability of working for each level of marital
##      status.



# I didn't want to use average age because what if it varies significantly between groups
# so bringing in dplyr for an average age in each group

library(dplyr)
prep_age <- NH11  %>% group_by(r_maritl) %>% summarise(m_age_p = mean(age_p))

NH11$m_age_p <- prep_age[match(NH11$r_maritl, prep_age$r_maritl, nomatch = NA_integer_), 2]
summary(NH11)

str(NH11$m_age_p) #is numeric here

# Create a dataset with predictors set at desired levels
predDat2 <- with(NH11,
                expand.grid(r_maritl = c("1 Married - spouse in household", "2 Married - spouse not in household","4 Widowed","5 Divorced","6 Separated","8 Living with partner"),
                            age_p =  mean(m_age_p)))
str(NH11$m_age_p)
str(predDat2)
View(NH11$m_age_p)

# predDat2 <- subset(NH11, select = r_maritl,m_age_p)  %>% group_by(r_maritl)

predDat2 <- subset(NH11, select = c(r_maritl, m_age_p))  %>% group_by(r_maritl)
colnames(predDat2)[2] <- "age_p" # fix name to run against model 
predDat2$age_p <- as.num(predDat2$age_p)
                            # predict hypertension at those levels
cbind(predDat2, predict(workd.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat2))


##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
