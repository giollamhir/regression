#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
setwd("C:/Users/chesed/Documents/Data Science Course/RegressionWork/linRegAsst/linear_regression")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)
#where did these labels come from? 

# I also like View(states.data) with the separate table view

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
hist(residuals(sat.mod))


## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
# BEGIN EXERCISE WORK

statEx.data <- readRDS("dataSets/states.rds") 

##   1. Examine/plot the data before fitting the model
str(statEx.data)
statEx.info <- data.frame(attributes(statEx.data)[c("names", "var.labels")])
str(statEx.info)
statEx.info

#sts.en.met <- subset(statEx.data, select = c(!is.na("energy"), !is.na("metro")))

sts.en.met <- subset(statEx.data, select = c("metro", "energy"))
sts.en.met <- na.omit(sts.en.met)
summary(sts.en.met)


str(sts.en.met)

#plot data
plot(sts.en.met)
lines(stats::lowess(sts.en.met))

# correlation between metro and energy
cor(sts.en.met)




##   2. Print and interpret the model `summary'

en.met.mod <- lm(energy ~ metro, # regression formula
              data=sts.en.met) # data set
# Summarize and print the results
summary(en.met.mod) # show regression coefficients table

##   3. `plot' the model to look for deviations from modeling assumptions
plot(en.met.mod)

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

# C-A Comment: I'm going to follow the example of the demo videos and test a number of potential variables together for best model
   # then experiment with removing them to see what the effect of removing variables is

sts.en.states <- subset(statEx.data, select = c("metro", "energy","density","house","senate","waste","green","income","high","college"))
sts.en.states <- na.omit(sts.en.states)
summary(sts.en.states)

str(sts.en.states)

#plot data
plot(sts.en.states)
lines(stats::lowess(sts.en.states))

# correlations 
cor(sts.en.states)
cor(sts.en.states, use = "pairwise.complete.obs")

# C-A Comment:  I'm going to test out anything that looks highly correlated OR
    # that I think logically might be related...

#models
mod1 <- lm(energy ~ metro + density + house + senate + waste + green + income + high + college, data = sts.en.states)
summary(mod1)
SSE1 <- sum(mod1$residuals^2)
RMSE1 <- sqrt(SSE1/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE1
print("only green is asterisked as significant, but the R-Squared of this combination is pretty good (better than that in the demo). I want to test removing others even though they aren't marked as significant.")


# subtract waste
mod2 <- lm(energy ~ metro + density + house + senate  + green + income + high + college, data = sts.en.states)
summary(mod2)
SSE2 <- sum(mod2$residuals^2)
RMSE2 <- sqrt(SSE2/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE2
print("mod2 with waste removed is not better than mod1; rsq and RMSE are slightly but insignificantly worse")

# subtract high, put waste back in 
mod3 <- lm(energy ~ metro + density + waste + house + senate  + green + income + college, data = sts.en.states)
summary(mod3)
plot(mod3)
print("mod3 with high school education removed has very slightly lower residual standard error and very slightly reduced r squared.")
SSE3 <- sum(mod3$residuals^2)
RMSE3 <- sqrt(SSE3/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE3

# subtract college, leave high out also
mod4 <- lm(energy ~ metro + density + waste + house + senate  + green + income, data = sts.en.states)
summary(mod4)
plot(mod4)
SSE <- sum(mod4$residuals^2)
RMSE <- sqrt(SSE/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE



# - senate
mod5 <- lm(energy ~ metro + density + waste + house  + green + income, data = sts.en.states)
summary(mod5)
plot(mod5)
SSE5 <- sum(mod5$residuals^2)
RMSE5 <- sqrt(SSE5/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE5
print("Subtracting senate makes error slightly worse and slightly reduces r squared. Does not improve model.")


# put senate back in, remove density
mod6 <- lm(energy ~ metro + waste + house + senate  + green + income, data = sts.en.states)
summary(mod6)
plot(mod6)
SSE6 <- sum(mod6$residuals^2)
RMSE6 <- sqrt(SSE6/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE6
print("mod6 has negligibly lower r squared but higher RMSE than mod5 or mod4; not really any better than model 4. If there's an extremely simple model for energy use, I haven't found it.")

mod7 <- (lm(energy ~ green, data=sts.en.states))
summary(mod7)
# plot(mod7)
SSE7 <- sum(mod7$residuals^2)
RMSE7 <- sqrt(SSE7/nrow(sts.en.states))
mean(sts.en.states$energy)
RMSE7
print("For a simple model, rather than trial and error with a lot of variables, I could select
        green spending as the highest correlation to energy. The model it yields is nearly as good (in terms of rsq and RMSE) as the model with many variables.")


## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

# added note: from contrast {stats} 
# contr.helmert returns Helmert contrasts, which contrast the second level with the first, the third with the average of the first two, and so on. 



##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

#Add the interaction to the model
en.green.by.income <- lm(energy ~ green*income, 
                         data=sts.en.states) 

#Show the results
summary(en.green.by.income)
coef(summary(en.green.by.income)) # show regression coefficients table


##   2. Try adding region to the model. Are there significant differences
##      across the four regions?
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
mod.en.region <-lm(energy ~ region, 
               states.data) 

summary(mod.en.region)

coef(summary(mod.en.region))
plot(mod.en.region)
hist(residuals(mod.en.region))
anova(mod.en.region)

table(states.data$region)
# try green variable plus region
mod.en.gr.region <-lm(energy ~ green + region, 
                   states.data) 

summary(mod.en.gr.region)
plot(mod.en.gr.region)
anova(mod.en.gr.region)

#add'l reference on anova table http://www.analyticsforfun.com/2014/06/performing-anova-test-in-r-results-and.html