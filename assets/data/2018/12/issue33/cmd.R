library(mice)
library(missForest)
library(tidyverse)

data(iris)
summary(iris)
fit <- with(data = iris, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))
summary(fit)

set.seed(1234)
iris.mis <- missForest::prodNA(iris, noNA = 0.1) %>%
    select(-Species)
summary(iris.mis)


md.pattern(iris.mis)
Amelia::missmap(iris.mis)

imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 123)
summary(imputed_Data)

completeData.2 <- mice::complete(imputed_Data, 2)

# build predictive model
fit1 <- with(data = imputed_Data, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))
#combine results of all 5 models
combine <- pool(fit1)
summary(combine)


set.seed(1234)
iris.mis <- missForest::prodNA(iris, noNA = 0.1)
amelia_fit <- amelia(iris.mis, m=5, parallel = "multicore", noms = "Species")
amelia_fit$imputations$imp1

fit2 <- Zelig::zelig(Sepal.Width ~ Sepal.Length + Petal.Width, data = amelia_fit, model = "ls")
summary(fit2)

(dat <- read_csv("~/Desktop/blog/dt_simulated.csv"))
dat %>%
    mutate_if()

original <- dat

set.seed(10)
dat[sample(1:nrow(dat), 20), "Cholesterol"] <- NA
dat[sample(1:nrow(dat), 20), "Smoking"] <- NA
dat[sample(1:nrow(dat), 20), "Education"] <- NA
dat[sample(1:nrow(dat), 5), "Age"] <- NA
dat[sample(1:nrow(dat), 5), "BMI"] <- NA
sapply(dat, function(x) sum(is.na(x)))

dat <- dat %>%
    mutate(
        Smoking = as.factor(Smoking),
        Education = as.factor(Education),
        Cholesterol = as.numeric(Cholesterol)
    )

init = mice(dat, maxit=0)
meth = init$method
predM = init$predictorMatrix

# predM[, c("BMI")]=0
meth[c("Age")]=""

meth[c("Cholesterol")]="norm"
meth[c("Smoking")]="logreg"
meth[c("Education")]="polyreg"

set.seed(1234)
imputed <- mice(dat, method=meth, predictorMatrix=predM, m=5)
imputed <- mice::complete(imputed)

sapply(imputed, function(x) sum(is.na(x)))

# Cholesterol
actual <- original$Cholesterol[is.na(dat$Cholesterol)]
predicted <- imputed$Cholesterol[is.na(dat$Cholesterol)]
mean(actual)
mean(predicted)

# Smoking
actual <- original$Smoking[is.na(dat$Smoking)]
predicted <- imputed$Smoking[is.na(dat$Smoking)]
table(actual)
table(predicted)

