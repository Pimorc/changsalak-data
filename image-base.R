##-----------------------------------------------------
## Packages
##
##-----------------------------------------------------
if(!require(lubridate)){install.packages("lubridate")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(lme4)){install.packages("lme4")}
if(!require(glmmTMB)){install.packages("glmmTMB")}
if(!require(SimplyAgree)){install.packages("SimplyAgree")}

library(lubridate)
library(ggplot2)
library(dplyr)
library(lme4)
library(glmmTMB)
library(SimplyAgree)

##-----------------------------------------------------
## Read data into R
## and clean some data points that are not
### detected since the first census
##-----------------------------------------------------
master.data <- read.csv("image_base.csv", header = T,
                        na.strings = ".")

## make id column with three varaiables
master.data <- master.data %>%
  mutate(master.data, id = paste(bench, order, sp, sep = "-"))


master.data$monitor.date <- as.Date(master.data$monitor.date,
                                    format = "%m/%d/%Y")
master.data <- master.data %>%
  mutate(
    bench = as.factor(bench),
    sp = as.factor(sp),
    season = as.factor(season),
    days.after = as.factor(days.after),
    no = as.factor(no),
    id = as.factor(id)
  )

## show the order and sp ID of the missing value in the first census
length(which(master.data$time.detected == 0))
which(master.data$ground.detect == 0 &
                          master.data$days.after == 40)

## extract row with the first census ground data is missing
zero.ground1 <- c(master.data[c(which(master.data$ground.detected == 0 &
                      master.data$days.after == 40)), 17])

## check if any have only image detected without gound
c(master.data[c(which(master.data$ground.detected == 0 &
                        master.data$image.detected == 1)), 17])

## remove the 0 ground height rows
master.clean <- master.data %>%
  filter(time.detected != 0)

master.no.ground <- master.data %>%
  filter(ground.detected == 0)

##--------------------------------------------------
## Make a summary table
##--------------------------------------------------
summary_bycensus <- master.clean %>%
  group_by(days.after) %>%
  summarise(
    n_ground_detected = sum(ground.detected == 1, na.rm = TRUE),
    n_image_detected = sum(image.detected == 1, na.rm = TRUE),
    percent_detected = n_image_detected/n_ground_detected * 100,
    .groups = "drop"
  )

summary_bycensus

summary_byseason <- master.clean %>%
  group_by(season) %>%
  summarise(
    n_ground_detected = sum(ground.detected == 1, na.rm = TRUE),
    n_image_detected = sum(image.detected == 1, na.rm = TRUE),
    percent_detected = n_image_detected/n_ground_detected * 100,
    .groups = "drop"
  )

summary_byseason

summary_bysp <- master.clean %>%
  group_by(sp) %>%
  summarise(
    n_ground_detected = sum(ground.detected == 1, na.rm = TRUE),
    n_image_detected = sum(image.detected == 1, na.rm = TRUE),
    percent_detected = n_image_detected/n_ground_detected * 100,
    .groups = "drop"
  )

print(summary_bysp, n = 30)

##----------------------------------------------------
## linear regression of height and ca.d
##----------------------------------------------------

hist(master.clean$ground.height)
hist(master.clean$ca.d)

max(master.clean$ground.height)

model.lm <- lm(scale(ca.d) ~ scale(ground.height),
            data = master.clean)

summary(model.lm)

cor.test(scale(master.clean$ca.d), scale(master.clean$ground.height))
cor.test(scale(master.clean$crown1), scale(master.clean$ground.height))

## height and crown area are correlated. correlation estimate is 60%
## data:  scale(master.clean$ca.d) and scale(master.clean$ground.height)
## t = 35.677, df = 2228, p-value < 2.2e-16
#######################################################

##-----------------------------------------------------
## Plot
##------------------------------------------------------
plot(master.clean$image.detected ~ master.clean$ground.height)
plot(master.clean$image.detected ~ master.clean$ca.d)

##-------------------------------------------------
## Model generalized linear mixed model
## fixed effects = height, species, season
## random effect = individual seedling in each bench
##-------------------------------------------------
library(glmmTMB)
model.full.tmb <- glmmTMB(
  image.detected ~ scale(ground.height) + sp + season + (1 |bench/id),
  data = master.clean,
  family = binomial
)
summary(model.full.tmb)

model.no.season <- update(model.full.tmb, . ~ . - season)
model.no.sp <- update(model.full.tmb, . ~ . - sp)
model.no.height <- update(model.full.tmb, . ~ . - scale(ground.height))
AIC(model.full.tmb, model.no.season, model.no.sp, model.no.height)

##best model has height, season, and species.

##---------------------------------------------------
## Model prediction
##---------------------------------------------------
new.dat <- expand.grid(
  season = as.factor(levels(master.clean$season)),
  sp = as.factor(levels(master.clean$sp)),
  ground.height = as.numeric(c(100,200))
)

new.dat$prob.detected <- round(predict(model.full.tmb,
                        newdata = new.dat,
                        type = "response",
                        re.form = NA), digit = 2)

new.dat$percent.detected <- round(100 * predict(model.full.tmb,
                                       newdata = new.dat,
                                       type = "response",
                                       re.form = NA), digit = 2)
write.csv(new.dat, file = "pred_detection_probability.csv")

##--------------------------------------------------
## Height ground vs uav derived
##--------------------------------------------------
height.dat <- master.data %>%
  filter(time.detected == 2)

plot(height.dat$ground.height, height.dat$derived.height)
abline(a = 0, b = 1, col = "red", lwd = 2)



height.dat <-height.dat %>%
  mutate(error = derived.height - ground.height)

summary_error <- height.dat %>%
  group_by(days.after) %>%
  summarise(mae = mean(abs(error)),
            rmse = sqrt(mean(error^2)),
            mape <- mean(abs(error / ground.height)) * 100)

summary_error.bysp <- height.dat %>%
  group_by(sp) %>%
  summarise(mae = mean(abs(error)),
            rmse = sqrt(mean(error^2)),
            mape <- mean(abs(error / ground.height)) * 100)

print(summary_error.bysp, n = 30)

##-------------------------------------------------
## limit of agreement
##-------------------------------------------------
agreement <- agreement_limit(data = height.dat,
                     x = "ground.height",
                     y = "derived.height")

repeated <- agreement_limit(
                    x ="ground.height",
                     y = "derived.height",
                     id = "id",
                     data = height.dat,
                     data_type = "nest")

test_tol <- tolerance_limit(x = "ground.height",
                           y = "derived.height",
                           data = height.dat,
                           prop_bias = TRUE)
plot(test_tol)
check(test_tol)

## log-transform
test.tol.log <- tolerance_limit(
  data = height.dat,
  log_tf = TRUE, # natural log transformation of responses
  x ="ground.height",
  y = "derived.height",
  id = "id", # Subject ID
  condition = "days.after", # Identify condition that may affect differences
  cor_type = "sym" # Set correlation structure as Compound Symmetry
)
plot(test.tol.log)
check(test.tol.log)

### prediction for 1 meter seedlings and 2 meters
new.dat2 <- data.frame(
  ground.height = rep(100, 30),
  sp = as.factor(levels(master.clean$sp))
)

new.dat2$pred <- predict(model.no.season,
                    newdata = new.dat2,
                    type = "response",
                    re.form = NA)
new.dat2$pred.per  <- new.dat2$pred * 100

##--------------------------------------------------
##
##--------------------------------------------------
model.full.bench <- glmmTMB(
  image.detected ~ scale(ground.height) + sp + season + (1 | bench),
  data = master.clean,
  family = binomial
)
summary(model.full.bench)

model.no.season.bench <- update(model.full.bench, . ~ . - season)
summary(model.no.season.bench)
AIC(model.full.bench, model.no.season.bench)
