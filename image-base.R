##-----------------------------------------------------
## Packages
##
##-----------------------------------------------------
if(!require(lubridate)){install.packages("lubridate")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(lme4)){install.packages("lme4")}
if(!require(glmmTMB)){install.packages("glmmTMB")}

library(lubridate)
library(ggplot2)
library(dplyr)
library(lme4)
library(glmmTMB)

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
## Model
##-------------------------------------------------
start.val <- list(theta = 0)

model.full <- glmer(
  image.detected ~ scale(ground.height) + sp + season +
    (1 | days.after) ,
  data = master.clean,
  family = binomial,
  start = start.val,
  control = glmerControl(
    optimizer = "nloptwrap",
    optCtrl = list(maxfun = 2e5) # increase iterations
  )
) ## not converged

summary(model.full)

summary(model.full)$optinfo$conv$lme4
relgrad <- with(model.full@optinfo$derivs, solve(Hessian, gradient))
max(abs(relgrad))

all_fits <- allFit(model.full)
summary(all_fits) ## All optimizers seem to work the same way
## that's reassuring evidence the fit is fine despite the warning.

##---------------------------------------------------------
## Run models from simpler to more complicated
## sp seems to be the factor that led to
## not converging
##---------------------------------------------------------

m0 <- glmer(image.detected ~ 1 + (1 | days.after),
            data = master.clean,
            family = binomial)
m1 <- update(m0, . ~ . + scale(ground.height))
m2 <- update(m1, . ~ . + season)
m3 <- update(m2, . ~ . + sp)
m4 <- update(m1, . ~ . + sp)
AIC(m0, m1, m2, m3, m4)

summary(m4)

anova(m4, m3)
## season doesn't need to be in the model

anova(m4, m1)
## sp seems to be in the model

library(glmmTMB)
model.full.tmb <- glmmTMB(
  image.detected ~ scale(ground.height) + sp + season + (1 | days.after),
  data = master.clean,
  family = binomial
)
summary(model.full.tmb)

model.no.season <- update(model.full.tmb, . ~ . - season)
model.no.sp <- update(model.full.tmb, . ~ . - sp)
model.no.height <- update(model.full.tmb, . ~ . - scale(ground.height))
AIC(model.full.tmb, model.no.season, model.no.sp, model.no.height)

##best model has height, and species.

summary(model.no.season)
anova(model.no.season, model.full.tmb)

model.full.full <- glmmTMB(
  image.detected ~ scale(ground.height) + scale(ca.d) + sp + (1 | days.after),
  data = master.clean,
  family = binomial
)
summary(model.full.full)

model.ca.noheight <- glmmTMB(
  image.detected ~ scale(ca.d) + sp + (1 | days.after),
  data = master.clean,
  family = binomial
)
summary(model.ca.noheight)
AIC(model.full.full, model.ca.noheight)

model.no.random<- glmmTMB(
  image.detected ~ scale(ground.height) + sp + season,
  data = master.clean,
  family = binomial
)
summary(model.no.random)

##---------------------------------------------------
## Model prediction
##---------------------------------------------------
new.dat <- expand.grid(
    ground.height     = seq(min(master.clean$ground.height),
                max(master.clean$ground.height),
                length.out = 50),
    sp = levels(master.clean$sp)
)

new.dat$pred <- predict(model.no.season,
                        newdata = new.dat,
                        type = "response",
                        re.form = NA)
