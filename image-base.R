##-----------------------------------------------------
## Packages
##
##-----------------------------------------------------
if(!require(lubridate)){install.packages("lubridate")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(dplyr)){install.packages("dplyr")}

library(lubridate)
library(ggplot2)
library(dplyr)

##-----------------------------------------------------
## Read data into R
## and clean some data points that are not
### detected since the first census
##-----------------------------------------------------
master.data <- read.csv("image_base.csv", header = T,
                        na.strings = ".")

master.data$season <- as.factor(master.data$season)
master.data$sp <- as.factor(master.data$sp)
master.data$bench <- as.factor(master.data$bench)
master.data$season <- as.factor(master.data$season)
master.data$monitor.date <- as.Date(master.data$monitor.date,
                                    format = "%m/%d/%Y")

## extract row with the first census ground data is missing
zero.ground1 <- c(which(master.data$ground.detect == 0 &
                          master.data$days.after == 40))

## show the order and sp ID of the missing value in the frist census
master.data[c(which(master.data$ground.detect == 0 &
                      master.data$days.after == 40)), 2:3]

## remove the row of the order and sp that have missing value
columns_to_combine <- c("order", "sp")



library(lme4)

model <- glmer(
  detected ~ height_m + crown_diam_m + species + season +
    (1 | plot_id/tree_id),
  data = tree_data,
  family = binomial(link = "logit")
)
