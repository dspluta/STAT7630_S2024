library(dplyr)
dat_raw <- read.csv("https://raw.githubusercontent.com/dspluta/STAT7630_S2024/main/Data/Real_estate_Data.csv")

dat_raw
dat <- dat_raw
colnames(dat) <- c("NO", "date", "age", "MRT_dist", "num_conv", "latitude", "longitude", "price")
dat <- dat %>% select(-NO)

head(dat)

# Question of Interest:
## How does date affect unit housing price, 
# adjusted for age of the house and location of the house?

# Proposed Model:
# PRICE ~ DATE + AGE + MRT_dist + latitude + longitude

# Exploratory Data Analysis

## Univariate Plots

hist(dat$date)
hist(dat$age)
hist(dat$MRT_dist, breaks = 50)
hist(dat$num_conv)
hist(dat$latitude)
hist(dat$longitude)
hist(dat$price)

## Bivariate Plots

plot(dat$date, dat$price)
plot(dat$age, dat$price)
plot(dat$MRT_dist, dat$price)
plot(dat$num_conv, dat$price)


