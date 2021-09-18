install.packages(c("GGally", "ISLR2", "naniar"))
library(GGally)
library(ISLR2)
library(timetk)
library(palmerpenguins)
library(tidyverse)
library(naniar)

bike <- bike_sharing_daily
bike$season <- c("Winter", "Spring", "Summer", "Fall")[bike$season]
bike$season <- factor(bike$season, levels = c("Winter", "Spring", "Summer", "Fall"))

bike$mnth <- factor(bike$mnth)

bike$holiday <- c("No", "Yes")[bike$holiday+1]
bike$holiday <- factor(bike$holiday, levels= c("No", "Yes"))

bike$workingday <- c("No", "Yes")[bike$workingday+1]
bike$workingday <- factor(bike$workingday, levels = c("No","Yes"))

bike$yr <- c("2011", "2012")[bike$yr+1]
bike$yr <- factor(bike$yr, levels = c("2011", "2012"))

bike$weathersit <- c("clear", "cloudy", "lightP", "heavyP")[bike$weathersit]
bike$weathersit <- factor(bike$weathersit, levels = c("clear", "cloudy", "lightP", "heavyP"))

bike <- droplevels(bike)

