library(tidyverse)
library(timetk)
library(palmerpenguins)

glimpse(bike_sharing_daily)
str(bike_sharing_daily)

bike <- bike_sharing_daily
bike$season <- c("Winter", "Spring", "Summer", "Fall") [bike$season]
bike$season <- factor(bike$season, levels = c("Winter", "Spring", "Summer", "Fall"))

bike$mnth <- factor(bike$mnth)

bike$holiday <- factor(c("No", "Yes")[bike$holiday+1])
bike$workingday <- factor(c("No", "Yes")[bike$workingday+1])

bike$yr <- factor(c("2011", "2012")[bike$yr+1])

bike$weathersit <- c("clear", "cloudy", "lightP", "heavyP")[bike$weathersit]
bike$weathersit <- factor(bike$weathersit, levels = c("clear", "cloudy", "lightP", "heavyP"))

glimpse(bike)

dataReporter::makeDataReport(bike, replace=TRUE)

table(bike$weathersit)

bike <- droplevels(bike)

table(bike$weathersit)

##### GGPLOT@ #####

ggplot(data = bike, mapping = aes(x = cnt)) + geom_histogram()

ggplot(data = bike, mapping = aes(x=cnt)) + geom_histogram(fill="lightpink", color="white")

ggplot(data = bike, mapping = aes(x=cnt, fill=yr)) + geom_histogram(color="white")

ggplot(data = bike, mapping = aes(x = weathersit)) + geom_bar(fill="lightblue", color= "navy")

ggplot(data = bike, mapping = aes(x=weathersit, fill=yr)) + geom_bar(color="black")

ggplot(data = bike, mapping = aes(x=weathersit, fill=yr)) +geom_bar(position = "fill")

ggplot(data = bike, mapping = aes(x=mnth,y=cnt)) + geom_col(color="blue", fill="white")

ggplot(data = bike, mapping = aes(x=mnth, y=cnt, fill=yr)) + geom_col()

ggplot(data = bike, aes(x=temp, y=cnt)) + geom_point()
ggplot(data = bike, mapping = aes(x=temp, y=cnt)) + geom_point()

ggplot(data = bike, aes(x=temp, y=cnt, color=yr)) + geom_point()

ggplot(data = bike,aes(x=temp, y=cnt, color=yr, shape=season)) + geom_point()

ggplot(data = bike, aes(x=temp, y=cnt, color=weekday)) + geom_point()

bike$weekday <- factor(bike$weekday)
ggplot(data = bike, aes(x=temp, y=cnt, color=weekday)) + geom_point()


ggplot(data = bike, aes(x=dteday, y=cnt)) + geom_line()

library(lubridate)
ggplot(data=bike, aes(x=yday(dteday), y=cnt, color=yr)) + geom_line()

###### Practice 1 Penguins######

library(palmerpenguins)
view(penguins)
peng <- penguins

ggplot(data = peng, mapping = aes(x=island , fill = species)) + geom_bar()

ggplot(data = peng, mapping = aes(x= bill_length_mm , y= bill_depth_mm , color= species,shape= species)) + geom_point()

####### 
ggplot(data = bike, aes(x=yday(dteday), y=cnt, color=yr)) + geom_line()+ scale_x_continuous(breaks = cumsum(c(1,31,28,31,30,31,30,31,31,30,31,30)),
  labels = month.abb[1:12]) + xlab("Date")

ggplot(data = bike, aes(x=temp, y=cnt, color=yr)) + geom_point() + facet_wrap(~season)

ggplot(data = bike, aes(x=temp, y=cnt)) +geom_point() + geom_smooth()

(p<- ggplot(data = bike, aes(x=temp, y=cnt)) +geom_point() )
p+geom_smooth(method = "lm", se=TRUE)

ggplot(data = bike, aes(x=temp, y=cnt, color=yr)) +geom_point() +geom_smooth(se=FALSE)

ggplot(data = bike, aes(x=temp, y=cnt, color=yr)) +geom_point() + facet_grid(workingday ~season)

ggplot(data = bike, aes(x=temp, y=cnt , color=yr)) +geom_point() +facet_grid(workingday ~ season)+
    geom_smooth(se=FALSE)

fit <- lm(cnt ~ temp + season + yr , data = bike)
bike %>%
  mutate(fit = fitted(fit)) %>%
  ggplot(aes(x=temp, y=cnt, color=yr)) + 
  geom_point(size=1) + 
  facet_grid(workingday ~ season)+
  geom_smooth(method = lm, se= FALSE) + 
  geom_line(aes(y=fit), linetype="twodash", size=1.5)+
  theme_bw()

######Practice 2 Penguins#####
ggplot(data = peng, aes(x=bill_length_mm, y=bill_depth_mm, color=sex))+
  geom_point()+facet_wrap(~species)

dataReporter::makeDataReport(peng, replace=TRUE)

fit1 <- lm(bill_depth_mm ~species+sex+bill_length_mm, data = peng)

peng %>% na.omit(.) %>% 
  mutate(fit1 =fitted(fit1)) %>% 
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm, color=sex))+
  geom_point(size=1)+
  facet_wrap(~species)+
  geom_smooth(method=lm, se=FALSE)+
  geom_line(aes(y=fit1),linetype="twodash", size=1.5)

ggplot(data = bike, aes(x=cnt, fill=yr)) + geom_density

###############

ggplot(data = bike, aes(x=cnt)) + geom_density(color="red")+
  geom_histogram(aes(y=after_stat(density)),fill="lightblue",color="navy", alpha=.5)

ggplot(data = bike,aes(x=cnt, fill=yr)) + geom_density(alpha=.5)+theme_classic()

ggplot(data = bike, aes(y=cnt, x=season)) +
  geom_boxplot(varwidth = TRUE)

ggplot(data = bike, aes(y=cnt, x=0)) + geom_boxplot()

ggplot(data = bike, aes(y=cnt, x=season)) + geom_violin(fill="pink")
