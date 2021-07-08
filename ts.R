library(tidyverse);theme_set(theme_bw())

linelist 

## there are a few ways to tally up how many cases are observed 

## 1 using tables 

linelist$day


table(linelist$day)

## 2 histogram

hist(linelist$day)

## 3 tidyverse via summary

tsdata <- (linelist
  %>% group_by(day)
  %>% summarise(count = n())
)

print(tsdata)

ggts <- (ggplot(data=tsdata, aes(x=day,y=count))
  + geom_point()
)

print(ggts)
