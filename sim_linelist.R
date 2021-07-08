library(dplyr)

source("linelist_fun.R")

set.seed(104)

R0 <- 2
initial_dates <- 1:1
daily_unknown_links <- rpois(length(initial_dates),lambda=2)

## unknown_links are left censored
## We assume these are all biters

unknown_links <- data.frame(day= initial_dates
                            , unknown_link_cases = daily_unknown_links
)

print(unknown_links)

### Expand the first generation of biters
expand_biters <- (unknown_links[rep(unknown_links$day,unknown_links$unknown_link_cases),]
  %>% transmute(day
    , biter_id = NA
    , id = as.character(1:nrow(.))
    , date_bitten = NA
    , biter_id = NA
    , num_bitee = NA
    , incubation = NA
    , latent = NA
    , infectious = NA
    , biter_infectious_wait = NA
    )
  %>% select(day, biter_id, id, date_bitten, num_bitee, incubation, latent, infectious, biter_infectious_wait)
)

print(expand_biters)



linelist <- linelist_generator(expand_biters,numGens=3, R0=2, mean_inc=8, mean_latent=8, mean_infectious=5)


print(linelist, n=Inf)

## creating infection time series 

tsdata <-(linelist
          %>% group_by(day)
          %>%summarise(count=n()))
print(tsdata)

library(tidyverse)

ggts <- ggplot(data = tsdata, aes(x = day, y = count), color=red) +
  geom_point(alpha = 0.7, color = "red")+
  labs(title = "Time series of number of dogs infected per day",
       x = "Dog bitten/Day",
       y = "Cases") 

print(ggts)

### We want to calculate what days the dogs are showing symptoms 

symptomes_linelist <- (linelist
  %>% mutate(symptom_date = day + incubation)                       
)

print(symptomes_linelist,n=Inf)

ts_symptoms <- (symptomes_linelist
  %>% group_by(symptom_date)
  %>% summarise(count = n())
  %>% filter(!is.na(symptom_date))
)

print(ggts
  + geom_point(data=ts_symptoms, aes(x=symptom_date,y=count)), color=black
)

