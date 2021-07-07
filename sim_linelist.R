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



linelist <- linelist_generator(expand_biters,numGens=1, R0=2, mean_inc=8, mean_latent=8, mean_infectious=5)

print(linelist, n=Inf)
