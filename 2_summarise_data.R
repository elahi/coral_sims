################################################################################
##' @title Summarise simulated time series data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(broom)

sim_df <- read.csv("output_sims/sim_df.csv") %>% tbl_df() %>% select(-X)

# RELEVEL SCENARIOS
# sim_df <- sim_df %>%
#   mutate(scenario = factor(scenario,
#                            levels = c("Stable", "Linear",
#                                       "Phase_shift", "Oscillations")))

# How many simulations total?
n_sims = 400
i = 1
i_vec <- rep(i,30)
for(i in 2:n_sims){
  i_new <- rep(i, 30)
  i_vec <- c(i_vec, i_new)
}

sim_df$sim_total <- i_vec

##### CALCULATE SUMMARY STATISTICS #####

grand_means <- sim_df %>% 
  group_by(sim_total, scenario, sim) %>% 
  summarise(mean = mean(y), 
            median = median(y), 
            sd = sd(y), 
            n = n(), 
            cv = sd/mean * 100) %>% ungroup()

grand_means <- grand_means %>% 
  mutate(mean_z = scale(mean)[,1], 
         med_z = scale(median)[,1], 
         cv_z = scale(cv)[,1])

##### GET MEAN COVER #####
## I need to know the mean coral cover for each time-location
yr_means <- sim_df %>% group_by(year, scenario) %>% 
  summarise(yr_mean = mean(y)) %>% ungroup()
  
# Attach to raw data
sim_df <- left_join(sim_df, yr_means, by = c("year", "scenario"))

## Attach oasis results to raw data
sim_df <- grand_means %>% 
  select(sim, scenario, cv, med_z, mean_z) %>% 
  left_join(sim_df, ., c("sim", "scenario")) %>% 
  mutate(sim2 = reorder(sim, desc(mean_z)))

## Get mean cover
mean_cover_df <- sim_df %>% 
  group_by(year) %>%
  summarise(y_mean = mean(y))

