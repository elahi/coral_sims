################################################################################
##' @title Functions to simulate time series of coral cover
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

library(dplyr)

##### Assign variables #####

# Mean coral cover
coral_cover = 40
# Standard deviation of coral cover
coral_cover_sd = 3
# Number of years
number_yrs = 30
# Linear trend
linear_trend = 0
# Standard deviation of trend
linear_trend_sd = 0
# Periodicity (years) [for a 30 yr time series, a period of 30 results in a U]
period_yrs = 35
# SD of periodicity (years) 
period_yrs_sd = 20
# Amplitude 
amp = 15
# Time
x = seq(1:number_yrs)
# Phase shift
phase_shift = 0 * pi # starting at top (cos curve)
#phase_shift = 0.5 * pi # sin curve, starting at middle
#phase_shift = 1 * pi # cos curve, starting at bottom

##### STABLE #####

##' cc = upper value of coral cover, used in normal or uniform distribution (40)
##' yrs = number of years in the time series (30)
##' rnorm_theta = probability of observing a zero or one (0.5)
##' (for the coin flip)
stable_simF <- function(cc = coral_cover, yrs = number_yrs, rnorm_theta = 0.5){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Get time series
  y = rnorm(number_yrs, mean = cc, sd = cc_sd)
  
  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = cc - y, slope = 0, intercept = cc, 
                       y = y)
  
  return(sim_df)
}

stable_simF()

##### PHASE SHIFT #####

##' cc = upper value of coral cover, used in normal or uniform distribution (40)
##' yrs = number of years in the time series (30)
##' rnorm_theta = probability of observing a zero or one (0.5)
##' (for the coin flip)
##' shift_min = the earliest year in which a phase shift can begin (10)
##' shift_max = the latest year in which a phase shift can begin (30)
##' 
phase_shift_simF <- function(cc = coral_cover, yrs = number_yrs, rnorm_theta = 0.5, 
                             shift_min = 10, shift_max = 30){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Get time series
  y = rnorm(number_yrs, mean = cc, sd = cc_sd)
  
  ## Now choose when the phase shift happens
  begin_shift = floor(runif(1, min = shift_min, max = shift_max))
  
  ## Get the new coral cover mean
  cc_new = runif(1, max = 0.5*cc, min = 1)
  
  ## Get the coral cover sd, similar to old sd
  cc_sd_new = rnorm(1, mean = cc_sd, sd = 0.1)
  
  # Change cc_sd_new if below some arbitrary threshold
  cc_sd_new = ifelse(cc_sd_new < 0.1, 0.1, cc_sd_new)
  
  ## Get phase shift time series
  y_new = rnorm(number_yrs, mean = cc_new, sd = cc_sd_new)
  
  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = cc - y, slope = 0, intercept = cc, 
                       y = y) %>% 
    mutate(y = ifelse(year < begin_shift, y, y_new))
  
  return(sim_df)
}

phase_shift_simF()

###### LINEAR TREND ######

##' cc = upper value of coral cover, used in normal or uniform distribution (40)
##' yrs = number of years in the time series (30)
##' rnorm_theta = probability of observing a zero or one (0.5)
##' (for the coin flip)
##' trend = slope of the linear trend (-0.5)
##' trend_sd = standard deviation for the trend (0.25)
##' 
linear_simF <- function(cc = coral_cover, yrs = number_yrs, 
                        trend = linear_trend, trend_sd = linear_trend_sd, 
                        rnorm_theta = 0.5){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Random variation for each time point
  w <- rnorm(number_yrs, mean = 0, sd = cc_sd) 
  ## Random value for slope
  slope <- rnorm(1, mean = trend, sd = trend_sd)
  ## Random value for intercept (i.e., the starting coral cover)
  intercept <- rnorm(1, cc, sd = cc_sd)
  
  ## Get time series
  y <- slope * x + intercept + w
  
  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = w, slope = slope, intercept = intercept, 
                       y = y)
  
  return(sim_df)
}

linear_simF()

###### CYCLES ######

##' cc = upper value of coral cover, used in normal or uniform distribution (30)
##' yrs = number of years in the time series (30)
##' rnorm_theta = probability of observing a zero or one (0.5)
##' (for the coin flip)
##' trend = slope of the linear trend (0)
##' trend_sd = standard deviation for the trend (0)
##' amp = amplitude
##' period (years) [for a 30 yr time series, a period of 30 results in a U] (40)
##'
non_linear_simF <- function(cc = coral_cover, yrs = number_yrs, 
                        trend = linear_trend, trend_sd = linear_trend_sd, 
                        rnorm_theta = 0.5, amplitude = amp){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Random variation for each time point
  w <- rnorm(number_yrs, mean = 0, sd = cc_sd) 
  ## Random value for slope
  slope <- rnorm(1, mean = trend, sd = trend_sd)
  ## Random value for intercept (i.e., the starting coral cover)
  intercept <- rnorm(1, cc, sd = cc_sd)
  
  # Phase shift
  phase_shift = runif(1, 0, 0.25) * pi
  
  # Amplitude
  amp = rnorm(1, mean = amp, sd = amp*0.5)
  
  # Period
  period = rnorm(1, mean = period_yrs, sd = period_yrs_sd)
  
  # Vector representing the nonlinear trend
  cs = amp * cos(2*pi*1:number_yrs/period + phase_shift)

  ## Get time series
  y <- slope * x + intercept + w + cs

  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = w, slope = slope, intercept = intercept, 
                       y = y)
  
  return(sim_df)
}

##' For the following cosine curve:
##' y = a * cos(bx + c)
##' a = amplitude (height)
##' b = period (length of one cycle of the curve)
##' if b = 1, we get the natural cycle of the cos curve, i.e., 2*pi
##' c = phase shift
##' if c = 0, the cosine curve starts at 'a'


non_linear_simF()
