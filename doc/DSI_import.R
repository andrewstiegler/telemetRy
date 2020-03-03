## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(telemetRy)
library(magrittr)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  exported_data <- DSI_export_to_dataframe('path_to_DSI_export')

## -----------------------------------------------------------------------------
# Sample dataframe is included as sample_BP_data:
head(sample_BP_data)

## ----eval=TRUE----------------------------------------------------------------
# In our sample dataset, room lights turn on at 6AM
sample_typical_day <- typical_day(data = sample_BP_data, lights_on = 6, 
                                  progressbar = FALSE)
head(sample_typical_day)

## ----eval=TRUE----------------------------------------------------------------
# In our sample dataset, room lights turn on at 6AM
sample_circadian_avg <- circadian_avg(data = sample_BP_data, lights_on = 6)
head(sample_circadian_avg[[1]])

## ----eval=TRUE----------------------------------------------------------------
# To plot typical SBP. first isolate SBP
sample_typical_sbp <- isolate_typical(data = sample_typical_day, 
                                      parameter = "SBP")
# Create a column for mean SBP
sample_typical_sbp$mean <- sample_typical_sbp %>% dplyr::select(-Time) %>% 
  rowMeans()

# Plot the mean against time in hours
ggplot(data = sample_typical_sbp)+
  geom_point(aes(x=Time/3600, y=mean))+
  ylab("Mean SBP (mmHg)")+
  xlab("Time (h)")+
  theme_classic()

# The second list element contains values when room is dark
sample_circadian_dark <- sample_circadian_avg[[2]]
# Isolate SBP
sample_circadian_dark_sbp <- isolate_typical(data = sample_circadian_dark, 
                                             parameter = "SBP")
# Create a column for mean SBP
sample_circadian_dark_sbp$mean <- sample_circadian_dark %>% 
  dplyr::select(-Time) %>% rowMeans()

# Plot the mean against time in days
ggplot(data = sample_circadian_dark_sbp)+
  geom_line(aes(x=Time, y=mean))+
  ylab("Mean SBP 12H-Dark (mmHg)")+
  xlab("Time (d)")+
  theme_classic()


