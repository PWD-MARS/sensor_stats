### Script to create timeseries plots based on HOBO sensor inventory and testing

## Library necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggplot2)
library(readxl)

## Helper functions to read a QAQC sheet, correct timezone, and filter based on start/end times
read_qaqc_sheet_lvl <- function(file_name, sheet_name) {
  sheet_data <- read_excel(paste0('data/', file_name), sheet = sheet_name, skip = 1) %>%
    # Select dtime and water level only
    select(dtime = 'Standard Dtime', wl_ft = 'Corrected Water Depth (ft)') %>%
    # Set time zone to UTC-5
    mutate(dtime = force_tz(dtime, tz = tz)) %>%
    # Discard data from before/after monitoring period
    filter(between(dtime, start_dt, end_dt))
  return(sheet_data)
}

read_qaqc_sheet_baro <- function(file_name, sheet_name) {
  sheet_data <- read_excel(paste0('data/', file_name), sheet = sheet_name, skip = 1) %>%
    # Select dtime and water level only
    select(dtime = 'Standard Dtime', abs_pres_psi = 'Abs Pres Baro (psi)') %>%
    # Set time zone to UTC-5
    mutate(dtime = force_tz(dtime, tz = tz)) %>%
    # Discard data from before/after monitoring period
    filter(between(dtime, start_dt, end_dt))
  return(sheet_data)
}

## Helper function for generating plot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# Plot 1: 2026-07-30 level test

## Set Parameters
tz <- 'Etc/GMT+5'
start_dt <- ymd_hms('2026-07-30 14:05:00', tz = tz)
end_dt <- ymd_hms('2026-08-01 14:05:00', tz = tz)
start_wl <- 12.125/12
end_wl <- 12.125/12
file_name <- 'HOBO_Drift_Test_20260730_AR.xlsx'
num_files <- 9

## Create dataframe with only dtime
test_data <- data.frame(
  dtime = seq(
    from = start_dt,
    to = end_dt,
    by = '5 mins'
  )
)

## Import level data from each sheet
for (i in 1:num_files) {
  # Set sheet name
  sheet_name <- paste0('LVL ', i, ' Data')
  # Set column name
  col_name <- paste0('LVL_', i)
  # Import data as data_i
  data_i <- read_qaqc_sheet_lvl(file_name, sheet_name)
  # Rename wl col
  names(data_i)[2] <- col_name
  test_data <- test_data %>%
    left_join(data_i, by = 'dtime')
}

## Add measured water level
measured_wl <- seq(
  from = start_wl,
  to = end_wl,
  length.out = nrow(test_data)
)
test_data <- test_data %>%
  mutate(Measured = measured_wl)

## Reshape monitoring data
reshaped_data <- test_data %>%
  pivot_longer(-dtime, names_to = "source", values_to = "water_level_ft")

## Create color scale for plot
sensors = sort(setdiff(unique(reshaped_data$source), "Measured"))
sensor_colors = gg_color_hue(length(sensors))
names(sensor_colors) = sensors
all_colors = c(sensor_colors, c(Measured="black"))

## Create wl plot
wl_ts <-
  ggplot(reshaped_data,
         aes(x = dtime, y = water_level_ft, color = source)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = all_colors) +  
  ylab("Water Level (ft)") +
  labs(title = paste0('Level Test Beginning ', as.character(date(start_dt)))) + 
  labs(color = "Source") + 
  theme(axis.title.x = element_blank())

## Save wl plot
ggsave(paste0('output/wl_plot_', as.character(date(start_dt)), '.png'), wl_ts)

