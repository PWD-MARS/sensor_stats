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
    # Set time zone to UTC-5 and handle rounding errors
    mutate(dtime = round_date(force_tz(dtime, tz = tz), 'minute')) %>%
    # Discard data from before/after monitoring period
    filter(between(dtime, start_dt, end_dt))
  return(sheet_data)
}

read_qaqc_sheet_baro <- function(file_name, sheet_name) {
  sheet_data <- read_excel(paste0('data/', file_name), sheet = sheet_name, skip = 1) %>%
    # Select dtime and water level only
    select(dtime = 'Standard Dtime', abs_pres_psi = 'Abs Pres Baro (psi)') %>%
    # Set time zone to UTC-5 and handle rounding errors
    mutate(dtime = round_date(force_tz(dtime, tz = tz), 'minute')) %>%
    # Discard data from before/after monitoring period
    filter(between(dtime, start_dt, end_dt))
  return(sheet_data)
}

## Helper function for generating plot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

## Function to plot level test and save plot

plot_lvl_test <- function(file_name, start_dt, end_dt, start_wl_ft, end_wl_ft, num_sensors = 9, tz = 'Etc/GMT+5', exclude = c()) {
  
  ## Create dataframe with only dtime
  test_data <- data.frame(
    dtime = seq(
      from = start_dt,
      to = end_dt,
      by = '5 mins'
    )
  )
  
  ## Import level data from each sheet
  for (i in 1:num_sensors) {
    # Only import if not an excluded sheet
    if ((i %in% exclude) == FALSE){
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
  }
  
  ## Add measured water level
  measured_wl_ft <- seq(
    from = start_wl_ft,
    to = end_wl_ft,
    length.out = nrow(test_data)
  )
  test_data <- test_data %>%
    mutate(Measured = measured_wl_ft)
  
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
  
}


## Function to plot baro test and save plot

plot_baro_test <- function(file_name, start_dt, end_dt, num_sensors = 9, tz = 'Etc/GMT+5', exclude = c()) {
  
  ## Create dataframe with only dtime
  test_data <- data.frame(
    dtime = seq(
      from = start_dt,
      to = end_dt,
      by = '5 mins'
    )
  )
  
  ## Import baro data from each sheet
  for (i in 1:num_sensors) {
    # Only import if not an excluded sheet
    if ((i %in% exclude) == FALSE){
      # Set sheet name
      sheet_name <- paste0('Baro ', i, ' Data')
      # Set column name
      col_name <- paste0('BARO_', i)
      # Import data as data_i
      data_i <- read_qaqc_sheet_baro(file_name, sheet_name) 
      # Rename abs pres col
      names(data_i)[2] <- col_name
      test_data <- test_data %>%
        left_join(data_i, by = 'dtime')
    }
  }
  
  ## Add baro_mean
  data_mean <- read_qaqc_sheet_baro(file_name, 'Baro Mean')
  mean <- data_mean$abs_pres_psi
  test_data <- test_data %>%
    mutate(Mean = mean)
  
  ## Reshape monitoring data
  reshaped_data <- test_data %>%
    pivot_longer(-dtime, names_to = "source", values_to = "abs_pres_psi")
  
  ## Create color scale for plot
  sensors = sort(setdiff(unique(reshaped_data$source), "Mean"))
  sensor_colors = gg_color_hue(length(sensors))
  names(sensor_colors) = sensors
  all_colors = c(sensor_colors, c(Mean="black"))
  
  ## Create baro plot
  baro_ts <-
    ggplot(reshaped_data,
           aes(x = dtime, y = abs_pres_psi, color = source)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = all_colors) +  
    ylab("Absolute Pressure (psi)") +
    labs(title = paste0('Baro Test Beginning ', as.character(date(start_dt)))) + 
    labs(color = "Source") + 
    theme(axis.title.x = element_blank())
  
  ## Save baro plot
  ggsave(paste0('output/baro_plot_', as.character(date(start_dt)), '.png'), baro_ts)
  
}



## Plot 1: 2026-07-30 level test

# Set Parameters
tz <- 'Etc/GMT+5'
file_name <- 'HOBO_Drift_Test_20260730_AR.xlsx'
start_dt <- ymd_hms('2026-07-30 14:05:00', tz = tz)
end_dt <- ymd_hms('2026-08-01 14:00:00', tz = tz)
start_wl_ft <- 12.125/12
end_wl_ft <- 12.125/12
num_sensors <- 9


plot_lvl_test(file_name, start_dt, end_dt, start_wl_ft, end_wl_ft, num_sensors = num_sensors, tz = 'Etc/GMT+5')


## Plot 2: 2025-04-22 level test

# Set Parameters
tz <- 'Etc/GMT+5'
file_name <- 'HOBO_Level_Test_Office_20250421_RJC.xlsx'
start_dt <- ymd_hms('2025-04-22 14:10:00', tz = tz)
end_dt <- ymd_hms('2025-05-01 12:50:00', tz = tz)
start_wl_ft <- 11.5/12
end_wl_ft <- 11.5/12
num_sensors <- 8
exclude <- c(4)

plot_lvl_test(file_name, start_dt, end_dt, start_wl_ft, end_wl_ft, num_sensors = num_sensors, tz = 'Etc/GMT+5', exclude = exclude)


## Plot 3: 2026-07-30 baro test

# Set Parameters
tz <- 'Etc/GMT+5'
file_name <- 'HOBO_Drift_Test_20260730_AR.xlsx'
start_dt <- ymd_hms('2026-07-30 14:05:00', tz = tz)
end_dt <- ymd_hms('2026-08-01 14:00:00', tz = tz)
num_sensors <- 9

plot_baro_test(file_name, start_dt, end_dt, num_sensors = num_sensors, tz = 'Etc/GMT+5')


## Plot 4: 2026-07-30 baro test

# Set Parameters
tz <- 'Etc/GMT+5'
file_name <- 'HOBO_Baro_Test_20231023_MA_new.xlsx'
start_dt <- ymd_hms('2023-10-23 17:00:00', tz = tz)
end_dt <- ymd_hms('2023-10-30 9:00:00', tz = tz)
num_sensors <- 8

plot_baro_test(file_name, start_dt, end_dt, num_sensors = num_sensors, tz = 'Etc/GMT+5')
