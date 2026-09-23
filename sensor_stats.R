### Script to generate stats and visuals based on HOBO sensor inventory and testing

## Library necessary packages
library(pool)
library(DBI)
library(tidyverse)
library(lubridate)
library(dplyr)
library(scales)
library(ggplot2)

## Create database connection
poolConn <- dbPool(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_prod",
  user = Sys.getenv("mars_uid"),
  password = Sys.getenv("mars_pwd"),
  timezone = NULL
)


## Set aspect ratio and height for plots
aspect_ratio <- 1.5
plot_height_in <- 6

## Load inventory data

inventory <- dbGetQuery(poolConn, 'SELECT * FROM sensors.viw_sensor_current_status') 
active_deployments <- dbGetQuery(poolConn, 'SELECT * FROM fieldwork.viw_active_deployments')
all_deployments <- dbGetQuery(poolConn, 'SELECT * FROM fieldwork.viw_deployment_full')
sensor_model_lookup <- dbGetQuery(poolConn, 'SELECT * FROM sensors.tbl_sensor_model_lookup')
sensor_status_lookup <- dbGetQuery(poolConn, 'SELECT * FROM sensors.tbl_sensor_status_lookup')
inventory <- inventory %>%
  left_join(active_deployments, by = 'sensor_uid') %>%
  left_join(sensor_model_lookup, by = 'sensor_model_lookup_uid') %>%
  left_join(sensor_status_lookup, by = 'sensor_status_lookup_uid') %>%
  select(sensor_uid, date_purchased, sensor_model, sensor_status, smp_id)

## Update inventory table to include material and calibration depth 
sensor_model <- c('U20-001-01', 'U20-001-04', 'U20L-01', 'U20L-04')
material <- c('Stainless Steel', 'Stainless Steel', 'Plastic', 'Plastic')
calibration_depth <- c('30 ft', '13 ft', '30 ft', '13 ft')  
model_info_lookup <- data.frame(sensor_model, material, calibration_depth)
inventory <- inventory %>%
  left_join(model_info_lookup, by = 'sensor_model')

## Clean up inventory table
inventory <- inventory %>%
  # Remove non-existent sensor from inventory
  filter(sensor_uid != 915) %>%
  # Add column indicating whether or not a sensor is deployed
  mutate(deployed = !is.na(smp_id)) 

## Generate historical stats on sensors and deployments

# Print total sensors purchased
print(paste0('Total sensors purchased by MARS: ', nrow(inventory)))

# Print total number of deployments
# For these purposes, a "deployment" is a unique combination of sensor and location
print(paste0('Total number of deployments: ', nrow(distinct(all_deployments, ow_uid, sensor_serial))))

# Print number of deployment locations
print(paste0('Number of unique deployment locations: ', length(unique(all_deployments$ow_uid))))

# Print number of SMPs
# Note this doesn't count non-SMP locations
print(paste0('Number of unique deployment locations: ', length(unique(all_deployments$smp_id))))

## Generate current inventory tables

# Table 1: All non-disposed sensors, broken up by material and calibration depth
inventory %>%
  # Remove disposed sensors
  filter(sensor_status != 'Disposed') %>%
  # Group by material and calibration depth and display counts in 2-way table
  group_by(material, calibration_depth) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = calibration_depth, values_from = n)

# Table 2: All non-disposed sensors, broken up by status
inventory %>%
  # Remove disposed sensors
  filter(sensor_status != 'Disposed') %>%
  # Combine Good Order MARS & Good Order AKRF statuses
  mutate(sensor_status = case_when(
    ((sensor_status == 'Good Order- AKRF Custody' | sensor_status == 'Good Order- MARS Custody') & deployed == FALSE) ~ 'Good Order, Not Deployed',
    ((sensor_status == 'Good Order- AKRF Custody' | sensor_status == 'Good Order- MARS Custody') & deployed == TRUE) ~ 'Good Order, Deployed',
    TRUE ~ sensor_status
  )) %>%
  # Group by status and display counts
  group_by(sensor_status) %>%
  summarise(n=n()) 

## Generate test history table
test_history <- dbGetQuery(poolConn, 'select * from sensors.tbl_sensor_tests')
test_type_lookup <- dbGetQuery(poolConn, 'select * from sensors.tbl_sensor_test_type_lookup')
test_status_lookup <- dbGetQuery(poolConn, 'select * from sensors.tbl_sensor_test_status_lookup')
test_history <- test_history %>%
  left_join(inventory, by = 'sensor_uid') %>%
  left_join(test_type_lookup, by = 'test_type_lookup_uid') %>%
  left_join(test_status_lookup, by = 'sensor_test_status_lookup_uid') %>%
  select(test_date, sensor_uid, test_type, mean_error_ft, max_abs_error_ft, mean_error_psi, max_abs_error_psi,
         date_purchased, sensor_model, material, calibration_depth, test_type, test_status)

## Separate into level and baro tables
test_history_lvl <- test_history %>%
  filter(test_type == 'Level') %>%
  select(-test_type, -mean_error_psi, -max_abs_error_psi)
test_history_baro <- test_history %>%
  filter(test_type == 'Baro') %>%
  select(-test_type, -mean_error_ft, -max_abs_error_ft)

## Generate level test stats & visuals

# Calculate number of tests performed and passing rate
num_tests_lvl <- nrow(test_history_lvl)
num_sensors_tested_lvl <- nrow(distinct(test_history_lvl, sensor_uid))
num_passes_lvl <- nrow(filter(test_history_lvl, test_status == 'Pass'))
pass_percent_lvl <- percent(num_passes_lvl / num_tests_lvl)
print(paste0(num_tests_lvl, ' level tests performed on ', num_sensors_tested_lvl, 
             ' unique sensors with a passing rate of ', pass_percent_lvl))


## Flag outliers (can adjust thresholds as needed)
outlier_mag_lvl_mean <- 0.5 
outlier_mag_lvl_max <- 1 
test_history_lvl <- test_history_lvl %>%
  mutate(outlier_mean = (abs(mean_error_ft) > outlier_mag_lvl_mean),
         outlier_max = (max_abs_error_ft > outlier_mag_lvl_max))

## Make box plots of mean error

# Make box plots separated by material
mean_error_by_material_lvl <- ggplot(test_history_lvl, aes(x = material, y = mean_error_ft)) + 
  geom_boxplot() + 
  labs(title = 'Level Tests', subtitle = 'Mean Error by Sensor Material', y = 'Mean Error (ft)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) + 
  ylim(-outlier_mag_lvl_mean, outlier_mag_lvl_mean)
ggsave('output/mean_error_by_material_lvl.png', mean_error_by_material_lvl, 
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Make box plots separated by calibration depth
mean_error_by_cal_depth_lvl <- ggplot(test_history_lvl, aes(x = calibration_depth, y = mean_error_ft)) + 
  geom_boxplot() + 
  labs(title = 'Level Tests', subtitle = 'Mean Error by Calibration Depth', y = 'Mean Error (ft)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) +
  ylim(-outlier_mag_lvl_mean, outlier_mag_lvl_mean)
ggsave('output/mean_error_by_cal_depth_lvl.png', mean_error_by_cal_depth_lvl,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Print outlier info
outliers_lvl_mean <- filter(test_history_lvl, outlier_mean == TRUE)$mean_error_ft %>% sort
print(paste0('Excluded ', length(outliers_lvl_mean), ' outliers. Outlier values:'))
cat(outliers_lvl_mean, sep = '\n')

## Make box plots of max absolute error

# Make box plots separated by material
max_error_by_material_lvl <- ggplot(test_history_lvl, aes(x = material, y = max_abs_error_ft)) + 
  geom_boxplot() + 
  labs(title = 'Level Tests', subtitle = 'Max Absolute Error by Sensor Material', y = 'Max Absolute Error (ft)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) +
  ylim(0, outlier_mag_lvl_max)
ggsave('output/max_error_by_material_lvl.png', max_error_by_material_lvl,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Make box plots separated by calibration depth
max_error_by_cal_depth_lvl <- ggplot(test_history_lvl, aes(x = calibration_depth, y = max_abs_error_ft)) + 
  geom_boxplot() + 
  labs(title = 'Level Tests', subtitle = 'Max Absolute Error by Calibration Depth', y = 'Max Absolute Error (ft)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) + 
  ylim(0, outlier_mag_lvl_max)
ggsave('output/max_error_by_cal_depth_lvl.png', max_error_by_cal_depth_lvl,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Print outlier info
outliers_lvl_max <- filter(test_history_lvl, outlier_max == TRUE)$max_abs_error_ft %>% sort
print(paste0('Excluded ', length(outliers_lvl_max), ' outliers. Outlier values:'))
cat(outliers_lvl_max, sep = '\n')



## Generate baro test stats & visuals

# Calculate number of tests performed and passing rate
num_tests_baro <- nrow(test_history_baro)
num_sensors_tested_baro <- nrow(distinct(test_history_baro, sensor_uid))
num_passes_baro <- nrow(filter(test_history_baro, test_status == 'Pass'))
pass_percent_baro <- percent(num_passes_baro / num_tests_baro)
print(paste0(num_tests_baro, ' baro tests performed on ', num_sensors_tested_baro, 
             ' unique sensors with a passing rate of ', pass_percent_baro))


## Flag outliers (can adjust thresholds as needed)
outlier_mag_baro_mean <- 0.2 
outlier_mag_baro_max <- 0.25 
test_history_baro <- test_history_baro %>%
  mutate(outlier_mean = (abs(mean_error_psi) > outlier_mag_baro_mean),
         outlier_max = (max_abs_error_psi > outlier_mag_baro_max))

## Make box plots of mean error

# Make box plots separated by material
mean_error_by_material_baro <- ggplot(test_history_baro, aes(x = material, y = mean_error_psi)) + 
  geom_boxplot() + 
  labs(title = 'Baro Tests', subtitle = 'Mean Error by Sensor Material', y = 'Mean Error (psi)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) + 
  ylim(-outlier_mag_baro_mean, outlier_mag_baro_mean)
ggsave('output/mean_error_by_material_baro.png', mean_error_by_material_baro,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Make box plots separated by calibration depth
mean_error_by_cal_depth_baro <- ggplot(test_history_baro, aes(x = calibration_depth, y = mean_error_psi)) + 
  geom_boxplot() + 
  labs(title = 'Baro Tests', subtitle = 'Mean Error by Calibration Depth', y = 'Mean Error (psi)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) + 
  ylim(-outlier_mag_baro_mean, outlier_mag_baro_mean)
ggsave('output/mean_error_by_cal_depth_baro.png', mean_error_by_cal_depth_baro,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Print outlier info
outliers_baro_mean <- filter(test_history_baro, outlier_mean == TRUE)$mean_error_ft %>% sort
print(paste0('Excluded ', length(outliers_baro_mean), ' outliers. Outlier values:'))
cat(outliers_baro_mean, sep = '\n')

## Make box plots of max absolute error

# Make box plots separated by material
max_error_by_material_baro <- ggplot(test_history_baro, aes(x = material, y = max_abs_error_psi)) + 
  geom_boxplot() + 
  labs(title = 'Baro Tests', subtitle = 'Max Absolute Error by Sensor Material', y = 'Max Absolute Error (psi)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) + 
  ylim(0, outlier_mag_baro_max)
ggsave('output/max_error_by_material_baro.png', max_error_by_material_baro,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Make box plots separated by calibration depth
max_error_by_cal_depth_baro <- ggplot(test_history_baro, aes(x = calibration_depth, y = max_abs_error_psi)) + 
  geom_boxplot() + 
  labs(title = 'Baro Tests', subtitle = 'Max Absolute Error by Calibration Depth', y = 'Max Absolute Error (psi)') +
  theme(axis.title.x = element_blank(), aspect.ratio = aspect_ratio) + 
  ylim(0, outlier_mag_baro_max)
ggsave('output/max_error_by_cal_depth_baro.png', max_error_by_cal_depth_baro,
       height = plot_height_in, width = plot_height_in/aspect_ratio)

# Print outlier info
outliers_baro_max <- filter(test_history_baro, outlier_max == TRUE)$max_abs_error_ft %>% sort
print(paste0('Excluded ', length(outliers_baro_max), ' outliers. Outlier values:'))
cat(outliers_baro_max, sep = '\n')

