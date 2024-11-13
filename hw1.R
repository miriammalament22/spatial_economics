#------------------------------------------------------------------------------#
# Universidad de San Andrés
# Master's in Economics
# Spatial Econometrics - Homework 1
# 3rd Trimester 2023
# Students: Miriam Malament
# Professor: Marcos Herrera-Gomez
#------------------------------------------------------------------------------#

#### Library Import and Data Preparation ####
# Loading necessary libraries for data manipulation, statistical analysis, and visualization
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)        # Spatial data manipulation
library(spdep)     # Spatial autocorrelation analysis
library(geosphere) # Distance calculations
library(sp)        # Spatial data handling

# Set working directory
setwd("~/Desktop/UdeSA/Optativas/3T/Econometría Espacial/spacial/HW1")

# Load data from Shapefile
shapefile_path1 <- "localesventa18posgar6.shp"
geodata1 <- st_read(shapefile_path1)

# Set desired CRS (Coordinate Reference System)
crs_target <- st_crs(5348)  # POSGAR 2017 - Argentina 6

# Convert to sf object and define CRS
df <- st_as_sf(geodata1, coords = c("LONGITUD", "LATITUD"), crs = 4326)  
df <- st_transform(df, crs = 5348) 

#### Exercise 1: Hedonic Model ####

# Basic summary statistics for variables
summary(df)

# Price Distribution Visualization
ggplot(df, aes(x = PRECIOUSD)) + 
  geom_histogram(binwidth = 50000, fill = "darkblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Commercial Property Prices in USD",
    x = "Price (USD)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))

# Create dependent variable as the natural logarithm of price in USD
df <- df %>% mutate(ln_PRECIOUSD = log(PRECIOUSD))

# Specify and fit the hedonic model
model <- lm(ln_PRECIOUSD ~ ANTIG_ + M2TOTAL + AMBIENTES + BANOS, data = df)
summary(model)

# Model summary table in LaTeX format (optional)
stargazer::stargazer(model, type = "latex")

#### Exercise 2: Spatial Analysis and Autocorrelation ####

# Create second-order spatial weights matrix
W2_cont <- nb2listw(knn2nb(knearneigh(st_coordinates(df), k = 2)))

# Calculate Moran's I for ln_PRECIOUSD
moran_ln_PRECIOUSD <- moran.test(df$ln_PRECIOUSD, listw = W2_cont) 
print(moran_ln_PRECIOUSD)

# Moran's scatter plot for ln_PRECIOUSD
moran.plot(df$ln_PRECIOUSD, listw = W2_cont, labels = FALSE)

# Calculate Moran's I for model residuals
df$residuals <- residuals(model)
moran_residuals <- moran.test(df$residuals, listw = W2_cont)
print(moran_residuals)

# Moran's scatter plot for residuals
moran.plot(df$residuals, listw = W2_cont, labels = FALSE)

#### Exercise 3: Additional Spatial Variables ####

# Load additional spatial datasets
shapefile_path2 <- "comisariasposgar6.shp"
comisarias <- st_read(shapefile_path2)
robos <- read_csv("robo_4_17.csv")
robos_sf <- st_as_sf(robos, coords = c("LONGITUD", "LATITUD"), crs = 4326)

# Transform CRS to WGS 84 for both datasets
df_wgs84 <- st_transform(df, crs = 4326)

# Calculate number of robberies within 500m buffer
buffers <- st_buffer(df_wgs84, dist = 500)
df_wgs84$robos_cercanos <- lengths(st_intersects(buffers, robos_sf))

# Calculate distance to the nearest police station
distances <- st_distance(df_wgs84, comisarias, by_element = TRUE)
df_wgs84$dist_comisaria <- distances

# Calculate distance to the Obelisco
obelisco_coords <- c(-58.3816, -34.6037)
coords <- st_coordinates(df_wgs84)
df_wgs84 <- df_wgs84 %>% 
  mutate(dist_obelisco = distm(cbind(coords[, "X"], coords[, "Y"]), obelisco_coords, fun = distVincentyEllipsoid))

#### Descriptive Statistics for Spatial Variables ####

# Prepare summary statistics
df_regular <- as.data.frame(df_wgs84)
stats <- df_regular %>% select(robos_cercanos, dist_comisaria, dist_obelisco) %>%
  summarise(across(everything(), list(
    mean = ~mean(.x, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  )))
print(stats)

# Correlation with dependent variable
correlations <- cor(df_regular %>% select(robos_cercanos, dist_comisaria, dist_obelisco, ln_PRECIOUSD), use = "complete.obs")
print(correlations)
