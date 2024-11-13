# Spatial Economics Project Repository

## Overview
This repository hosts the projects I am working on as part of my exploration into spatial econometrics. These projects aim to apply and enhance my understanding of econometric models to spatial data, focusing on retail space pricing and unidentified statistical processes.

## Projects

### Project 1: Spatial-Contextual Effects on Retail Space Sale Prices

#### Description
This project integrates spatial-contextual variables into a hedonic model for retail space sale prices in Buenos Aires (CABA). The data is derived from the Buenos Aires Data website and includes various characteristics of retail spaces available for sale in 2018.

#### Variables:
- **Antig**: Age of the retail space in years.
- **M2total**: Total square meters.
- **M2cub**: Total covered square meters.
- **Ambientes**: Number of rooms.
- **Banos**: Number of bathrooms.
- **PrecioUSD**: Price in US dollars.
- **PrecioARS**: Price in Argentine pesos.
- **Barrio**
- **Comuna**

#### Objectives:
1. Create a dependent variable as the natural logarithm of the US dollar price and specify a hedonic model based on the characteristics of the retail spaces.
2. Analyze spatial autocorrelation using a second-order contiguity matrix and Moran's I test for both the dependent variable and model residuals.
3. Calculate spatial variables such as crime rates in the vicinity, distances to the nearest police station, and distances to the Obelisco.
4. Re-estimate the hedonic model by incorporating the calculated spatial variables.
5. Include neighborhood fixed effects and spatial outliers to refine the model accuracy.

### Project 2: Identifying the "Mystery Process"

#### Description
This project focuses on specifying the most appropriate model for data contained within the "mystery_process.mat" file. I explore different model specification strategies to understand and identify the underlying statistical process.

#### Software:
- MATLAB
- R (with provided script)

#### Objectives:
1. Apply maximum likelihood estimation using strategies from specific to general and vice versa.
2. Document and explain each step leading to the final model selection.

## Tools and Software
- I use GeoDa, R, and Stata for spatial econometric analysis.
- Spatial data is projected using POSGAR 2017 - 6 (EPSG: 5348).

## Additional Resources
Materials on spatial econometrics from various courses and seminars are utilized to support the projects contained in this repository.
