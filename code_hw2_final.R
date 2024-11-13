#------------------------------------------------------------------------------#
# Universidad de San Andrés
# Maestría en Economía
# Econometría Espacial
# 2023, 3er trimestre
# Alumnos: Miriam Malament
# Profesor: Marcos Herrera-Gomez
#------------------------------------------------------------------------------#

#### Preparación de Datos ####

# Cargar librerías necesarias para manipulación de datos, análisis estadístico y visualización
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(sf)  
library(spdep)  
library(geosphere)  
library(sp) 
library(R.matlab)

# Working Directory
setwd("~/Desktop/UdeSA/Optativas/3T/Econometría Espacial/spacial/HW2")

library(R.matlab)
data <- readMat("mystery_process.mat")
names(data)

w <- data$W
class(w)

library(spdep)
w_lista <- mat2listw(w, style = "W")
summary(w_lista)

# Cargamos las otras librerias espaciales 
library(spatialreg)
library(rgdal)
options("rgdal_show_exportToProj4_warnings"="none")

### EJERCICIO 1: De lo particular a lo general ####

# Paso 1: Modelo OLS
reg.ols <- lm(y ~ x1 + x2, data=data)
summary(reg.ols)
stargazer::stargazer(reg.ols) # Convertir tabla a formato LaTeX

# Paso 2: Test de Moran para verificar la autocorrelación espacial
lm.morantest(reg.ols, w_lista, alternative = "two.sided")

# Paso 3: Pruebas LM para detectar autocorrelación espacial en los residuos
lms <- lm.LMtests(reg.ols, w_lista, test = "all")
tests <- t(sapply(lms, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tests) <- c("Test", "df", "p-valor")
printCoefmat(tests) 

# Paso 4: Modelo SLX (M6)
reg.slx <- lm(y ~ x1 + x2 + lag.listw(w_lista, x1) + lag.listw(w_lista, x2), data=data)
summary(reg.slx)
stargazer::stargazer(reg.slx)

# Paso 5: Repetir Test de Moran y pruebas LM para el modelo SLX
lm.morantest(reg.slx, w_lista, alternative = "two.sided")
lms <- lm.LMtests(reg.slx, w_lista, test = "all")
tests <- t(sapply(lms, function(x) c(x$statistic, x$parameter, x$p.value)))
colnames(tests) <- c("Test", "df", "p-valor")
printCoefmat(tests)

# Paso 6: Continuar con modelos más complejos como SDEM y SARAR
# Modelo SDEM (M4)
reg.sdem <- spatialreg::errorsarlm(y ~ x1 + x2, Durbin=~x1 + x2, data=data, w_lista, etype = "emixed")
summary(reg.sdem)
(LR_lambda1=2*(reg.sdem$LL - logLik(reg.slx)))

# Modelo SARAR
reg.sarar <- spatialreg::sacsarlm(y ~ x1 + x2, data=data, w_lista)
summary(reg.sarar)
(LR_rho2=2*(reg.sarar$LL - reg.sdem$LL))

# Comparar AIC y BIC entre modelos
aic_seleccionados <- c(AIC(reg.sdem), AIC(reg.sarar))
bic_seleccionados <- c(BIC(reg.sdem), BIC(reg.sarar))
nombres_modelos <- c("SDEM", "SARAR")
data.frame(Modelo = nombres_modelos, AIC = aic_seleccionados, BIC = bic_seleccionados)

# Modelo elegido: SDEM

### EJERCICIO 2: De lo general a lo particular ####

# Estimamos el SEM Y SLM para las comparaciones

# Modelo SEM
reg.sem <- spatialreg::errorsarlm(y ~ x1 + x2, data=data, w_lista)
summary(reg.sem)
(LR_lambda_sem=2*(reg.sem$LL - logLik(reg.ols)))

# Modelo SLM
reg.slm <- spatialreg::lagsarlm(y ~ x1 + x2, data=data, w_lista)
summary(reg.slm)
(LR_rho_slm=2*(reg.slm$LL - logLik(reg.ols)))

# Modelo SDM
reg.sdm <- spatialreg::lagsarlm(y ~ x1  + x2, Durbin=~x1  + x2,data=data, w_lista, type = "mixed")
summary(reg.sdm)
(LR_rho1=2*(reg.sdm$LL - logLik(reg.slx1)))

# SDEM vs. SLX
LR.Sarlm(reg.sdem,reg.slx) #sdem

# SDEM VS. SEM
LR.Sarlm(reg.sdem,reg.sem) #sdem

# SDM VS. SEM 
LR.Sarlm(reg.sdm,reg.sem) #sdm

# 2. SDM VS. SLX
LR.Sarlm(reg.sdm,reg.slx)  #sdm

# SARAR vs. SLM
LR.Sarlm(reg.sarar,reg.slm) #sarar

# SARAR vs. SEM 
LR.Sarlm(reg.sarar,reg.sem) #sarar

# Comparar AIC y BIC entre modelos
aic_seleccionados <- c(AIC(reg.sdem), AIC(reg.sarar), AIC(reg.sdm))
bic_seleccionados <- c(BIC(reg.sdem), BIC(reg.sarar), BIC(reg.sdm))
nombres_modelos <- c("SDEM", "SARAR", "SDM")
data.frame(Modelo = nombres_modelos, AIC = aic_seleccionados, BIC = bic_seleccionados)
