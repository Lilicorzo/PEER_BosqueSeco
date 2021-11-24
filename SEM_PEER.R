library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(dslabs)
library(viridis)
library(ggExtra)
library(ggridges)
library(sf)
library(lavaan)
library(lavaanPlot)
library(tidyr)
library(knitr)
library(mvnormalTest)
library("PerformanceAnalytics")
library(MASS)
library("blavaan")
library("semPlot")
library("qgraph")
library("nloptr")


BASE_SEM_NORM <- read_csv("dat/BASE_SEM_NORM.csv")


## Modelo 12----
#El modelo 12  se compone de dos variables latentes (Prácticas y Riesgo) conformadas 
#por tres variables exógenas cada una, y 8 variables exógenas adicionales 
#y una variable endógena (SCI 2019)


model12 <- '
  # Variables latentes
    Practicas =~ P_S11P124_Fnatural + P_S11P125_No_protegen + P_S11P125_Prac_Cons_Rest
    Riesgo =~ P_S6P71_Enfermedad + P_S6P71_Plaga + P_S6P71_Inundacion
     

  # regressions
    Practicas ~ LHFI00_MEAN + pend_mean + Bosq_Nat
    SCI_19_Mean ~ Practicas +  P_S6P61_SP_Autoconsumo + P_S7P84A_Total_hembras
    SCI_19_Mean ~ G_VPrecipitacionQuanCvW_sigma + pend_mean
    SCI_19_Mean ~ LHFI00_MEAN + Riesgo
    SCI_19_Mean ~ P_S6P70_No_Sist_Riego
    P_S6P70_No_Sist_Riego ~ Bosq_Nat + G_VPrecipitacionQuanCvW_sigma  
    
  # Covarianzas
    Practicas ~~ Riesgo
    
'
fit12 <- sem(model = model12, data = BASE_SEM_NORM, meanstructure=TRUE)
summary(fit12, fit.measures =TRUE, standardized = TRUE, rsquare=TRUE) 

modindices(fit12, sort = TRUE, maximum.number = 10)

semPaths(fit12,"std", "est",fade = FALSE, rotation = 2, theme= "colorblind", layout = "tree3",intercepts = FALSE, residuals = FALSE, thresholds = FALSE, equalizeManifests = TRUE, 
         optimizeLatRes = TRUE, curvePivot = TRUE)

## Modelo 15----
#El modelos 15 esta compuesto de dos variables latentes (Practicas y Riesgo), 
#7 variables exógenas y una variable endógena (Diferencia entre SCI2019 y SCI2014) 

model15 <- '
# Variables latentes
    Practicas =~ P_S11P124_Fnatural + P_S11P125_No_protegen + P_S11P125_Prac_Cons_Rest
    Riesgo =~ P_S6P71_Enfermedad + P_S6P71_Plaga + P_S6P71_Inundacion
     

  # regressions
    Practicas ~ LHFI00_MEAN + Bosq_Nat 
    DIF_SCI ~ Practicas +  P_S6P61_SP_Autoconsumo + P_S7P83A_Total_machos
    DIF_SCI ~ G_VPrecipitacionQuanCvW_sigma + pend_mean
    DIF_SCI ~ LHFI00_MEAN + Riesgo
    DIF_SCI ~ P_S6P70_No_Sist_Riego
    P_S6P70_No_Sist_Riego ~ Bosq_Nat + G_VPrecipitacionQuanCvW_sigma  
    
  # Covarianzas
    Practicas ~~ Riesgo
    
'
fit15 <- sem(model = model15, data = BASE_SEM_NORM, meanstructure=TRUE)
summary(fit15, fit.measures =TRUE, standardized = TRUE) 

modindices(fit15, sort = TRUE, maximum.number = 10)

semPaths(fit15,"std", "est",fade = FALSE, rotation = 2, theme= "colorblind", layout = "tree3",intercepts = FALSE, residuals = FALSE, thresholds = FALSE, equalizeManifests = TRUE, 
         optimizeLatRes = TRUE, curvePivot = TRUE) 


