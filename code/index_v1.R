# Index building

library(openxlsx)
library(dplyr)
library(rstudioapi)   

# Nice way of getting the active directory
#directory <-  rstudioapi::getActiveDocumentContext()$path
directory <-  "C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms"

EDI_dir <- paste0(directory, '/data/ICIP_V22.xlsx')

# First dimension
EDI <- read.xlsx(xlsxFile = EDI_dir, sheet = 1,fillMergedCells = T)

