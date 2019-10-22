# Index building

library(openxlsx)
library(dplyr)
library(rstudioapi)   

# Nice way of getting the active directory
#directory <-  rstudioapi::getActiveDocumentContext()$path
directory <-  "C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms"

# We neede to look for the values in these columns
EDI_global <- read.xlsx(paste0(directory, '/output/EDI_promedios.xlsx'))

# Data with nested groups, which will be averaged
EDI_dir <- paste0(directory, '/data/ICIP_V22.xlsx')

#-----------------#
# First dimension #
#-----------------#

EDI_1 <- read.xlsx(xlsxFile = EDI_dir, sheet = 1,fillMergedCells = T)

# Test
EDI_1$`#.pregunta.pivote` <-  gsub(pattern = '\n', '',EDI_1$`#.pregunta.pivote`)

# store each question with its code
nested_EDI_1 <- data.frame(code=EDI_1$Codigo,stringsAsFactors = F, qs=EDI_1$`#.pregunta.pivote`)

# Obtain row of questions
row_temp <- unlist(strsplit(nested_EDI_1$qs[1],split = ';'))
# Match column names of global to ieth row
colums_temp <- EDI_global[,temp]
# Means of the group
new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
