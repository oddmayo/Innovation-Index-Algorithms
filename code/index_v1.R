# Index building

library(openxlsx)
library(dplyr)
library(rstudioapi)   

# Nice way of getting the active directory
#directory <-  rstudioapi::getActiveDocumentContext()$path
directory <-  "C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms"

# We neede to look for the values in these columns
global <- read.xlsx(paste0(directory, '/output/pivote_nuevo.xlsx'))

# Delete first 3 rows
global <- global[-c(1:3),]

# Convert character types to numeric
global[] <- lapply(global, function(x) as.numeric(as.character(x)))

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
colums_temp <- global[,row_temp]
# Means of the group
if ( is.data.frame(colums_temp) ) {
  new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
} else {
  new_col_temp <- as.numeric(colums_temp)
  }
  


container <- c()
for (i in 1:nrow(nested_EDI_1)) {
  # Obtain row of questions
  row_temp <- unlist(strsplit(nested_EDI_1$qs[i],split = ';'))
  # Match column names of global to ieth row
  colums_temp <- global[,row_temp]
  # Means of the group
  if ( is.data.frame(colums_temp) ) {
    new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
  } else {
    new_col_temp <- as.numeric(colums_temp)
  }
  
  container[[i]] <- new_col_temp
}

new_nested_EDI_1 <- nested_EDI_1
new_nested_EDI_1$means <- container

new_nested_EDI_1$means[1]

