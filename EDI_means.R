require(readxl)
library(stringi)
# Work directory
directory <- 'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

# Data
EDI <- readxl::read_excel(paste0(directory, 'EDI_Entidad_2018.xlsx'),col_names = T)

# Special case
G02 <- EDI$G02

# EDI data set to work with
EDI$G02 <- NULL

# Mean per group
EDI_means <- aggregate(d[, 3:4], list(d$Name), mean)
