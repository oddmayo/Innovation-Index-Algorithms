library(openxlsx)
library(data.table)
library(dplyr)

# Data from governorates
EDI_g <- fread("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/data/EDI_gobernaciones.csv",encoding = 'UTF-8',header = T,stringsAsFactors = F)
# Useless column
EDI_g$FEXPFINAL <- NULL

# Remove 8 and 9 answers (None of the above and so forth)
EDI_g[, 2:ncol(EDI_g)][EDI_g[, 2:ncol(EDI_g)] == 8] <- NA
EDI_g[, 2:ncol(EDI_g)][EDI_g[, 2:ncol(EDI_g)] == 9] <- NA

# Mean per group 
resultado <- aggregate(EDI_g[, 1:ncol(EDI_g)], list(EDI_g$CODENT), mean, na.rm = T)

# Useless column
resultado$Group.1 <- NULL

# Save data
write.xlsx(resultado,"C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/output/EDI_g_promedios.xlsx")

