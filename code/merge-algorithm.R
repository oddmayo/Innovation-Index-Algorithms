# PRIMER ALGORITMO

#install.packages("openxlsx")
#install.packages("tidyverse")
library(openxlsx)
library(tidyverse)

# Data
EDI <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/output/EDI_promedios.xlsx")
equi <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/data/equivalencias.xlsx")
pivote <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/data/pivote.xlsx")

# Unique columns
equi <- equi[ , !duplicated(colnames(equi))]

# Only compatible entities
equi <- equi %>% filter(Compatible == 1)

# Store first 3 rows since these are deleted after matching the codes
pivote_tres_filas <- pivote[1:3,]

# Leave only entities that appear in equi data
pivote_2 <- pivote %>% filter(pivote$X1 %in% equi$DAFP)

# Restore the 3 rows deleted
pivote_2 <- rbind(pivote_tres_filas,pivote_2)

# Leave EDI data present in equi data
EDI_2 <- EDI %>% filter(EDI$CODENT %in% equi$DANE)

# Order EDI data by equi data
EDI_2 <- EDI_2[ order(match(EDI_2$CODENT, equi$DANE)), ]

# Bind EDI means to equi data
base <- cbind(equi, EDI_2)

# Order base according to DAFP code in pivote - so it can be pasted to EDI
base <- base[ order(match(base$DAFP, pivote_2$X1)), ]

# Delete columns with irrelevant data
base <- base[,-c(2:6)]

# Delete leading column
base$DAFP <- NULL

# Add 3 empty rows to match row lenght
x <- rep(NA, ncol(base))

# Run this 3 times
base <- rbind(x, base)

# Final merge
resultado <- cbind(pivote_2, base)

# Export
#openxlsx::write.xlsx(x = resultado, file = "output/pivote_nuevo.xlsx")
