# PRIMER ALGORITMO

#install.packages("openxlsx")
#install.packages("tidyverse")
library(openxlsx)
library(tidyverse)

# Cargar datos esenciales
EDI <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/output/EDI_promedios.xlsx")
equi <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/data/equivalencias.xlsx")
pivote <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/data/pivote.xlsx")

# Dejar columnas únicas
equi <- equi[ , !duplicated(colnames(equi))]

# Dejar solo las compatibles en la equivalencia
equi <- equi %>% filter(Compatible == 1)

#########################
# # NOT YET
# # Convertir primer fila en nombre de las colmnas
# colnames(pivote) <- as.character(unlist(pivote[1,]))
# pivote = pivote[-1, ]

# Gurdar 3 primeras filas de pivote, pues al buscar los códigos coincidentes
# se eliminan dichas filas

pivote_tres_filas <- pivote[1:3,]

# Dejar en el pivote solo aquellas entidades que aparecen
# en la tabla de equivalencias

pivote_2 <- pivote %>% filter(pivote$X1 %in% equi$DAFP)

# Pegar 3 filas a nuevo pivote
pivote_2 <- rbind(pivote_tres_filas,pivote_2)

# Dejar  los datos de EDI que se encuentran también en la tabla
# de equivalencias

EDI_2 <- EDI %>% filter(EDI$CODENT %in% equi$DANE)

# Ordenar las filas de EDI_2 de acuerdo al orden de las entidades en
# equivalencias

EDI_2 <- EDI_2[ order(match(EDI_2$CODENT, equi$DANE)), ]

# Pegar promedios de EDI a equivalencias
base <- cbind(equi, EDI_2)

# Ordenar base de acuerdo al orden del código DAFP en pivote para luego pegar EDI
base <- base[ order(match(base$DAFP, pivote_2$X1)), ]

# Eliminar columnas con info redundante o innecesaria
base <- base[,-c(2:6)]

# Eliminar columna guía
base$DAFP <- NULL

# Añadir 3 filas vacías a base para empatar con nuevo pivote
x <- rep(NA, ncol(base))

# Correr 3 veces esta línea
base <- rbind(x, base)

# Merge final
resultado <- cbind(pivote_2, base)

# Exportar
#openxlsx::write.xlsx(x = resultado, file = "output/pivote_nuevo.xlsx")
