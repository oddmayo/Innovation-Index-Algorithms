require(readxl)
library(stringi)
# Work directory
directory <- 'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

# Data
EDI <- readxl::read_excel(paste0(directory, 'EDI_Entidad_2018.xlsx'),col_names = T)

# Special case
G02 <- data.frame(CODENT = EDI$CODENT, G02 = EDI$G02)

# EDI data set to work with
EDI$G02 <- NULL

# Replace all '8' and '9' for NAs
EDI[EDI==8] <- NA
EDI[EDI==9] <- NA

# Mean per group
EDI_means <- aggregate(EDI[, 1:ncol(EDI)], list(EDI$CODENT), mean, na.rm = T)

# G02 case
library(dplyr)

G02_counts <-  G02 %>% 
  group_by(CODENT, G02) %>%
  tally()

library(tidyr)

G02_sums <-  xtabs(n~., G02_counts)

# dplyr solution with data frame output
G02_sums <-  spread(G02_counts, G02, n, fill=0)

library(openxlsx)
# Save EDI_means as xlsx
openxlsx::write.xlsx(x = EDI_means, file = paste0(directory, "output/EDI_promedios.xlsx"))

# Save G02_sums as xlsx
openxlsx::write.xlsx(x = G02_sums, file = paste0(directory, "output/G02_sumas.xlsx"))
