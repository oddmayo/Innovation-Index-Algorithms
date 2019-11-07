# Normalize EDI means

EDI <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/output/EDI_promedios.xlsx")


for (column in ncol(EDI)) {
  if (mean(EDI$column)) {
    
  }
}

columns <- colnames(EDI)
columns <- columns[3:length(columns)]

for (column in columns) {
  temp_mean <-  mean(EDI[,column])
  if (with(temp_mean, temp_mean >= 1 & temp_mean <2) ) {
    temp_mean
    }
}

 

with(temp_mean, temp_mean >= 1 & temp_mean <2)
