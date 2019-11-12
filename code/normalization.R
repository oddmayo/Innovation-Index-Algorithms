# Normalize EDI means

EDI <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/output/EDI_promedios.xlsx")


columns <- colnames(EDI)
columns <- columns[3:length(columns)]

range_1 <- function(x){
  n = 2-x
  return(n)
}

range_2 <-  function(x) {
  n = ( (x-1)/4 )
  return(n)
}

empty_df = EDI[,FALSE]
  
  for (column in columns) {
    temp_mean <-  mean(EDI[,column])
    if (temp_mean >= 1 & temp_mean < 2) {
      test <- lapply(EDI[,column], range_1)
      empty_df[,column] <- unlist(test)
      
    } else if (temp_mean >= 2 & temp_mean <= 5) {
        test <- lapply(EDI[,column], range_2)
        empty_df[,column] <- unlist(test)
      }
  }

  

