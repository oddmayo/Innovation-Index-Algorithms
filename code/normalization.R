# Normalize EDI means

# Working directory
directory <- 'C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

# Load EDI means
EDI <- openxlsx::read.xlsx("C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/output/EDI_promedios.xlsx")

# columns to iterate through loop
columns <- colnames(EDI)
columns <- columns[3:length(columns)]

# Case #1: if values are between 1 and 2 - [1,2)
range_1 <- function(x){
  n = 2-x
  return(n)
}

# Case #2: if values are between 2 and 5 - [2,5]
range_2 <-  function(x) {
  n = ( (x-1)/4 )
  return(n)
}

# Empty dataframe
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

library(tibble)
empty_df <- add_column(empty_df, EDI$CODENT, .before = 1)

names(empty_df)[names(empty_df) == "EDI$CODENT"] <- "CODENT"

EDI_means <- empty_df

library(openxlsx)
# Final EDI means
openxlsx::write.xlsx(x = EDI_means, file = paste0(directory, "output/EDI_promedios_normalizada.xlsx"))
