library(readxl)

directory <-  'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

data <- readxl::read_excel(paste0(directory, 'data/EDI_Entidad_2018.xlsx')  )


library(data.table)
data2 <-  aggregate(data[, 2:ncol(data)], list(data$CODENT), mean)

uniques <-  unique(c(data$CODENT,data$CUIN))

data3 <- data2[,2:ncol(data2)]
# Custom formula

rule <-  function(x) {
  n = ( (x-1)/4 )
  return(n)
}

data3[] <- lapply(data3, rule)

data4 <-  add_column(data3, data, .before = 1)

