library(readxl)

directory <-  'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

data <- readxl::read_excel(paste0(directory, 'data/EDI_Entidad_2018.xlsx')  )

data2 <-  aggregate(data[, 3:ncol(data)], list(data$CODENT), mean)

#uniques <-  unique(c(data$CODENT,data$CUIN))

data3 <- data2[,2:ncol(data2)]

# Custom formula

rule <-  function(x) {
  n = ( (x-1)/4 )
  return(n)
}

data3[] <- lapply(data3[2:ncol(data3)], rule)

data_final <-  add_column(data3, data2$Group.1, .before = 1)
colnames(data_final)[which(names(data_final) == "data2$Group.1")] <- "CODENT"


sum(table(data$H01a))

table(data$H02)
table(data$H03a)
table(data$H03b)
table(data$H03d)
table(data$H03e)
table(data$H04i)
table(data$H04a)

mean(data$C01a)
mean(data$C01e)
