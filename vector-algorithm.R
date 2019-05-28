require(readxl)
# Work directory
directory <- 'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

# Example variables vector
vector <- readxl::read_excel(paste0(directory,'example-vector.xlsx'),sheet = 'Sheet2',col_names = F)
colnames(vector) <- c('variable','questions')




