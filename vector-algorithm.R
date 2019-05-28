require(readxl)
# Work directory
directory <- 'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

# Example variables vector
vector <- readxl::read_excel(paste0(directory, 'example-vector.xlsx'),sheet = 'Sheet2',col_names = F)
colnames(vector) <- c('variable','questions')

# Pivote table with desired columns
pivote <- readxl::read_excel(paste0(directory, 'Pivote.xlsx'))

# First try
separate <- as.list(strsplit(vector$questions, ";")) 
separate[[1]][[1]]

mapply(function(n,v) which(separate[[n]]==v) , d1$name,d1$val)

pivote[4:nrow(pivote), separate[[1]] ]
