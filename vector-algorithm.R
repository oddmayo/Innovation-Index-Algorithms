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

example <-  pivote[4:nrow(pivote), separate[[1]] ]
split(example, seq_along(1:10))

library(stringi)
pattern <- unlist(stringi::stri_extract_all_regex(vector$questions[1],"[0-9]+") )
split(example,  rep(length(unlist(uniques)),13)  )
unique(pattern)

uniques <-  t(as.matrix(table(pattern)))


container <-  sapply(unique(pattern),
              function(x) example[startsWith(names(example) , x )],
              simplify = FALSE)

container <- lapply(container, function(x) as.data.frame(sapply(x, as.numeric))) 


result <- rowSums(container[[1]])

result <- NULL
for (dataframe in 1:length(container)) {
  result[[dataframe]] = ( rowSums(container[[dataframe]]) / ncol(container[[dataframe]]) )
  
}




uniques <- uniques[1:length(separate)]
