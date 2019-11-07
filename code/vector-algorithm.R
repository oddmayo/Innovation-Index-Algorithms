require(readxl)
library(stringi)
# Work directory
directory <- 'C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms/'

# Example variables vector
vector <- readxl::read_excel(paste0(directory, 'example-vector.xlsx'),sheet = 'Sheet2',col_names = F)
colnames(vector) <- c('variable','questions')

# Pivote table with desired columns
pivote <- readxl::read_excel(paste0(directory, 'Pivote.xlsx'))

#----------------------------------------------------------------------------------
# First try
#----------------------------------------------------------------------------------

# List of all the variables
separate <- as.list(strsplit(vector$questions, ";")) 

example <-  pivote[4:nrow(pivote), separate[[1]] ]

# numeric pattern to separate questions of same variable
pattern <- unlist(stringi::stri_extract_all_regex(vector$questions[1],"[0-9]+") )

# store dataframes of questions in a list
container <-  sapply(unique(pattern),
              function(x) example[startsWith(names(example) , x )],
              simplify = FALSE)

# convert character dataframes into numeric
container <- lapply(container, function(x) as.data.frame(sapply(x, as.numeric))) 

# store means in list
result <- NULL
for (dataframe in 1:length(container)) {
  # sum of values of column per question / total of questions in this variable
  result[[dataframe]] = ( rowSums(container[[dataframe]]) / ncol(container[[dataframe]]) )
}

# final mean
final <- Reduce("+", result) / length(result)
final <- as.data.frame(final)

#----------------------------------------------------------------------------------
# For attempt
#----------------------------------------------------------------------------------

# List of all the variables
separate <- as.list(strsplit(vector$questions, ";")) 
final <- NULL

for (variable in 1:length(separate)) {
  example <-  pivote[4:nrow(pivote), separate[[variable]] ]
  
  # numeric pattern to separate questions of same variable
  pattern <- unlist(stringi::stri_extract_all_regex(vector$questions[variable],"[0-9]+") )
  
  # store dataframes of questions in a list
  container <-  sapply(unique(pattern),
                       function(x) example[startsWith(names(example) , x )],
                       simplify = FALSE)
  
  # convert character dataframes into numeric
  container <- lapply(container, function(x) as.data.frame(sapply(x, as.numeric)))

  # store means in list
  result <- NULL
  
  for (dataframe in 1:length(container)) {
    # sum of values of column per question / total of questions in this variable
    result[[dataframe]] = ( rowSums(container[[dataframe]]) / ncol(container[[dataframe]]) )
  }
  
  # final mean
  final[[variable]] <- ( Reduce("+", result) / length(result) )
  
}

final <- as.data.frame(final)
colnames(final) <- vector$variable
# end








