# Index building

library(openxlsx)
library(dplyr)
library(rstudioapi)   

# Nice way of getting the active directory
#directory <-  rstudioapi::getActiveDocumentContext()$path
directory <-  "C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms"

# We neede to look for the values in these columns
global <- read.xlsx(paste0(directory, '/output/pivote_nuevo.xlsx'))

# Delete first 3 rows
global <- global[-c(1:3),]

# Convert character types to numeric
global[] <- lapply(global, function(x) as.numeric(as.character(x)))

# Data with nested groups, which will be averaged
EDI_dir <- paste0(directory, '/data/ICIP_V22.xlsx')

# Entities names
names <- read.xlsx(paste0(directory, '/output/pivote_nuevo.xlsx'))
names <- names[-c(1:3),1:2]
colnames(names) <- c('código','entidad')

#-----------------#
# First dimension #
#-----------------#

EDI_1 <- read.xlsx(xlsxFile = EDI_dir, sheet = 2,fillMergedCells = T)

# Test
EDI_1$`#.pregunta.pivote` <-  gsub(pattern = '\n', '',EDI_1$`#.pregunta.pivote`)

# store each question with its code
nested_EDI_1 <- data.frame(code=EDI_1$Codigo,stringsAsFactors = F, qs=EDI_1$`#.pregunta.pivote`)

# Obtain row of questions
row_temp <- unlist(strsplit(nested_EDI_1$qs[19],split = ';'))
row_temp <-  row_temp[row_temp != ""]
# Match column names of global to ieth row
colums_temp <- global[,row_temp]

if ( is.data.frame(colums_temp) ) {
  if (all(names(colums_temp) == case_1)) {
    new_col_temp <- rowSums(sweep(colums_temp, 2, vector_1, "*"),na.rm = T)
  }
  else {
    new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
  }
  
  
} else {
  new_col_temp <- as.numeric(colums_temp)
}


container <- c()
for (i in 1:nrow(nested_EDI_1)) {
  # Obtain row of questions
  row_temp <- unlist(strsplit(nested_EDI_1$qs[i],split = ';'))
  # Match column names of global to ieth row
  colums_temp <- global[,row_temp]
  # Means of the group
  if ( is.data.frame(colums_temp) ) {
    new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
  } else {
    new_col_temp <- as.numeric(colums_temp)
  }
  
  container[[i]] <- new_col_temp
}


test <-  as.data.frame(t(do.call(cbind, container)))
test[test=="NaN"] <- NA
test <- as.matrix(test)

rownames(test) <- nested_EDI_1$code
library(analytics)
group_means <-  rowmean(test,group = rownames(test),na_rm = T)
group_means_to_save <- t(group_means)
group_means_to_save <- cbind(names, group_means_to_save)
names(group_means_to_save) <- gsub("[[:digit:]]", "", names(group_means_to_save) )

group_means_2 <- data.frame(promedio = colMeans(group_means))
group_means_to_save_2 <- cbind(names, group_means_2)


# Empty lists to save info
to_save = c()
to_save_2 = c()

# Empty list to keep important objects
to_keep = c()

# For the 4 pilars

for (pilar in 1:4) {
  
  EDI <- read.xlsx(xlsxFile = EDI_dir, sheet = pilar, fillMergedCells = T)
  
  # Test
  EDI$`#.pregunta.pivote` <-  gsub(pattern = '\n', '',EDI$`#.pregunta.pivote`)
  
  # store each question with its code
  nested_EDI <- data.frame(code = EDI$Codigo,stringsAsFactors = F, qs=EDI$`#.pregunta.pivote`)
  
  container <- c()
  for (i in 1:nrow(nested_EDI)) {
    # Obtain row of questions
    row_temp <- unlist(strsplit(nested_EDI$qs[i],split = ';'))
    row_temp <-  row_temp[row_temp != ""]
    # Match column names of global to ieth row
    colums_temp <- global[,row_temp]
    # Means of the group
    if ( is.data.frame(colums_temp) ) {
      new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
    } else {
      new_col_temp <- as.numeric(colums_temp)
    }
    
    container[[i]] <- new_col_temp
  }
  
  # Convert list to dataframe
  test <-  as.data.frame(t(do.call(cbind, container)))
  test[test=="NaN"] <- NA
  test <- as.matrix(test)
  rownames(test) <- nested_EDI$code
  
  # Row means per group
  library(analytics)
  group_means <-  rowmean(test,group = rownames(test),na_rm = T)
  group_means_to_save <- t(group_means)
  group_means_to_save <- cbind(names, group_means_to_save)
  #names(group_means_to_save) <- gsub("[[:digit:]]", "", names(group_means_to_save) )
  
  # Total row means
  group_means_2 <- data.frame(promedio = colMeans(group_means))
  group_means_to_save_2 <- cbind(names, group_means_2)
  
  # Saving info
  to_save[[pilar]] <- group_means_to_save
  to_save_2[[pilar]] <- group_means_to_save_2
  
  # Important object for last computation
  to_keep[[pilar]] <- group_means_2
  
  
  
}

library(rlist)

almost_final_df <- list.cbind(to_keep)
final_df <- data.frame("índice" = rowMeans(almost_final_df))
final_df <- cbind(names, final_df)




# Special cases
case_1 <-  c('I31A','I31B','I31C','I31D','I31E')
vector_1 <- c(1,0.75,0.5,0.25,0)

case_2 <- c('')



rowSums(sweep(colums_temp, 2, vector_1, "*"),na.rm = T)
