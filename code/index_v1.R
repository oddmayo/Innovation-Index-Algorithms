# Index building

library(openxlsx)
library(dplyr)
library(rstudioapi)   

# Nice way of getting the active directory
#directory <-  rstudioapi::getActiveDocumentContext()$path
directory <-  "C:/Users/LcmayorquinL/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/Innovation-Index-Algorithms"

# We neede to look for the values in these columns
global <- read.xlsx(paste0(directory, '/data/Pivote NAC Version25.xlsx'))

# Delete first 3 rows
global <- global[-c(1:3),]
# Delete last row
global <- global[-nrow(global),]

# Convert character types to numeric
global[,2:ncol(global)] <- lapply(global[,2:ncol(global)], function(x) as.numeric(as.character(x)))

# Chosen entities
chosen <- read.xlsx(paste0(directory, '/data/entidades_escogidas.xlsx'))

# Filter global by chosen entities
library(data.table)
global2 <- setDT(global)[X1 %in% chosen$Code]

# Data with nested groups, which will be averaged
EDI_dir <- paste0(directory, '/data/ICIP_V25.xlsx')

# Entities names
names <- read.xlsx(paste0(directory, '/output/pivote_nuevo.xlsx'))
names <- names[-c(1:3),1:2]
colnames(names) <- c('código','entidad')

names <-  names %>% filter(names$código %in% chosen$Code)
names <- names[match(chosen$Code, names$código),]

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
  if (all(names(colums_temp) == case_2)) {
    new_col_temp <- rowSums(sweep(colums_temp, 2, vector_2, "*"),na.rm = T)
  }
  else if (all(names(colums_temp) == case_1)) {
    colums_temp$`115E`[is.na(colums_temp$`115E`)] <- 0
    colums_temp$`115D`[is.na(colums_temp$`115D`)] <- 0
    SUB_115CA <- colums_temp$`115C` / colums_temp$`115A`
    SUB_115CA[is.na(SUB_115CA)] <- 0
    
    SUB_115BA <- colums_temp$`115B` / colums_temp$`115A`
    SUB_115BA[is.na(SUB_115BA)] <- 0
    # Operation
    SUB_115BA[SUB_115BA > 1] <- 1
    SUB_115CA[SUB_115CA > 1] <- 1
    colums_temp$`115D`[colums_temp$`115D` > 0] <- 1
    colums_temp$`115E`[colums_temp$`115E` > 0] <- 1
    
    new_col_temp <- (SUB_115BA+SUB_115CA+colums_temp$`115D`+colums_temp$`115E`)/4
  }
  
  else if(all(names(colums_temp) == case_3)) {
    new_col_temp <- (rowSums(sweep(colums_temp, 2, vector_3, "*"),na.rm = T))/6
  }
  
  else if(all(names(colums_temp) == case_4)) {
    new_col_temp <- (rowSums(sweep(colums_temp, 2, vector_4, "*"),na.rm = T))/3
  }
  
  
  else {
    new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
  }
  
} else {
  if (row_temp == case_5) {
    case_5.1 <- (1 - colums_temp)
    new_col_temp <- as.numeric(case_5.1)
  }else{
    new_col_temp <- as.numeric(colums_temp)
  }
  
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


# Special cases
case_1 <- c("115A", "115B", "115C", "115D", "115E")

case_2 <-  c('I31A','I31B','I31C','I31D','I31E')
vector_2 <- c(1,0.75,0.5,0.25,0)

case_3 <- c("3C1","3C2","3D1","3D2","3E1","3E2","3F1","3F2","3G1","3G2","3H1","3H2")
vector_3 <- c(1,0.5,1,0.5,1,0.5,1,0.5,1,0.5,1,0.5)

case_4 <- c("3A1","3A2","3C1","3C2","3H1","3H2")
vector_4 <- c(1,0.5,1,0.5,1,0.5)

case_5 <- "334B"

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
      if (all(names(colums_temp) == case_2)) {
        new_col_temp <- rowSums(sweep(colums_temp, 2, vector_2, "*"),na.rm = T)
      }
      else if (all(names(colums_temp) == case_1)) {
        colums_temp$`115E`[is.na(colums_temp$`115E`)] <- 0
        colums_temp$`115D`[is.na(colums_temp$`115D`)] <- 0
        SUB_115CA <- colums_temp$`115C` / colums_temp$`115A`
        SUB_115CA[is.na(SUB_115CA)] <- 0
        
        SUB_115BA <- colums_temp$`115B` / colums_temp$`115A`
        SUB_115BA[is.na(SUB_115BA)] <- 0
        # Operation
        SUB_115BA[SUB_115BA > 1] <- 1
        SUB_115CA[SUB_115CA > 1] <- 1
        colums_temp$`115D`[colums_temp$`115D` > 0] <- 1
        colums_temp$`115E`[colums_temp$`115E` > 0] <- 1
        
        new_col_temp <- (SUB_115BA+SUB_115CA+colums_temp$`115D`+colums_temp$`115E`)/4
      }
      
      else if(all(names(colums_temp) == case_3)) {
        new_col_temp <- (rowSums(sweep(colums_temp, 2, vector_3, "*"),na.rm = T))/6
      }
      
      else if(all(names(colums_temp) == case_4)) {
        new_col_temp <- (rowSums(sweep(colums_temp, 2, vector_4, "*"),na.rm = T))/3
      }
      
      
      else {
        new_col_temp <- rowMeans(colums_temp, na.rm=TRUE)
      }
      
    } else {
      if (row_temp == case_5) {
        case_5.1 <- (1 - colums_temp)
        new_col_temp <- as.numeric(case_5.1)
      }else{
        new_col_temp <- as.numeric(colums_temp)
      }
      
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


# Pilars
pilar_1 <- to_save_2[[1]]
pilar_2 <- to_save_2[[2]]
pilar_3 <- to_save_2[[3]]
pilar_4 <- to_save_2[[4]]

# Dimensions
matriz <- cbind(to_save[[1]],to_save[[2]],to_save[[3]],to_save[[4]])
matriz <- matriz[, !duplicated(colnames(matriz), fromLast = F)]
