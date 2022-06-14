#***********************************************************
#
# A script for performing simplex method (minimization and
# maximization).
#
# @author John Kenneth Manalang
# @created_date  2020-12-14  00:10
#
#***********************************************************

# declaration of functions starts here

# function for getting the pivot column
getPivotColumn <- function(tableau){
  col = NULL
  last_row = length(tableau[,1])
  column_count = length(tableau[1,])
  
  # finding which negative value has the greatest magnitude
  for(i in 1:column_count){
    if(tableau[last_row, i] >= 0) next
    if(is.null(col)) col = i
    if(tableau[last_row, i] < tableau[last_row, col]) col = i
  }
  
  # will return the value NULL of col if there is no negative value found
  return (col)
}

# function for getting the pivot row
getPivotRow <- function(tableau, pivot_column){
  row = NULL
  last_column = length(tableau[1,])
  
  # getting and comparing of test ratios
  for(i in 1:(length(tableau[,1])-1)){
    if(tableau[i, pivot_column] == 0) next
    
    # for current test ratio
    a = tableau[i, last_column]
    b = tableau[i, pivot_column]
    
    if((a/b) <= 0) next
    
    if(is.null(row)) {
      row = i
      next
    }
    
    # for current winning row
    c = tableau[row, last_column]
    d = tableau[row, pivot_column]
    
    if((a/b) < (c/d)){
      row = i
    }
  }
  # will return the value NULL of row if there is no non-zero/non-negative test ratio found
  return (row)
}

# function for eliminating row based on the normalized row
eliminateRow <- function(tableau, normalized_row, pivot_column, pivot_row){
  
  # iterating through all rows
  for(i in 1:length(tableau[,1])){

    if(i == pivot_row) next
    
    temp = normalized_row * tableau[i, pivot_column]

    tableau[i,] = tableau[i,] - temp
    
  }
  return (tableau)
}

# function for getting which columns has one non-zero value
getNonZeroRow <- function(tableau, column){
  row_count = length(tableau[,1])
  row = NULL
  zero_count = 0
  
  # iterating through all rows of specific column
  for(i in 1:row_count){
    if(tableau[i, column] == 0) {
      zero_count=zero_count+1
      next
    }
    row = i
  }
  
  if(zero_count == (row_count-1)) return (row)
  return (-1)
}

# function for getting the basic solution for the simplex method
getBasicSolution <- function(tableau, isMax){
  column_count = length(tableau[1,])
  last_row = length(tableau[,1])
  
  col_name = colnames(tableau)
  
  basicSolution = matrix(0, nrow = 1, ncol = column_count-1, dimnames = list(c(1), col_name[-(column_count-1)]))
  
  # getting the basic solution for maximization
  if(isMax){
    for(i in 1:(column_count-1)){
      row = getNonZeroRow(tableau, i)
      if(row != -1){
        value = tableau[row, column_count] / tableau[row, i]
        basicSolution[1,i] = value
      } else {
        basicSolution[1,i] = 0
      }
    }
  
  # getting the basic solution for minimization
  } else {
    tableau_noZ = tableau[,-(column_count-1)]
    for(i in 1:(column_count-1)){
      basicSolution[1, i] = tableau_noZ[last_row, i]
    }
  }
  
  return (basicSolution)
}

# function for getting the number of items ship per plant to per warehouse
shipping.num <- function (basicSol){
  rowNames = c("Denver", "Phoenix", "Dallas")
  colNames = c("California", "Utah", "New Mexico", "Illinois", "New York")
  
  # getting the values of the coefficients (minimization)
  quantity = matrix(basicSol[9:23], TRUE , nrow = 3, ncol = 5, dimnames = list(rowNames, colNames))

  # matrix of payment per PPE
  amountPerPPE = matrix(0, TRUE , nrow = 3, ncol = 5, dimnames = list(rowNames, colNames))
  amountPerPPE[1,] = c(10,8,6,5,4)
  amountPerPPE[2,] = c(6,5,4,3,6)
  amountPerPPE[3,] = c(3,4,5,5,9)
  
  # getting the total payment per warehouse
  Total = numeric(5)
  
  totalPricesPerShip = quantity*amountPerPPE
  
  for(i in 1:(length(totalPricesPerShip[1,]))){
    Total[i] = sum(totalPricesPerShip[,i])
  }
  
  quantity = rbind(quantity, Total)
  
  return (quantity)
}

# function for performing simplex method 
simplex <- function(tableau, isMax, shipping.num){
  return_value = list(final.tableau = matrix(), basic.solution = matrix(), opt.val = numeric(1), shipping.num = matrix())
  terminated = FALSE
  while(TRUE){
    pivot_column = getPivotColumn(tableau) # getting the pivot column

    if(is.null(pivot_column)) break
    pivot_row = getPivotRow(tableau, pivot_column) # getting the pivot row
    
    if(is.null(pivot_row)){
      print("error: null pivot row")
      print("No feasible solution")
      terminated = TRUE
      break
    }

    pivot_element = tableau[pivot_row, pivot_column] # getting the pivot element
    
    normalized_row = tableau[pivot_row,] / pivot_element # getting the normalized row

    tableau[pivot_row,] = normalized_row 
    tableau = eliminateRow(tableau, normalized_row, pivot_column, pivot_row) # eliminating rows in the pivot column
  }
   # storing the necessary values after the loop ends
  return_value[[1]] = tableau
  if (!terminated) return_value[[2]] = getBasicSolution(tableau, isMax)
  if (!terminated) return_value[[3]] = return_value[[2]][length(tableau[1,])-1]
  if (shipping.num) return_value[[4]] = shipping.num(return_value[[2]])
  
  return (return_value)
}

# function for generating column names
getColName <- function (varCount, consCount, bool){
  colName = character(varCount+consCount+2)
  if(bool == TRUE){
    for(i in 1:varCount){
      colName[i] = paste0("x", i)
    }
    for(i in 1:consCount){
      colName[i+varCount] = paste0("S", i)
    }
  }else{
    for(i in 1:consCount){
      colName[i] = paste0("S", i)
    }
    for(i in 1:varCount){
      colName[i+consCount] = paste0("x", i)
    }
  }
  
  colName[varCount+consCount+1] = "Z"
  colName[varCount+consCount+2] = "Solution"
  
  return(colName)
}

# function for generating row names
getRowName <- function (consCount){
  rowName = character(consCount+1)
  
  for(i in 1:consCount){
    rowName[i] = paste0("Constraint ", i)
  }
  rowName[consCount+1] = "Objective Function"
  
  return(rowName)
}

# function for helping to build tableau for GUI
formTableau <- function (mat, bool){
  # getting the variable and constraint counts
  varCount = length(mat[1,]) - 1
  consCount = length(mat[,1]) - 1
  
  # getting the column and row names
  colName = getColName(varCount, consCount, bool)
  rowName = getRowName(consCount)

  
  if(bool == TRUE){ # for maximization
    # creating empty tableau
    tableau = matrix(0, nrow = consCount+1, ncol = varCount+consCount+2, dimnames = list(rowName,colName))
    
    # inserting the values to their proper coordinates
    for(i in 1:varCount){
      tableau[,i] = mat[,i]
    }
    tableau[consCount+1,] = tableau[consCount+1,] * (-1)
    
    # same with the slack variables
    for(i in 1:(consCount+1)){
      tableau[i, varCount+i] = 1
    }
    
    tableau[, varCount+consCount+2] = mat[,varCount+1]
    
    return (tableau)
    
  }else{ # for minimization
    rowCount = length(mat[1,])
    colCount = varCount+consCount+2
    tmin = matrix(0, nrow = rowCount, ncol = colCount, dimname = list(NULL, colName))
    for(i in 1:(consCount)){
      tmin[,i] = mat[i,]
    }
    # ---- making of the tableau ----
    tmin[,length(tmin[1,])] = mat[length(mat[,1]),]
    tmin[rowCount, colCount] = 0
    #setting up the slack variables
    for(i in 1:rowCount){
      tmin[i, consCount+i] = 1
    }
    tmin[rowCount,] = -1*tmin[rowCount,] #transposing of equation
    tmin[rowCount,(colCount-1)] = 1 #for variable Z (minimize variable)
    return (tmin)
  }
}

