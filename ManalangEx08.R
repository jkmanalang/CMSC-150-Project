#***********************************************************
#
# This is a script that performs Quadratic Spline
# Interpolation
#
# @author John Kenneth Manalang
# @created_date  2020-12-03  19:33
#
#***********************************************************

# declaration of functions starts here

# ------------------------------ functions from previous exer starts here --------------------------------

checkFunctions <- function(system){
  # initiating the varCount with the number of the variables in the first function
  varCount = length(getVariableList(system[[1]]))
  
  # then compare it in the remaining functions' variable count
  for (x in 2:length(system)){
    if (varCount != length(getVariableList(system[[x]]))){
      return (FALSE)
    }
  }
  
  # return TRUE if all functions' variable count are equal
  return (TRUE)
}

# function that swaps 2 rows
swap <- function(augMatrix, x, y){
  swappedMatrix = augMatrix
  temp = swappedMatrix[x,]
  swappedMatrix[x,] = swappedMatrix[y,]
  swappedMatrix[y,] = temp
  return (swappedMatrix)
}

# NOTE: modified to fit what is only needed in the regression
GaussJordanElimination <- function (augMatrix, variableCount){
  
  # creating a vector that will serve as the storage for the solution set later
  solutionSet = numeric(variableCount)
  
  for (i in 1:variableCount){
    if (i != variableCount){
      # loop for finding the pivot row
      for(x in i:variableCount){
        if(abs(augMatrix[x,i]) == max(abs(augMatrix[i:variableCount,i]))){
          pivotRow = x
          break
        }
      }
      
      # terminating the function if pivot element is 0
      if (augMatrix[pivotRow,i] == 0){
        return (NA)
      }
      augMatrix = swap(augMatrix, i, pivotRow)
    }
    
    # normalizing the current row by dividing the entire row with the pivot element
    augMatrix[i,] = augMatrix[i,] / augMatrix[i,i]
    
    # this loop is for making the entire column of the pivot element equal to 0 (except the pivot element itself)
    for (j in 1:variableCount){
      if (i==j) {next}
      normalizedRow = augMatrix[j,i] * augMatrix[i,]
      augMatrix[j,] = augMatrix[j,] - normalizedRow
      
    }
  }
  
  # storing the last column of the matrix to a vector 'solutionSet'
  for (i in 1:variableCount){
    solutionSet[i] = augMatrix[i, variableCount+1]
  }
  
  return (solutionSet)
}


# ------------------------------- functions from previous exer ends here ---------------------------------

# function to get the coefficients from the internal points
getInternalKnots <- function(data, row_count, n){
  
  internalMatrix = matrix(0, nrow = row_count+1, ncol = n+1)
  # helper variables to determine which column the coefficients will be put in
  column_helper = 0
  current_data_row = 2
  
  # loop for each row
  for(i in 1:row_count+1){
    if(i == 1) next # skip first row (a1 = 0)
    
    internalMatrix[i, 1+(3*column_helper)] = (data[[1]][current_data_row])^2  # for a
    internalMatrix[i, 2+(3*column_helper)] = (data[[1]][current_data_row])    # for b
    internalMatrix[i, 3+(3*column_helper)] = 1                                # for c
    internalMatrix[i, n+1] = data[[2]][current_data_row]                      # constant
    
    # if statement to manipulate helpers
    if(i%%2 == 1) {current_data_row = current_data_row+1}
    if(i%%2 == 0) {column_helper = column_helper+1}
  }
  
  return (internalMatrix)
}

# function to get the coefficients from the end points
getEndPoints <- function(data, n){

  endPointMatrix = matrix(0, nrow = 2, ncol = n+1)
  
  # first point
  endPointMatrix[1,1] = (data[[1]][1])^2
  endPointMatrix[1,2] = (data[[1]][1])
  endPointMatrix[1,3] = 1
  endPointMatrix[1,n+1] = data[[2]][1]
  
  # last point
  endPointMatrix[2,n-2] = (data[[1]][length(data[[1]])])^2
  endPointMatrix[2,n-1] = (data[[1]][length(data[[1]])])
  endPointMatrix[2,n] = 1
  endPointMatrix[2,n+1] = data[[2]][length(data[[1]])]
  
  return (endPointMatrix)
}

# function to get the coefficients from the derivatives of the internal points
getInternalDerivatives <- function(data, n, internalCount){
  
  internalDerivativesMatrix = matrix(0, nrow = internalCount, ncol = n+1)
  
  # for each point
  for(i in 1:internalCount){
    column_helper = (3*(i-1))
    
    internalDerivativesMatrix[i, column_helper + 1 ] = (data[[1]][i+1])*2     # for a
    internalDerivativesMatrix[i, column_helper + 2 ] = 1                      # for b
    internalDerivativesMatrix[i, column_helper + 4 ] = -((data[[1]][i+1])*2)  # for next a
    internalDerivativesMatrix[i, column_helper + 5 ] = -1                     # for next b
  }
  
  return (internalDerivativesMatrix)
}

# function for formulating and storing the coefficients with their x into a string
getFunc <- function(unknowns, internalCount){
  fxns = list()
  func_trace = 0
  
  for(i in 1:(internalCount+1)){
    func = "function(x) ("
    
    func = paste(func, unknowns[1+(3*func_trace)], sep="")
    func = paste(func, "*(x^2))+(", sep="")
    func = paste(func, unknowns[2+(3*func_trace)], sep="")
    func = paste(func, "*(x))+(", sep="")
    func = paste(func, unknowns[3+(3*func_trace)], sep="")
    func = paste(func, ")", sep="")
    
    fxns[[i]] = func
    
    func_trace = func_trace+1
  }
  
  return (fxns)
}

# function for getting which interval the given x will fall into. return -1 if none.
getInterval <- function(data, x){
  interval = -1

  for(i in 1:(length(data[[1]])-1)){
    if(data[[1]][i] <= x && x <= data[[1]][i+1]){
      interval = i
      return (interval)
    }
  }
  return (interval)
}

# function for getting the quadratic spline interpolation
poly.qsi <- function(data, x){
  result = list()
  
  
  n = (length(data[[1]])-1) * 3           # number of equation to be made
  eq = matrix(0, nrow=n, ncol=n+1)        # matrix to store coefficients
  internalCount = length(data[[1]]) - 2   # number of internal points
  row_count_internal = internalCount*2    # number of equation for the internal points
  
  internalKnots = getInternalKnots(data, row_count_internal, n)         # getting the coefficients from internal points
  endPoints = getEndPoints(data, n)                                     # from end points
  internalDerivatives = getInternalDerivatives(data, n, internalCount)  # from derivatives of internal points
  
  # storing the values to the matrix eq
  for(i in 1:((length(data[[1]])-2)*2)+1){
    eq[i,] = internalKnots[i,]
  }
  
  row = 1
  for(i in (row_count_internal+2) : (row_count_internal+3)){
    eq[i,] = endPoints[row,]
    row = row+1
  }
  
  row = 1
  for(i in (n-internalCount+1):n){
    eq[i,] = internalDerivatives[row,]
    row = row+1
  }
  
  # ensuring that the value of a1 is 0
  eq[1,] = 0
  eq[,1] = 0
  eq[1,1] = 1
  
  
  # getting the unknowns using Gauss Jordan Elimination
  unknowns = GaussJordanElimination(eq, n)

  
  
  # constructing the function strings
  fxns = getFunc(unknowns, internalCount)
  qsi.fxns = list()
  
  # performing eval parse to the function strings
  for(i in 1:(length(fxns))){
    qsi.fxns[[i]] = eval(parse(text = fxns[[i]]))
  }
  
  # getting the interval where the x belongs
  interval = getInterval(data, x)
  
  # filling the result list
  result[[1]] = qsi.fxns
  
  if (interval == -1){
    result[[2]] = NA
  } else {
    result[[2]] = qsi.fxns[[interval]](x)
  }
  
  return (result)
}

# function to check if the x values of the data is sorted
is.sorted <- function(dataPoints){
  
  for(i in 1:(length(dataPoints[[1]])-1)){
    if(dataPoints[[1]][i] > dataPoints[[1]][i+1]){
      return (FALSE)
    }
  }
  
  return (TRUE)
}




