## makeCacheMatrix contains 4 functions: 2 to get and set the data 
## And 2 functions to get and set the inverse of the matrix

## This cahche function also contains the functionality of what we want to do. 
## It will solve the inverse of the matrix, set or get the data, and get the inverse of the matrix.
## Any of these 4 functions can be called upon by another funciton

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix <- NULL
  
  set <- function(cache_matrix){
    x <<- cache_matrix
    inverse_matrix <<- NULL
  }
  
  get <- function(){
    x
  }
  
  set_inverse <- function(solve){
    inverse_matrix <<- solve
  }
  
  get_inverse <- function(){
    inverse_matrix
  } 
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## This function checks if the matrix inverse is already available.
## If it is available it will retrieve the solution
## If it is not available it will go and call the functionality from the previous funciton
## This results in the inverse of the matrix being calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$get_inverse()
  
  if(!is.null(inverse_matrix)){
    message('getting cached inverse matrix')
    return(inverse_matrix)
    }
    
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
