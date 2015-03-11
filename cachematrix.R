## Put comments here that give an overall description of what your
## functions do

## Create a caching object.
##  the object has 4 methods:
##     get() - get the value of the object
##     set(newvalue) - set the value of the object
##     setcalculation(newcalculatedvalue) - set an associated "calculated" value 
##     getcalculation() - get the associated "calculated" value
##
## Example:
##   o = makeCacheMatrix()
##   o$set(matrix(c(1,2,3,4), c(2,2)))
##   o$setcalculation(solve(o))
##   print o$getcalculation()
##
makeCacheMatrix <- function(x = matrix()) {
  # initialize the calculated value holder to NULL
  cached_calculation <- NULL
  
  # setter function for the matrix value
  set <- function(new_matrix_value) {
    x <<- new_matrix_value           # set the new matrix value
    cached_calculation <<- NULL      # clear the cached inverse value
  }

  # getter function for the matrix value
  get <- function() x
  
  # setter function for the inverse value
  setcalculation <- function(new_calculated_value) cached_calculation <<- new_calculated_value

  # getter function for the inverse value  
  getcalculation <- function() cached_calculation
  
  # return an object with a getter, a setter
  list(set = set, get = get,
       setcalculation = setcalculation,
       getcalculation = getcalculation)
}


## Create a matrix object which caches its
##    inverse value.
##
## Mixes the calculation function 'solve' with the
##   a 'makeCacheMatrix' object
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get the cached calculated value (the inverse)
  #  and return it if it has been set
  inverse_matrix <- x$getcalculation()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  
  # otherwise, get the original matrix value
  #  calculate the inverse, save it, and return it
  original_matrix <- x$get()
  inverse_matrix <- solve(original_matrix, ...)
  x$setcalculation(inverse_matrix)
  inverse_matrix
}
