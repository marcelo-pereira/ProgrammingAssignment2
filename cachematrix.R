## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inversedMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  get <- function() x
  setInversedMatrix <- function(newInversedMatrix) inversedMatrix <<- newInversedMatrix
  getInversedMatrix <- function() inversedMatrix
  list(set = set, get = get,
       setInversedMatrix = setInversedMatrix,
       getInversedMatrix = getInversedMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversedMatrix <- x$getInversedMatrix()
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  data <- x$get()
  inversedMatrix <- solve(data)
  x$setInversedMatrix(inversedMatrix)
  inversedMatrix
}
