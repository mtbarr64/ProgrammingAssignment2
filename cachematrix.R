## These functions takes a matrix (assumed square) as input and uses the solve()
## function to find the inverse of the matrix, and stores the inverse in cache.

## This is the define function that sets up the calls and placeholders.

makeCacheMatrix <- function(x = matrix()) {
      ## Define the calls inside the function
      m <- NULL
      set <- function(y) {
             x <<- y
             m <<- NULL
      }
      get <- function() x
      setinverse <- function(data_matrix) m <<- data_matrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function determines if cache is populate and performs the inverse
## function as needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
