# These functions define and create an object that holds a numeric matrix
# and a cached value of the matrix inverse. The first time the inverse is
# needed, the solve() function is called to create the inverse. After this
# the cached value is provided instead of having to re-call the solve()
# function.

# RG Willhoft, 7/9/2019

# Creates a new cached matrix object given a numeric matrix and four functions:
#   set - set the value of the matrix
#   get - get the value of the matrix
#   setinverse - set the value of the inverse 
#   getinverse - get the value of the inverse
# All of these functions are usually only called by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # newly created, no cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # matrix changed, no cached inverse
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function( inverse ) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# Call this function to get the matrix inverse. If there is a cached
# inverse, then it will be returned; if there is no cached value, then
# the solve() function will be called to create the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


# Example of using functions:
# > matrix2 <- matrix(rnorm(25),5,5)
# > cached_matrix2 <- makeCacheMatrix(matrix2)
# > cacheSolve(cached_matrix2)
# [,1]       [,2]       [,3]       [,4]       [,5]
# [1,] -0.94360399  0.2140634 -0.4877280 -0.4194409 0.31962361
# [2,] -1.43333076  0.1481665 -1.2895628 -0.4519695 0.02834273
# [3,]  0.06595474 -0.1332631  0.8779405  0.8409763 0.41459105
# [4,]  0.45883657 -0.5013798  0.5659207  0.6996285 0.37420025
# [5,]  1.74504745 -0.5407194  1.4535696  1.7615731 0.73097132
# > cacheSolve(cached_matrix2)
# getting cached inverse
# [,1]       [,2]       [,3]       [,4]       [,5]
# [1,] -0.94360399  0.2140634 -0.4877280 -0.4194409 0.31962361
# [2,] -1.43333076  0.1481665 -1.2895628 -0.4519695 0.02834273
# [3,]  0.06595474 -0.1332631  0.8779405  0.8409763 0.41459105
# [4,]  0.45883657 -0.5013798  0.5659207  0.6996285 0.37420025
# [5,]  1.74504745 -0.5407194  1.4535696  1.7615731 0.73097132
