## author BARIS AKGUN
## makeCacheMatrix funct. creates a matrix whose inverse form can be cached.
## makeCacheMatrix functions' are;
## get: it returns the original matrix
## set: it sets the original matrix
## getinverse: it returns the inverse of the original matrix
## setinverse: it sets the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixInverse) inv <<- matrixInverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve funct. craetes inverse of the given matrix that is created by makeCacheMatrix funct.
## cacheSolve funct. checks whether the inverse of matrix has already been created or not.
## if matrix' inverse has not been created before, cacheSolve funct. calculates the inverse of given matrix and return given matrix' inverse.
## if matrix' inverse has been created before, cacheSolve funct. does not calculates the inverse of given matrix and returns given matrix' inverse from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("matrix'inverse is getting from cached data")
    return(inv)
  }
  message("(Firt call) inverse of given matrix is calculated...")
  mtrxOrg <- x$get()
  inv <- solve(mtrxOrg)
  x$setinverse(inv)
  inv
}


 ## Running Code Example;
 ## 1. download cachematrix.R file to your desktop and copy it to your R working directory.
 ## 2. get cachematrix.R fucntions with using source command
 ## source("cachematrix.R") 
 ## 3. Create sample matrix with using makeCacheMatrix function
 ## mtrx <- makeCacheMatrix(matrix(rnorm(9*3), ncol=3,nrow=3)) 
 ## 4. To get inverse of matrix use cacheSolve function
 ## cacheSolve(mtrx) 
 ## 5. To get inverse of matrix from cached, use again the cacheSolve function
 ## cacheSolve(mtrx) 
