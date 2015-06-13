## makeCacheMatrix
## keeps track of two objects, a matrix and its inverse, by providing
## 4 'operations';
## - set or get the matrix
## - set or get the inverse of the matrix

## The mechanism in play is function closure with list of 'operations' and lexical scoping,
## in short, function with data

## CacheSolve
## utilizes the makeCacheMatrix list of 'operations' by two scenarios
## If the inverse of matrix exists, just return it
## If the inverse of matrix does not exist, calculate, store it and return it

## NB! CacheSolve assumes an INVERTIBLE matrix from makeCacheMatrix

## See the following for more in-depth information;
## http://adv-r.had.co.nz/Environments.html
## http://adv-r.had.co.nz/Functional-programming.html


# makeCacheMatrix implementation

makeCacheMatrix <- function(x = matrix()) {
      
     # Hosting the matrix inverse object
     matrix_inverse <- NULL
     
     # Set new matrix and reset the matrix inverse, in parent environment
     set <- function(y) {
          x <<- y
          matrix_inverse <<- NULL
     }
     
     # Get the matrix
     get <- function() x
     
     # Set the matrix inverse in parent environment
     setinverse <- function(inverse) matrix_inverse <<- inverse
     
     # Get the matrix inverse found in parent environment
     getinverse <- function() matrix_inverse
     
     # Return a list of 'operation' for the matrix, giving long-living exec env.
     list(set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


# cacheSolve implementation using makeCacheMatrix 'operations'

cacheSolve <- function(x, ...) {
     
     the_inverse <- x$getinverse()
     
     # Return the cached inverse if existing
     if(!is.null(the_inverse)) {
          message("getting cached data")
          return(the_inverse)
     }
     
     # inverse does not exist, calculate it, store it and return it
     the_matrix <- x$get()
     the_inverse <- solve(the_matrix, ...)
     x$setinverse(the_inverse)
     the_inverse
}


## TEST scenarios for makeCacheMatrix

set.seed(17)

a_matrix <- matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5)
m <- makeCacheMatrix(a_matrix)

all(m$get() == a_matrix)                # get the matrix back
is.null(m$getinverse())                 # get the inverse - should be NULL
m$setinverse(solve(m$get()))            # set the inverse
all(m$getinverse() == solve(m$get()))   # get the inverse, equal to calc in previous step

a_matrix2 <- matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5)
m$set(a_matrix2)

all(m$get() == a_matrix2)               # get the new matrix back
is.null(m$getinverse())                 # get the inverse - should be NULL 

rm(a_matrix, a_matrix2, m)              # Cleaning up

## TEST scenarios for cacheSolve

set.seed(23)

m <- makeCacheMatrix(matrix(sample.int(15, size = 5*5, replace = TRUE), nrow = 5, ncol = 5))

cacheSolve(m) # 1st time - NO "getting cached data"
cacheSolve(m) # 2nd time - "getting cached data"
cacheSolve(m) # 3rd time - "getting cached data"

m$set(matrix(sample.int(15, size = 7*7, replace = TRUE), nrow = 7, ncol = 7))

cacheSolve(m) # 1st time - NO "getting cached data"
cacheSolve(m) # 2nd time - "getting cached data"

rm(m)


