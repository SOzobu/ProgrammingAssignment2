## Put comments here that give an overall description of what your
## functions do


## Function (makeCacheMatrix) creates a special “matrix” object that can cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
## "get" returns the vector x stored in the main function, 
## while "set" changes the vector stored in the main function.
## and "setmean" and "getmean" only store the value of the input 
## in a variable "m" into the main function makeVector (setmean) and return it (getmean).



makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## Function (cacheSolve) computes the inverse of the special (matrix) which is returned by makeCacheMatrix above 
## If the inverse has already been calculated then the "cachesolve" should retrieve the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored with "makeCacheMatrix", 
## "m" calculates the inverse, and "x$setmean(m)" stores it in the object m in "makeCacheMatrix". 
              
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

              
 ## Example
> a <- diag(5,3)
> a
     [,1] [,2] [,3]
[1,]    5    0    0
[2,]    0    5    0
[3,]    0    0    5
> 
> CachedMarix <- makeCacheMatrix(a)
> cacheSolve(CachedMarix)
     [,1] [,2] [,3]
[1,]  0.2  0.0  0.0
[2,]  0.0  0.2  0.0
[3,]  0.0  0.0  0.2
> 
> b <- diag(2,6)
> b
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    2    0    0    0    0    0
[2,]    0    2    0    0    0    0
[3,]    0    0    2    0    0    0
[4,]    0    0    0    2    0    0
[5,]    0    0    0    0    2    0
[6,]    0    0    0    0    0    2
> 
> cacheSolve(CachedMarix)   #getting cached data
getting cached data
     [,1] [,2] [,3]
[1,]  0.2  0.0  0.0
[2,]  0.0  0.2  0.0
[3,]  0.0  0.0  0.2
> 
> b <- diag(2,6)
> b
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    2    0    0    0    0    0
[2,]    0    2    0    0    0    0
[3,]    0    0    2    0    0    0
[4,]    0    0    0    2    0    0
[5,]    0    0    0    0    2    0
[6,]    0    0    0    0    0    2
