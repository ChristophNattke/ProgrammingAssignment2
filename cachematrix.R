## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix, which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

## 1. set the value of the matrix
    m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }

## 2. get the value of the matrix
  get <- function() x

## 3. set the value of the inverse
  setinverse <- function(inverse) m <<- inverse

## 4. get the value of the inverse
  getinverse <- function() m

## returns a list containing the function
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated then the cacheSolve function should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

  m <- x$getinverse()

## If the inverse has already been calculated the inverse is retrieved from the cache
  if (!is.null(m)) {
          message("getting cached data")
          return(m)
  }

## Compute the inverse of the special matrix returned by makeCacheMatrix 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

## Return a matrix that is the inverse of 'x'
  m
}
