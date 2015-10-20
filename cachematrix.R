##This functions are going to cache the 
## inverse of a matrix

## makeCacheMatrix:
## 1. resetting the value m 
## 2. a function set (set the value of the matrix)
## 3. a function get (get the value of the matrix)
## 4. a function setting the value of the inverse
## 5. a function getting the value of the inverse
## 6. creating the list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}

## cacheSolve calculates the inverse of the matrix
## if the inverse has already been calculated, it retrieves the inverse from cache
## otherwise it sets the inverse via setSolve

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m      
  ## Return a matrix that is the inverse of 'x'
}
