

##
## makeCacheMatrix function creates a special object that has ability to store 
## a matrix and it's inverse  
##
## The objects has getter and setter functions for matrix and matrix inverse data.
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
## cacheSolve function returns inverse of a matrix that was created as a special 
## object using the makeCacheMatrix function.
##
## The function either returns previously calculated inverse if its available or 
## calculates the inverse and stores it before returning it.
##
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message('Using cached inverse')
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
