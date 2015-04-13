## These functions make a cache for a matrix, test for it, and solve it. 
## It will return the inverse of whatever matrix that is put in. If that matrix has already been evaluated,
## it will output the previously evaluated solution.

## makeCacheMatrix takes a matrix and creates a special vector which is a list 
## containing functions to set the value of the matrix, get the value of the matrix
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
  {
    i <- NULL
    set <- function (y)
      {
        x <<- y
        i <<- NULL
      }
    get <- function () x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i 
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse
         )

  }


##cacheSolve calculates the mean of the special vector created in makeCacheMatrix that
## is created in the above function. It first checks to see if that matrix has already been solved
## If so, it outputs the inverse and skips the solve. Otherwise, it calculates it and sets it via setinverse

cacheSolve <- function(x, ...) 
  {
## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) 
    {
      message("Getting cached data.")
      return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
