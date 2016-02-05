##Assigment two
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL ### iniciate null cache

  set <- function(y) {  ## as in the example crereate working enviroment
    x <<- y
    cache <<- NULL
  }
  get <- function() x # as in example set the get for the list 
  get_cache <- function() cache # so.. get cache
  matrix_value <- function(matrix_inv) cache <<- matrix_inv ##send to cache the inversw
  
  list(set=set,get=get,get_cache=get_cache,matrix_value= matrix_value ) # return everything in list
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  m <- x$get_cache()
  if (!is.null(m)) { # if cache is not empty then matrix exists
    message("getting cached data")
    return(m) ## as in the example
  } else { # there is no matrix
    my_matrix <- x$get() # get it
    if ( ncol(my_matrix) == nrow(my_matrix)){ ## at least square in size
      m <- solve(my_matrix) # get the inverse and to cache 
    }
  }
 return(m)
}
