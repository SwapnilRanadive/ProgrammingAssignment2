
## makeCacheMatrix function takes matrix as an argument. Matrix is initialized using matrix()

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse_matrix variable to NULL
  inverse_matrix <- NULL
  
  ## set function to set the matrix to be processed and re-initialize inverse_matrix
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  ## get the matrix to be used for processing
  get <- function() x
  
  ## set the inverse matrix after forming it in cacheSolve function
  setinverse <- function(inverse)  inverse_matrix <<- inverse
  
  ## get the value of inverse matrix
  getinverse <- function() inverse_matrix
  
  ## create a list for cacheSolve function to access along with data and inverse matrix
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve function uses makeCacheMatrix to check the inverse of an input matrix

cacheSolve <- function(x, ...) {
  
  ## get the inverse marix from makeCacheMatrix using getinverse()
  inverse_matrix <- x$getinverse()
  
  ## if function 
  if (!is.null(inverse_matrix)) {
    
    message("getting cached data")
    return(inverse_matrix)
    
    
  }
  
  ## get the value of matrix; this is input matrix 
  data <- x$get()
  
  ## inverse the matrix using solve()
  inverse_matrix <- solve(data, ...)
  
  ## set the value of inverse matrix to be used as cache
  x$setinverse(inverse_matrix)
  
  inverse_matrix
  
}
