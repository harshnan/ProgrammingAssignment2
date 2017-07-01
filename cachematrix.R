## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to create Cached data for the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inverse_matrix <- NULL
    
    set <- function(y)
    {
        x <<- y
        inverse_matrix <<- NULL
    }
    
    get <- function() {x}
    
    set_inverse <- function(inverse) {inverse_matrix <<- inverse}
    get_inverse <- function() {inverse_matrix}
    
    list(set = set, get = get, set_inverse = set_inverse , get_inverse = get_inverse)
}


## Write a short comment describing this function
## Function to call inverse matrix if already present in cache else create inverse of matrix 

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inverse_matrix <- x$get_inverse()
    
    if(!is.null(inverse_matrix)) 
    {
        message("Getting cached data for inverse matrix")
        return(inverse_matrix)
    }
    
    data <- x$get()
    inverse_matrix <- solve(data)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
