## store the result of inverse matrix into cache in case needed again

## makeCacheMatrix is a function which stores information about a matrix and its inverse matrix.
## The function returns to a list that has four following elements
## 1.set - set the value of the matrix
## 2.get - reads the value of the matrix
## 3.set_inverse - set the value of the inverse matrix
## 4.get_inverse - get the value of the inverse matrix
makeCacheMatrix <- function(x=matrix()){
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <-function() x
    set_inverse <- function(inv_matrix) inverse <<- inv_matrix
    get_inverse <- function() inverse
    list <- list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
    return(list)
}

##cacheSolve is a function that tests if the problem was solved before.
##If so, return to that cache value directly. Otherwise, calculate it.
cacheSolve <- function(x,...){
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setmean(inverse)
    return(inverse)
}