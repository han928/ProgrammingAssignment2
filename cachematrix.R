## makecache matrix only set input x, get input x, and set the inverse and get 
## the inverse of x, no caculation involved
## this function is to cache the matrix inverse

## cache the function if its not inverse already

makeCacheMatrix <- function(x = matrix()) {
    # create inverse_m that has nothing in it
    inverse_m <-NULL
    

    
    # set the input y into x 
    set <- function(y){
        x<<-y
        inverse_m <<- NULL
        
    }
    
    # get the value of the matrix
    get <-function() x
    
    # set the inverse of x by inputting the inverse matrix value in inverse_x
    set_inverse <- function(inverse_x) inverse_m <<- inverse_x
    
    # function to get the inverse matrix
    get_inverse <- function() inverse_m
    
    # function list
    list(set=set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
    
    
}


## caculate the inverse matrix if not cached in makeCachematrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # first try to get matrix inverse from makeCacheMatrix
    inverse_m <- x$get_inverse()
    
    # check if cached, if return is not NULL then return the cached value
    if(!is.null(inverse_m)){
        message("getting cache data")
        return (inverse_m)
    }
    
    # kind of else argument, first get matrix from makeCacheMatrix with get func
    m <- x$get()
    
    # use solve function to calculate the inverse
    inverse_m <- solve(m)
    
    # cache the value back to makeCacheMatrix
    x$set_inverse(inverse_m)
    # and return the inverse matrix
    inverse_m
    
}
