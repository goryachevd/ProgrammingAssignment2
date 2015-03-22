# This file contains two functions: makeCacheMatrix and cacheSolve.
# These functions accept a matrix, compute its inverse, and store the inverse in cache. 
# If a user tries to get an inverse of the same matrix, the value will be taken from cache, instead of being calculated anew. 


# Function makeCacheMatrix accepts as its argument a matrix and returns 
# a list containing four functions (set/get/setinverse/getinverse)

makeCacheMatrix <- function(x = matrix()) {                         # Function makeCacheMatrix begins, accepts matrix as argument 
    inverse    <- NULL                                          # Set the local inverse to null 
    set        <- function(y) {                            
        x       <<- y                               # Set function: Set x = y in the parent environment
        inverse <<- NULL                            # Set inverse value to NULL in the parent environment
    }
    get        <- function() x                                  # Function: Return x
    setinverse <- function(inverse_val) inverse <<- inverse_val # Function: assign inverse = inverse_val in the parent environment
    getinverse <- function() inverse                            # Function: Return inverse
    
    # Create a list with 4 elements/arguments                         
    list(       set = set         ,                             # set function   
                get = get         ,                             # Get function
                setinverse = setinverse  ,                             # setinverse function  
                getinverse = getinverse                                # getinverse function
    )
}

# Function cacheSolve determines if the inverse of the matrix has already been computed. 
# If yes, the cached value is returned, along with a message that the value came from cache. 
# If not, the inverse is computed and returned. 

cacheSolve <- function(x, ...) {                                    # Function cacheSolve begins
    inverse <- x$getinverse()                                       # Find if the inverse already exists 
    if(!is.null(inverse)) {                                     # If yes, then do:
        message("getting cached matrix inverse")            # Print message
        return(inverse)                                     # Return cached inverse
    }
    data    <- x$get()                                          # Obtain the matrix
    inverse <- solve(data, ...)                                 # Inverse the matrix via solve(x) 
    x$setinverse(inverse)                                       # Create an inverse in the parent environment       
    inverse                                                         # Return inverse  
}
