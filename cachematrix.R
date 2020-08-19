## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                               
        set <- function(y){                             #set value of matrix with FUN y
                x <<- y                                 #<<- to assign value to object in an environment different from current env
                i <<- NULL
        }
        get <- function(){x}                            #get value of the matrix
        setInverse <- function(inverse) i <<- inverse   #set value of inverse
        getInverse <- function() i                      #gets value of inverse
        list(set = set, get = get,                      #create list to display values
             setInverse = setInverse, 
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()                             #returns matrix inverse of x and assigns it to i
        if(!is.null(i)){                                #when i is coming from chache message "getting cached data" is returned
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat,...)
        x$setInverse(i)                                 #set value of inverse in the cache with setInverse
        i
}
