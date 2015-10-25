##Programming Assignment 2
##Coursera: R programming (signature track)
##date: 24th of Oct 2015

##There are 2 functions in this file. They used the R scoping rules to cache time-consuming
##computation of inverse of a matrix. The first function creates a special object that can 
##cache the inverse of a matrix. The second one retrieve  the inverse if the matrix has not been
##changed or calculate it if the matrix has been changed.

##makeCacheMatrix
##inputs: square invertible  matrix
##outputs: a special  object 'matrix' that can hold the input  inverse and functions to access it

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL #local variable, stores the inverse  matrix of the arg x
    
    # function to reset the input matrix and set the inv to NULL  
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    #function, reterning the matrix  
    get <-function() x
    #setting the invers
    setInv <- function(inver) inv <<- inver
    # getting the invers
    getInv <- function() inv
    
    #the list of functions to be returned
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
    
}


##cacheSolve
##inputs: special object 'matrix' that caches its inverse
##outputs: returns the inverse if the original matrix has not been changed 
##         calculate the inverse if the original matrix has been changed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of the matrix
    ## used to create the special matrix 'x'
    
    #first, read the invers stored in the local variable in makeCacheMatrix 
    inv <- x$getInv()
    #if it is not NULL, return it
    if (!is.null(inv)){
        message("getting inverse")
        return(inv)
    }
    
    #if it is NULL, then calculate  it
    mtx <- x$get()    #first, retrieve the matrix from 'x'
    inv <- solve(mtx, ...) #calculate the inverse
    x$setInv(inv)     #set the inverse into the makeCacheMarix to read next time
    inv    #return the inverse
}
