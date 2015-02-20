## Matrix inversion is usually a costly computation and caching can be used to reduce the computation cost. 
## Instead of computing the inversion of a matrix that have been solved before repeatedly, caching allows you
## to look up for the inversion. This assignment takes advantage of the scoping rules of the R language
## to cache (preserve) matrix, inversion of a matrix inside an R object which will allow future look up.

## The makeCacheMatrix function creates a sepcial object that can cache its inverse.The functions inlcuded 
## will perform following tasks: set (store) and retrieve the matrix, set and retrieve the inversion of the 
## matrix. 

## create a matrix object that will take in a matrix to solve
makeCacheMatrix <- function(x = matrix()) {     
        
        ## Remove any cached matrix inversion
        x_inverse <- NULL 
        
        ## This function saves the new matrix to solve
        set_matrix <- function(matrix){
                x <<- matrix           ## New matrix is copied into variable matrix in parent scope
                x_inverse <<- NULL              ## Previous matrix inversion from parent scope is removed
        }
                
        ## This get method returns the matrix stored in the object
        get_matrix <- function() {
                x                  
        }
        
        ## This set method save the inversion of a matrix that has been solved
        set_matrix_inversion <- function(matrix_inversion) {
                x_inverse <<- matrix_inversion
        }
        
        ## This returns the calculated inversion
        get_matrix_inversion <- function()  x_inverse
        
        
        ## This is a list of functions that allows storing and retrieving matrix and its inversion
        list(set_matrix=set_matrix, 
             get_matrix = get_matrix, 
             set_matrix_inversion = set_matrix_inversion, 
             get_matrix_inversion = get_matrix_inversion)
        
}

## The cacheSolve function computes the inverse of the matrix returned by above makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix has not changed), then the function will retrieve 
## the inverse from the cache rather than calculating it again. The assumption we make here is that all 
## supplied matrices are invertible.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
                
        ## Retrieve the inversion of matrix x
        matrix_inversion <- x$get_matrix_inversion()
        
        ## Verified that the matrix has already been solved, retrieve the solution
        if(!is.null(matrix_inversion)){
                message("getting cached inversed matrix")
                return (matrix_inversion)
        }
        
        ## If the matrix has not been solved before, solve it, and then store the matrix and its inversion
        ## Cases where the given matrix is not square is taken care of
        
        data<- x$get_matrix()
        
        matrix_inversion <- solve(data) ##%*% data
        
        x$set_matrix_inversion(matrix_inversion)
        
        ## Return the inversion to the parent scope
        matrix_inversion
}
