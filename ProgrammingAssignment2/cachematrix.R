## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix -> Under normal conditions, returns a list with function attributes
## cacheSolve      -> Returns matrix inverse - optionally, computes it.  
##
## Write a short comment describing this function
## 0. Overall, this code follows the example provided in the course assignment page. 
##    Further, some details for matrix input are added. 
## 1. Checks whether the input satisfies basic conditions (sq n invertible matrix).
## 2. Does some basic checking to avoid arbitrary set_inverse requests.
## 3. Under normal conditions, returns a list object with the following 
##    function attributes: set, get, set_inverse, and get_inverse.
##
makeCacheMatrix <- function(x = matrix()) {
        # check if the matrix is square and is invertible...
        if( class(x) != class(matrix()) | nrow(x) != ncol(x) ) {
                message("Invalid input. Please supply a square invertible matrix.")
                return(x)
        } else if ( det(x) == 0 ) {
                message("Invalid input. Please supply a square invertible matrix.")
                return(x)
                }
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        set_inverse <- function(solved_mat) {
                # following checks are to avoid arbitrary inputs by users.
                if( class(solved_mat) != class(matrix()) ) {
                        message("Sorry, invalid input.")
                        return(solved_mat)
                } else if( nrow(solved_mat) != ncol(solved_mat) ) {
                        message("Sorry, invalid input.")
                        return(solved_mat)
                }
                inv_matrix <<- solved_mat
        }
        get_inverse <- function() inv_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}
##
## Write a short comment describing this function
## 0. Overall, this code follows the example provided in the course assignment page. 
##    Further, some details for matrix input are added. 
## 1. Returns inverse of matrix.
## 2. If the matrix inverse has already been cached, returns it - else 
##    computes matrix inverse. 
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {
                message("Getting cached data...")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$set_inverse( inv_matrix )
        inv_matrix
}
