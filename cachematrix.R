
##creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( mat = matrix()){
	    
	  inverse <- NULL

        set <- function(y) {
                mat <<- y
                inverse <<- NULL
        }
        get <- function() mat 

	  setinverse <- function() inverse <<- solve(mat)
		
	  getinverse <- function() inverse

	  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##this function checks if 2 matrices are equal
matequal <- function(x, y){
	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}

## call the special object 
cm  <- makeCacheMatrix()

## computes the inverse of the special "matrix" cm returned by makeCacheMatrix above
	## if the inverse has already been calculated (and the matrix has not changed), 
	##   then the cachesolve should retrieve the inverse from the cache.
## it returnes the input matrix, a messaje to show if the inverse matrix is retrieved from cache

cacheSolve<- function(mat){
		
	print(mat)

	inverse <- cm$getinverse()

	if(!is.null(cm$getinverse()) && matequal(cm$get(),mat)) {
                message("getting cached matrix inversed:")
                return(inverse)
        }	
	else{
		message("setting matrix inversed in cache")
		cm$set(mat)
		cm$setinverse()
		return(cm$getinverse())
	}		
}


##you can verify the code in the R console:

## step 1: call cacheSolve( cbind(c(1,0,5), c(2,1,6), c(3,4,0)))
## it will return 
##     			[,1] [,2] [,3]
##			[1,]    1    2    3
##			[2,]    0    1    4
##			[3,]    5    6    0
## getting cached matrix inversed:
##			     [,1] [,2] [,3]
##			[1,]  -24   18    5
##			[2,]   20  -15   -4
##			[3,]   -5    4    1

## step 2: call again cacheSolve( cbind(c(1,0,5), c(2,1,6), c(3,4,0)))
## it will return 
##     			[,1] [,2] [,3]
##			[1,]    1    2    3
##			[2,]    0    1    4
##			[3,]    5    6    0
## getting cached matrix inversed:
##			     [,1] [,2] [,3]
##			[1,]  -24   18    5
##			[2,]   20  -15   -4
##			[3,]   -5    4    1

## step 2: call cacheSolve( cbind(c(1,0,5), c(2,1,6), c(3,4,1))) - the matrix has changed
## it will return 
##			     [,1] [,2] [,3]
##			[1,]    1    2    3
##			[2,]    0    1    4
##			[3,]    5    6    1
##setting matrix inversed in cache
##			      [,1] [,2] [,3]
##			[1,] -11.5    8  2.5
##			[2,]  10.0   -7 -2.0
##			[3,]  -2.5    2  0.5

	
