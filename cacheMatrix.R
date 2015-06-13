## Functions that cache the inverse of a matrix
##
## Usage example:
##
## source('cachematrix.R')
## m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## cacheSolve(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric())
{

	## Initialize the inverse property

	cache = NULL

	## Method to set the matrix

	setMatrix <- function(newValue)
	{
		x <<- newValue
		cache <<- NULL
	}
	
	## Method the get the matrix
	
	getMatrix <- function()
	{
		## Return the matrix
		x
	}

	## Method to set the inverse of the matrix

	cacheInverse <- function(solve)
	{
		cache <<- solve
	}

	## Method to get the inverse of the matrix

	getInverse <- function()
	{

		## Return the inverse property
		cache
	}

	## Return a list of the methods

	list( setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(y, ...)
{

	## Return a matrix that is the inverse of 'x'

	inverse <- y$getInverse()

	## Just return the inverse if its already set

	if(!is.null(inverse))
	{
		message("getting cached data")
		return(inverse)
	}

	## Get the matrix from our object

	data <- y$getMatrix()

	## Calculate the inverse using matrix multiplication

	inverse <- solve(data)
	
	## Get the matrix from our object

	y$cacheInverse(inverse)

	## Return the matrix
	inverse
}
