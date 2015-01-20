## Commentaries are shown in English and Spanish
## Set of Functions designed to create a list that sets an "special" list of functions and data matrix
## and calculate the inverse of the matrix, if the result have been calculated then is get it from cache if not, calculation on the fly.

## Conjunto de funciones utilizadas para crear una lista especial de datos de matrices y funciones
## así como el cálculo del inverso de la matriz, si ya fue calculado se obtiene del cache, si no, se obtiene al vuelo.


## Creates an special list containing th matrix and functions to set and get the matrix and its inverse
## Crea una lista especial que contiene la matriz y funciones para establecer y llamar a la matriz y a su inverso.

makeCacheMatrix <- function(x = matrix()) {
        Inv<-NULL
        Set <- function(y){
                x<<-y
                Inv<<-NULL
        }
        Get <- function() x
        SetInverse <- function(inverse) Inv<<-inverse
        GetInverse <- function() Inv
        list(Set=Set, Get=Get,
             SetInverse=SetInverse,
             GetInverse=GetInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv<- x$GetInverse()
        if (!is.null(Inv)){
                message("getting cache data")
                return(Inv)
        }
        data <- x$Get()
        Inv<- solve(data,...)
        x$SetInverse(Inv)
        Inv
}
