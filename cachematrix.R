## Commentaries are shown first in English then in Spanish
## Se muestran comentarios primero en Inglés y posteriormente en Español

## Set of Functions designed to create a list that sets an "special" list of functions and data matrix
## and calculate the inverse of the matrix, if the result have been calculated then is get it from cache if not,
##calculation is on the fly.

## Conjunto de funciones utilizadas para crear una lista especial de datos de matrices y funciones
## así como el cálculo del inverso de la matriz, si ya fue calculado se obtiene del cache, si no, se obtiene al vuelo.

## Function that creates an special list containing th matrix and functions to set and get the matrix and its inverse
## Funcion que crea una lista especial que contiene la matriz y funciones para establecer y llamar a la matriz y a su inverso.
makeCacheMatrix <- function(x = matrix()) {
        ## Initializa Variable
        ##Inicialización de variable
        InvMat<-NULL
        ## This function is used to set or reset the values of the matrix, if it´s resetted then initializes InvMat top NULL
        ## Funcion utilizada para fijar o reestablecer la matriz, si es reestablecida se reinicializa InvMat a NULL
                Set <- function(y){
                x<<-y
                InvMat<<-NULL
        }
        ## Gets the matrix from the list
        ## Obtiene la matriz de la lista
        Get <- function() x
        ## Writes the Inverse to the environment thru InvMat Variable, that later we will use as cache
        ## Escribe el Inverso hacia el ambiente mediante la variable InvMat, que posteriormente se usará como caché
        SetInverse <- function(inverse) InvMat<<-inverse
        ## Function that makes the call to the InvMat environment variable or cache
        ## Funcion que llama a la variable InvMat del ambiente ó caché
        GetInverse <- function() InvMat
        ## Generates the formatted special list that contains functions and data (matrix) as function´s output
        ## Genera la lista en formato especial que contiene las funciones y los datos (matriz) como salida de la funcion
        list(Set=Set, Get=Get,
             SetInverse=SetInverse,
             GetInverse=GetInverse)
}

## Function that checks if the inversion calculation has been made, if done, calls Environment InvMat variable of cache, 
## if does not, execute calculation on the fly
## Función que revisa si el calculo de inversión fue hecho, si ya, llama la variabe de ambiente InvMat ó caché
## Si no, ejecuta el cálculo al vuelo.
cacheSolve <- function(x, ...) {
        ## Checks if the calculation has been done for a set of data, If it is then gets value from the "cache" thru InvMat and returns the result
        ## Revisa si ya se hizo el calculo para el conjunto de datos, Si lo hizo, obtiene el valor del cache a traves de la variable InvMat y regresa el resultado
        InvMat<- x$GetInverse()
        if (!is.null(InvMat)){
                message("Getting cache data")
                return(InvMat)
        }
        ## If the calculation has not been done, then gets the original matrix, calculate it´s inverse, 
        ## sets Cache thru InvMat Variable and return the results
        ## Si el cálculo no se ha hecho, entonces se obtiene la matriz original, se calcula el inverso, 
        ## se establece el Cache a través de la variable InvMat y se regresa el resultado
        data <- x$Get()
        InvMat<- solve(data,...)
        x$SetInverse(InvMat)
        InvMat
}

## END OF SET OF FUNCTIONS
## FIN DEL CONJUNTO DE FUNCIONES
