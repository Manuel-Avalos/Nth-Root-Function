name = "Manuel Avalos"
#Stat 6260: R Programming
#11/1/2017 Fall

#I obtained Newton's uncoded algorithm for calculating the nth root from 
# https://en.wikipedia.org/wiki/Nth_root_algorithm
#
#Given a number (a) we want to approximate the (n)th root of (a)
# with respect to the the error tolerance (tol)
#This function will return an approximation of the (n)th root of (a)
############################################################################


nth.root <- function(a,n,tol){ 
  if(tol < 1e-15){
    return("tol too low") #if tol is too low the function will return an error message
  }
  else{
    x <- 1 #initial guess
    
    error = ((1/n)*((a/(x^(n-1)))-x))  #error from initial guess
    
    while(abs(error) > tol){ #activates if the absolute error is greater than the tol 
      
      x <- x + error  #replaces intial guess with the intial guess plus the error
      
      error <- ((1/n)*((a/(x^(n-1)))-x)) #updates the error after we calculate a new x
    }
    return(x) #returns the first x whose absolute error is less than the tol
              #this is the approximation of nth root of a
  }
}