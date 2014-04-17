#' Implement the Morris Method (1991)
#' 
#' This function will generate a random orientation matrix (B*) that is used to 
#' implement the Morris method.  
#' 
#' @param p controls delta, step size
#' @param k number of parameters
#' @return  list of 
#' \itemize{
#'          \item \code{order} The order in which the variables are changed
#'          \item \code{Bstar} The random permutation matrix
#'          }
#' @examples
#' mm1991(10,2)

mm1991<-function(p,k){
  
  #B is a lower triangular matrix of 1's
  B<-matrix(0,k+1,k)
  B[lower.tri(B)]<-1  
  
  #Dstar is a k-by-k diagonal matrix with entries +-1 with equal probability
  Dstar<-diag(sample(c(-1,1),k,replace=T))
  
  #Pstar is a random permutation matrix
  Pstar<-matrix(0,k,k)
  toOne<-sample(1:k)
  for(j in 1:k) Pstar[j,toOne[j]]<-1
  
  #delta is a predefined multiple of 1/(p-1)
  delta<-p/(2*p-2)
  
  #poss is the vector of possible values each x_i can take on
  poss<-seq(0,1-delta,by=1/(p-1))
  
  #xstar is the randomly generated 'base value' of x
  xstar<-matrix(sample(poss,k,replace=T),nrow=1)
  
  #Consturct the randomized Bstar matrix
  
  p1<-(delta/2)*((2*B-matrix(1,k+1,k))%*%Dstar+matrix(1,k+1,k))
  Bstar<-(matrix(1,k+1,1)%*%xstar+p1)%*%Pstar
  return(list(order=toOne,Bstar=Bstar))
}