#' calculate the price of American option by diffrence method
#'
#' @param D expiration date
#' @param sigma Annual volatility
#' @param S0 current price of underlying assets
#' @param r Continuously compounded risk-free interest rate
#' @param q Bonus rate
#' @param K strike price
#' @param type call option:"C" or "c"; put option: "p" or "P"
#'
#' @return current price of the American option price
#' @export
#'
#' @examples
#' Aoption(30,0.1,1,0.1,0.1,1,"c")
Aoption <- function(D,sigma,S0,r,q,K,type){
  K <- K/S0
  # Select dx and dt that satisfy the convergence theorem
  dx <- min(floor(100*sigma^2/(r-q-sigma^2/2))/100,0.1)
  dt <- min(floor(10*dx^2/sigma^2)/10,0.1)
  N <- ceiling(D/dt)
  dt <- D/N
  #calculate the price of the call/put option at the expiration date
  V <- matrix(0,nrow = 2*N+1, ncol = N+1)
  if(type == "c" | type == "C") {
    for (i in 1:(2*N+1)) {
      V[i,N+1]=max(exp((i-N-1)*dx)-K,0)
    }
  } else if( type == "p" | type == "P") {
    for (i in 1:(2*N+1)) {
      V[i,N+1]=max(K-exp((i-N-1)*dx),0)
    }
  } 
  #Use explicit difference format to derive results
  a <- sigma^2*dt/dx^2
  k1 <- 1-a
  k2 <- (a+r-q-a*dx/2)/2
  k3 <- (a-r+q+a*dx/2)/2
  k0 <- 1+r*dt
  for (n in N:1) {
    for (m in (N+2-n):(n+N)) {
      # American option should decide whether to execute at any t
      V[m,n] <- max((k1*V[m,n+1]+k2*V[m+1,n+1]+k3*V[m-1,n+1])/k0,V[m,N+1])
    }
  }
  #output
  V0 <- S0*V[N+1,1]
  return(V[N+1,1])
}
