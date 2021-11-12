
require(MASS)

generate_pairs <- function( n ) {
  # Generate n pairs of financial returns.
  muX <- 2; muY <- -1;
  CovMx <- matrix( c(1,-.25,-.25,2), nrow = 2); 
  data <- mvrnorm(n=100, mu=c(muX,muY), Sigma=CovMx);
  return( data.frame( 'X'=data[,1], 'Y'=data[,2]) );
}
