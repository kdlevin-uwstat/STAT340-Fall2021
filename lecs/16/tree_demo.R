
# Generate demo data illustrating when linear/logistic regression can fail.
x <- rnorm( 1000, mean=0, sd=sqrt(sigma2) );
xtransformed <- exp(-(x^2-5))/(1+exp(-(x^2-5)));

for( i in 1:length(x) ) {
  y[i] <- rbinom(n=1, size=1, prob=xtransformed[i] );
}

df <- data.frame( 'x'=x, 'y'=y );
write.csv( df, file='binary_response.demo.csv', row.names=FALSE );
