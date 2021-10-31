
data('mtcars');

# Add a column for each of the higher-order powers of hp.
mtcars$hp2 <- mtcars$hp^2;
mtcars$hp3 <- mtcars$hp^3;
mtcars$hp4 <- mtcars$hp^4;
mtcars$hp5 <- mtcars$hp^5;

# And let's recenter and renormalize those to ensure stability.
# If we don't do this, we're going to get some really big prediction
# errors in the 4-th and 5-th order models.
# Once again, there are more concise/clever ways to do this,
# but this is simple enough.
mtcars$hp <- (mtcars$hp - mean(mtcars$hp))/sd(mtcars$hp);
mtcars$hp2 <- (mtcars$hp2 - mean(mtcars$hp2))/sd(mtcars$hp2);
mtcars$hp3 <- (mtcars$hp3 - mean(mtcars$hp3))/sd(mtcars$hp3);
mtcars$hp4 <- (mtcars$hp4 - mean(mtcars$hp4))/sd(mtcars$hp4);
mtcars$hp5 <- (mtcars$hp5 - mean(mtcars$hp5))/sd(mtcars$hp5);

norders <- 5; # Fit 5 different model orders
nreps <- 10; # Number of times to repeat the CV experiment
nrows <- norders*nreps;
resids <- data.frame('Rep'=rep(1:nreps, norders),
				'Order'=rep(1:norders, each=nreps),
				'Error'=rep(NA, nrows ) );

# As usual in this course, we are doing this in a way that is readable,
# at the expense of concise and/or clever code.
# This is to ensure that this code is easy to understand, while bearing
# in mind that it is not necessarily the solution that most seasoned
# R users would implement.
for (i in 1:nreps ) {
  # Split the data randomly into two parts.
  S1 <- sample( nrow(mtcars), size=nrow(mtcars)-1, replace=FALSE );
  S2 <- -S1; # Indexing with a negative index set picks out the complement

  # Fit the linear model, but only on the data in our randsamp.
  m1 <- lm(mpg ~ 1 + hp, mtcars[S1,] );
  # evaluate the linear model on the rest.
  m1.preds <- predict( m1, mtcars[S2,] );
  # This idx variable will be used to pick out the row of resids that we want.
  idx <- (resids$Rep==i & resids$Order==1);
  resids[ idx, ]$Error <- mean( (m1.preds - mtcars[S2,]$mpg)^2 );

  # Fit the quadratic model on the data in our rand samp.
  m2 <- lm(mpg ~ 1 + hp + hp2, mtcars[S1,] );
  # evaluate the quadratic model on the rest.
  m2.preds <- predict( m2, mtcars[S2,] );
  idx <- (resids$Rep==i & resids$Order==2);
  resids[ idx, ]$Error <- mean( (m2.preds - mtcars[S2,]$mpg)^2 );

  # Fit and evaluate the cubic model.
  m3 <- lm(mpg ~ 1 + hp + hp2 + hp3, mtcars[S1,] );
  # evaluate the cubic model on the rest.
  m3.preds <- predict( m3, mtcars[S2,] );
  idx <- (resids$Rep==i & resids$Order==3);
  resids[ idx, ]$Error <- mean( (m3.preds - mtcars[S2,]$mpg)^2 );

  # Fit and evaluate the 4-th order model.
  m4 <- lm(mpg ~ 1 + hp + hp2 + hp3 + hp4, mtcars[S1,] );
  # evaluate the cubic model on the rest.
  m4.preds <- predict( m4, mtcars[S2,] );
  idx <- (resids$Rep==i & resids$Order==4);
  resids[ idx, ]$Error <- mean( (m4.preds - mtcars[S2,]$mpg)^2 );

  # Fit and evaluate the 5-th order model.
  m5 <- lm(mpg ~ 1 + hp + hp2 + hp3 + hp4 + hp5, mtcars[S1,] );
  # evaluate the cubic model on the rest.
  m5.preds <- predict( m5, mtcars[S2,] );
  idx <- (resids$Rep==i & resids$Order==5);
  resids[ idx, ]$Error <- mean( (m5.preds - mtcars[S2,]$mpg)^2 );
}

# Pick out just one set of replicates to illustrate a single "round" of CV.
resids_onerun <- data.frame( 'Order'=1:5, Error=resids[resids$Rep==1,]$Error);


