
# Define a function to do the model training and residual computation.
mtcars_fit_models <- function( holdout_idxs ) {
  
  # Split the mtcars data into train and the hold-out.
  train_data <- mtcars[ -holdout_idxs, ];
  leftout <- mtcars[ holdout_idxs, ];
  
  errors <- rep( NA, 5 ); # We're training up to order 5.
  
  # Fit the linear model, then evaluate.
  m1 <- lm(mpg ~ 1 + hp, train_data );
  m1.pred <- predict( m1, leftout );
  errors[1] <- mean( (m1.pred - leftout$mpg)^2 ); 
  
  # Fit the quadratic model, then evaluate.
  m2 <- lm(mpg ~ 1 + hp + I(hp^2), train_data );
  m2.pred <- predict( m2, leftout );
  errors[2] <- mean( (m2.pred - leftout$mpg)^2 ); 

  # Fit the cubic model, then evaluate.
  m3 <- lm(mpg ~ 1 + hp + I(hp^2) + I(hp^3), train_data );
  m3.pred <- predict( m3, leftout );
  errors[3] <- mean( (m3.pred - leftout$mpg)^2 ); 
  
  # Fit the 4-th order model, then evaluate.
  m4 <- lm(mpg ~ 1 + hp + I(hp^2) + I(hp^3) + I(hp^4), train_data );
  m4.pred <- predict( m4, leftout );
  errors[4] <- mean( (m4.pred - leftout$mpg)^2 ); 
  
  # Fit the 5-th order model, then evaluate.
  m5 <- lm(mpg ~ 1 + hp + I(hp^2) + I(hp^3) + I(hp^4) + I(hp^5), train_data );
  m5.pred <- predict( m5, leftout );
  errors[5] <- mean( (m5.pred - leftout$mpg)^2 ); 
  
  return( errors );
}
