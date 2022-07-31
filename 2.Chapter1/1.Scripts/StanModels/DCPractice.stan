// saved as DC_Practice.stan

data {
  int N; // rows
  int I; // decisionmaker
  int P; // N attributes
  matrix[N, P] X;
  int start_index[I];
  int end_index[I];
  vector[N] choice;
}
parameters {
  vector[P] beta;
}
model {
  vector[N] probs;
  
  // priors 
  beta ~ normal(0, .75);
  
  // helper for likelihood
  for(i in 1:I) {
    probs[start_index[i]:end_index[i]] = softmax(X[start_index[i]:end_index[i]] * beta);
  }
  target += choice' *log(probs) + (1 - choice)' * log(1 - probs);
}
