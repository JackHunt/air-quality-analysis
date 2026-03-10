data {
  int<lower=0> p;
  int<lower=0> N;

  array[N] real y;
}

parameters {
  real alpha;
  array[p] real beta;
  real sigma;
}

model {
  for (n in (p + 1) : N) {
    real mu = alpha;

    for (k in 1 : p) {
      mu += beta[k] * y[n - k];
    }

    y[n] ~ normal(mu, sigma);
  }
}

generated quantities {
  array[N] real y_pred;

  for (n in (p + 1) : N) {
    for (k in 1 : p) {
      y_pred[n] += beta[k] * y[n - k];
    }
  }
}
