data {
  int<lower=0> p;
  int<lower=0> N;
  int<lower=0> N_test;

  array[N] real y;
  array[N_test] real y_test;
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
  array[N_test] real y_test_pred;

  if (N_test > N) {
    fatal_error("There must be more fit than test points:", N, N_test);
  }

  for (n in (p + 1) : N) {
    for (k in 1 : p) {
      y_pred[n] += beta[k] * y[n - k];

      if (n <= N_test) {
        y_test_pred[n] += beta[k] * y_test[n - k];
      }
    }
  }
}
