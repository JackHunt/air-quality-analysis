data {
  int<lower=1> p;
  int<lower=1> n;
  int<lower=1> N;
  int<lower=1> N_test;

  array[N] real y;
  array[N_test] real y_test;
}

parameters {
  real alpha;
  array[p] real beta;
  real<lower=0.01> sigma;
}

model {
  alpha ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);

  for (j in (p + 1) : N) {
    real mu = alpha;

    for (k in 1 : p) {
      mu += beta[k] * y[j - k];
    }

    y[j] ~ normal(mu, sigma);
  }
}

generated quantities {
  array[N] real y_pred;
  array[N_test] real y_test_pred;

  if (N_test > N) {
    fatal_error("There must be more fit than test points:", N, N_test);
  }

  y_pred = rep_array(0.0, N);
  y_test_pred = rep_array(0.0, N_test);

  for (i in 1 : p) {
    y_pred[i] = y[i];
    y_test_pred[i] = y_test[i];
  }

  for (j in (p + 1) : N) {
    for (k in 1 : p) {

      y_pred[j] += beta[k] * y[j - k];

      if (j <= N_test) {
        if (k < (p - n)) {
          y_test_pred[j] += beta[k] * y_test[j - k];
        } else {
          y_test_pred[j] += beta[k] * y_test_pred[j - k];
        }
      }
    }
  }
}
