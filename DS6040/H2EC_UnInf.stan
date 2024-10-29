data {
    int<lower=0> nA;
    vector[nA] A_wine;
    int<lower=0> nF;
    vector[nF] F_wine;

}
parameters {
    real A_mu;
    real<lower=0> A_sigma;
    real F_mu;
    real<lower=0> F_sigma;
}
transformed parameters {
    real diff_mu;
    diff_mu = A_mu - F_mu;
    real diff_sigma;
    diff_sigma = A_sigma + F_sigma;
}
model {
  A_mu ~ normal(0, 1000);
  A_sigma ~ cauchy(0, 1000);
  F_mu ~ normal(0, 1000);
  F_sigma ~ cauchy(0, 1000);

  A_wine ~ normal(A_mu, A_sigma);
  F_wine ~ normal(F_mu, F_sigma);
}