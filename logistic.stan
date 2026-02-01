data {
  int<lower=1> Nobs;
  int<lower=1> Nout;
  int<lower=1> Nind;
  vector[Nobs] t;
  real y[Nobs,Nout];
  array[Nobs,Nout] int<lower=0,upper=1> y_obs;
  array[Nobs] int<lower=1,upper=Nind> jj;
}
parameters {
  real<lower=0> lscale[Nout];
  real<lower=0> lpos[Nout];
  real<lower=0> lamp[Nout];
  real<lower=0> sigmaerror;
}
model {
  real mu[Nobs,Nout];
  sigmaerror ~ inv_gamma(3,0.5);
  lscale ~ normal(5, 1);
  lpos ~ normal(70, 30);
  lamp ~ normal(2, 1);
  for (n in 1:Nobs){
    for (k in 1:Nout){
      mu[n,k] = inv_logit((t[n]-lpos[k])/lscale[k])*lamp[k];
      if (y_obs[n,k]){
        y[n,k] ~ normal(mu[n,k],sqrt(sigmaerror));
      }
    }
  }
}
