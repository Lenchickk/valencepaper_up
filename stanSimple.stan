data {
  int<lower=0> P; // number of parties
  int<lower=0> R; //number of regions
  int<lower=0> N; // number of observatios
  real sec[N]; //nationalistic individual scores
  real lr[N]; //left-right individual score
  int<lower=1,upper=R> region[N]; //region of an individual
  int<lower=1, upper=P> vote[N]; // vote of an individual
  real psec[P];// nationalistic party score
  real plr[P];//left-right party score
  int<lower=0, upper=1> phi[R,P];
  
}
parameters {
  real muu[P]; 
  real beta1[R];
  real beta2[R];
  real mu[R,P];
}

model
{
    real v[N,P];
    real expv[N,P];
    vector[N] pv[P];
    real taul;
    real taum;
    real tbeta1;
    real tbeta2;
    
    for (i in 1:N) {
        for (p in 1:P) {
            v[i, p] <- muu[p] - beta1[region[i]] * (sec[i] - 
                psec[p])^2 - beta2[region[i]] * (lr[i] - plr[p])^2 + mu[region[i], p];
            expv[i, p] <- exp(v[i, p]) * phi[region[i], p];
            pv[i, p] <- expv[i, p]/sum(expv[i]);
        }
        vote[i] ~ categorical(pv[i]);
    }
        for (p in 2:P) {
        muu[p] ~ normal(0.00000E+00, taul);
    }
    
    for (r in 1:R) {
        for (p in 1:P) {
            mu[r, p] ~ normal(0.00000E+00, taum);
        }
    }
    
    for (r in 1:R) {
        beta1[r] ~ normal(0.00000E+00, tbeta1);
    }
    for (r in 1:R) {
        beta2[r] ~ normal(0.00000E+00, tbeta2);
    }
    
    taum ~ gamma(0.1, 0.1);
    taul ~ gamma(0.1, 0.1);
    tbeta1 ~ gamma(0.1, 0.1);
    tbeta2 ~ gamma(0.1, 0.1);
}
    
    muu[1] <- 0.00000E+00;

