data {
  int<lower=0> P; // number of parties
  int<lower=0> R; //number of regions
  int<lower=0> N; // number of observatios
  int<lower=1,upper=R> region[N]; //region of an individual
  int<lower=1, upper=P> vote[N]; // vote of an individual
  int<lower=0, upper=1> phi[R,P];
  real sec[N]; //nationalistic individual scores
  real lr[N]; //left-right individual score
  real psec[P];// nationalistic party score
  real plr[P];//left-right party score
}
parameters {
  real muu[P]; 
  real beta1[P];
  real beta2[P];
}

transformed parameters {
  real v[N,P];
  real expv[N,P];
  vector[N] pv[P];
 
  for (i in 1:N) {
        for (p in 1:P) {
           v[i, p] <- muu[p] - beta1[region[i]] * (sec[i] - 
                psec[p])^2 - beta2[region[i]] * (lr[i] - plr[p])^2 ;
            expv[i, p] <- exp(v[i, p]) * phi[region[i], p];
            pv[i, p] <- expv[i, p]/sum(expv[i]);
        }
    }
      
  
}

model
{

    for (p in 1:P) {
        muu[p] ~ normal(0.00000E+00, 1);
    }
    
    for (r in 1:R) {
        beta1[r] ~ normal(0.00000E+00, 1);
        beta2[r] ~ normal(0.00000E+00, 1);
    }
     
    vote ~ categorical(pv);
    
}


 