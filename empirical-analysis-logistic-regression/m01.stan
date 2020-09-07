data {
  int<lower=1> J;                 // number of interviewees
  int<lower=1> T;                 // number of targets (ego, ingroup, outgroup)
  int<lower=1> N;                 // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];     // interviewee ID for observation n
  int<lower=1,upper=T> tt[N];     // target for observation n
  int<lower=0,upper=1> y[N];      // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];       //Matsigenka ethnicity: 1=yes, 2=no, for each obs n
  int<lower=1,upper=2> MachiIndiv[J];  //Matsigenka ethnicity: 1=yes, 2=no, for each indiv j
}

transformed data {
  vector[3] zeros = [0,0,0]';     //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[3] ones = [1,1,1]';      //column vectors of 1s
  vector[3] tens = [10,10,10]';      //column vectors of 10s
}

parameters {
  vector[T] zInt[J];                  //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];                 //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];     //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];   //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts
}

transformed parameters {

  matrix[J,T] off_Int;              //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                    //Stan manual pg 150-151, Rethinking pg 409

  for (j in 1:J) {
    off_Int[j] = (diag_pre_multiply(sigmaInt[MachiIndiv[j]], L_R_a[MachiIndiv[j]]) * zInt[j])';  //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  }

}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts

  zInt ~ multi_normal(zeros, diag_matrix(ones));      //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(tens));     //array of T-vectors of mean intercepts
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);                  // array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);               //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }



  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]';        //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector

    params[n] = alpha[tt[n]];                         //in the logit(x) link function, x=gamma(alpha-beta) where gamma=1 and beta=0 if just a single question

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;        //reconstructed intercept for each individual


  matrix[T,T] R_a[2];      //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];    //2-array of TxT variance-covariance matrices


  vector[N] log_lik;       //for WAIC
  vector[T] alpha;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]';

    log_lik[n] = bernoulli_logit_lpmf( y[n] | alpha[tt[n]] );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';                           //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';          //construct cov matrix from cholesky factors of cov matrix to look at in output
  } //for

}

