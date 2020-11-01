
functions {
  real[] cumu_phenos(  
                        int tmax,           // number of time steps to recurse
                        real[] a,           // in-group affinity [aA, aB]
                        real[] b,           // out-group coord bonus [bA, bB]
                        real m,             // xcult comp maint cost
                        real i,             // identity valuation
                        real c,             // cog diss cost
                        real mu,            // payoff bias for copying
                        real[] p            // initial pheno freqs [pA11, pA1X, pA2X, pA22, pB11, pB1X, pB2X, pB22]
                       ) {

    real aA = a[1];
    real aB = a[2];

    real bA = b[1];
    real bB = b[2];

    real pA11 = p[1];   //init pheno freqs
    real pA1X = p[2];
    real pA2X = p[3];
    real pA22 = p[4];

    real pB11 = p[5];
    real pB1X = p[6];
    real pB2X = p[7];
    real pB22 = p[8];

    // initialize phenotype freqs in next time step
    real pA11prime = 0;
    real pA1Xprime = 0;
    real pA22prime = 0;
    real pA2Xprime = 0;

    real pB22prime = 0;
    real pB2Xprime = 0;
    real pB11prime = 0;
    real pB1Xprime = 0;


    for (t in 1:tmax) {

        // perceived frequencies
        real poA1in = pA11*(pA11 + pA1X + pA2X + pA22) + pA1X*(pA11 + pA1X + 0.5*pA2X) + pA2X*(pA11 + 0.5*pA1X);
        real poA1out = pA11*(pB11 + pB1X + pB2X + pB22) + pA1X*(pB11 + pB1X + 0.5*pB2X) + pA2X*(pB11 + 0.5*pB1X);
        real poB2in = pB22*(pB22 + pB2X + pB1X + pB11) + pB2X*(pB22 + pB2X + 0.5*pB1X) + pB1X*(pB22 + 0.5*pB2X);
        real poB2out = pB22*(pA22 + pA2X + pA1X + pA11) + pB2X*(pA22 + pA2X + 0.5*pA1X) + pB1X*(pA22 + 0.5*pA2X);


        // perceived payoffs
        real wA11tilde = aA*poA1in + (1 - aA)*(1 - poB2out)*(1 + bA) + i*poA1in*poB2out;
        real wA1Xtilde = aA*(poA1in + (1 - poA1in)*(1 - c)) + (1 - aA)*((1 - poB2out)*(1 + bA) + poB2out*(1 + bA - c)) - m + i*poA1in*poB2out;
        real wA2Xtilde = aA*((1 - poA1in) + poA1in*(1 - c)) + (1 - aA)*(poB2out*(1 + bA) + (1 - poB2out)*(1 + bA - c)) - m + i*(1 - poA1in)*(1 - poB2out);
        real wA22tilde = aA*(1 - poA1in) + (1 - aA)*poB2out*(1 + bA) + i*(1 - poA1in)*(1 - poB2out);

        real wB11tilde = aB*(1 - poB2in) + (1 - aB)*poA1out*(1 + bB) + i*(1 - poB2in)*(1 - poA1out);
        real wB1Xtilde = aB*((1 - poB2in) + poB2in*(1 - c)) + (1 - aB)*(poA1out*(1 + bB) + (1 - poA1out)*(1 + bB - c)) - m + i*(1 - poB2in)*(1 - poA1out);
        real wB2Xtilde = aB*(poB2in + (1 - poB2in)*(1 - c)) + (1 - aB)*((1 - poA1out)*(1 + bB) + poA1out*(1 + bB - c)) - m + i*poB2in*poA1out;
        real wB22tilde = aB*poB2in + (1 - aB)*(1 - poA1out)*(1 + bB) + i*poB2in*poA1out;


        // total transition probability
        real pA11tildetot = inv_logit((mu*(wA11tilde - wA1Xtilde)))*inv_logit((mu*(wA11tilde - wA2Xtilde))) + inv_logit((mu*(wA1Xtilde - wA11tilde)))*inv_logit((mu*(wA1Xtilde - wA2Xtilde))) + inv_logit((mu*(wA2Xtilde - wA11tilde)))*inv_logit((mu*(wA2Xtilde - wA1Xtilde)));
        real pA22tildetot = inv_logit((mu*(wA22tilde - wA1Xtilde)))*inv_logit((mu*(wA22tilde - wA2Xtilde))) + inv_logit((mu*(wA1Xtilde - wA22tilde)))*inv_logit((mu*(wA1Xtilde - wA2Xtilde))) + inv_logit((mu*(wA2Xtilde - wA22tilde)))*inv_logit((mu*(wA2Xtilde - wA1Xtilde)));
        real pB22tildetot = inv_logit((mu*(wB22tilde - wB2Xtilde)))*inv_logit((mu*(wB22tilde - wB1Xtilde))) + inv_logit((mu*(wB2Xtilde - wB22tilde)))*inv_logit((mu*(wB2Xtilde - wB1Xtilde))) + inv_logit((mu*(wB1Xtilde - wB22tilde)))*inv_logit((mu*(wB1Xtilde - wB2Xtilde)));
        real pB11tildetot = inv_logit((mu*(wB11tilde - wB2Xtilde)))*inv_logit((mu*(wB11tilde - wB1Xtilde))) + inv_logit((mu*(wB2Xtilde - wB11tilde)))*inv_logit((mu*(wB2Xtilde - wB1Xtilde))) + inv_logit((mu*(wB1Xtilde - wB11tilde)))*inv_logit((mu*(wB1Xtilde - wB2Xtilde)));


        // phenotype freqs in next time step
        pA11prime = aA*pA11*(pA11 + pA1X + pA2X + pA22*(inv_logit((mu*(wA11tilde - wA1Xtilde)))*inv_logit((mu*(wA11tilde - wA2Xtilde))))/pA11tildetot) + (1 - aA)*pA11*(pB11 + pB1X + pB2X + pB22*(inv_logit((mu*(wA11tilde - wA1Xtilde)))*inv_logit((mu*(wA11tilde - wA2Xtilde))))/pA11tildetot);
        pA1Xprime = aA*pA11*pA22*(inv_logit((mu*(wA1Xtilde - wA11tilde)))*inv_logit((mu*(wA1Xtilde - wA2Xtilde))))/pA11tildetot + (1 - aA)*pA11*pB22*(inv_logit((mu*(wA1Xtilde - wA11tilde)))*inv_logit((mu*(wA1Xtilde - wA2Xtilde))))/pA11tildetot + aA*pA1X*(pA11 + pA1X) + aA*pA1X*pA2X*(0.5*1 + 0.5*inv_logit((mu*(wA1Xtilde - wA2Xtilde)))) + aA*pA1X*pA22*inv_logit((mu*(wA1Xtilde - wA2Xtilde))) + (1 - aA)*pA1X*(pB11 + pB1X) + (1 - aA)*pA1X*pB2X*(0.5*1 + 0.5*inv_logit((mu*(wA1Xtilde - wA2Xtilde)))) + (1 - aA)*pA1X*pB22*inv_logit((mu*(wA1Xtilde - wA2Xtilde))) + aA*pA2X*pA11*inv_logit((mu*(wA1Xtilde - wA2Xtilde))) + aA*pA2X*pA1X*(0.5*0 + 0.5*inv_logit((mu*(wA1Xtilde - wA2Xtilde)))) + (1 - aA)*pA2X*pB11*inv_logit((mu*(wA1Xtilde - wA2Xtilde))) + (1 - aA)*pA2X*pB1X*(0.5*0 + 0.5*inv_logit((mu*(wA1Xtilde - wA2Xtilde)))) + aA*pA22*pA11*(inv_logit((mu*(wA1Xtilde - wA22tilde)))*inv_logit((mu*(wA1Xtilde - wA2Xtilde))))/pA22tildetot + (1 - aA)*pA22*pB11*(inv_logit((mu*(wA1Xtilde - wA22tilde)))*inv_logit((mu*(wA1Xtilde - wA2Xtilde))))/pA22tildetot;
        pA22prime = aA*pA22*(pA22 + pA2X + pA1X + pA11*(inv_logit((mu*(wA22tilde - wA1Xtilde)))*inv_logit((mu*(wA22tilde - wA2Xtilde))))/pA22tildetot) + (1 - aA)*pA22*(pB22 + pB2X + pB1X + pB11*(inv_logit((mu*(wA22tilde - wA1Xtilde)))*inv_logit((mu*(wA22tilde - wA2Xtilde))))/pA22tildetot);
        pA2Xprime = 1 - pA11prime - pA1Xprime - pA22prime;

        pB22prime = aB*pB22*(pB22 + pB2X + pB1X + pB11*(inv_logit((mu*(wB22tilde - wB2Xtilde)))*inv_logit((mu*(wB22tilde - wB1Xtilde))))/pB22tildetot) + (1 - aB)*pB22*(pA22 + pA2X + pA1X + pA11*(inv_logit((mu*(wB22tilde - wB2Xtilde)))*inv_logit((mu*(wB22tilde - wB1Xtilde))))/pB22tildetot);
        pB2Xprime = aB*pB22*pB11*(inv_logit((mu*(wB2Xtilde - wB22tilde)))*inv_logit((mu*(wB2Xtilde - wB1Xtilde))))/pB22tildetot + (1 - aB)*pB22*pA11*(inv_logit((mu*(wB2Xtilde - wB22tilde)))*inv_logit((mu*(wB2Xtilde - wB1Xtilde))))/pB22tildetot + aB*pB2X*(pB22 + pB2X) + aB*pB2X*pB1X*(0.5*1 + 0.5*inv_logit((mu*(wB2Xtilde - wB1Xtilde)))) + aA*pB2X*pB11*inv_logit((mu*(wB2Xtilde - wB1Xtilde))) + (1 - aB)*pB2X*(pA22 + pA2X) + (1 - aB)*pB2X*pA1X*(0.5*1 + 0.5*inv_logit((mu*(wB2Xtilde - wB1Xtilde)))) + (1 - aB)*pB2X*pA11*inv_logit((mu*(wB2Xtilde - wB1Xtilde))) + aB*pB1X*pB22*inv_logit((mu*(wB2Xtilde - wB1Xtilde))) + aB*pB1X*pB2X*(0.5*0 + 0.5*inv_logit((mu*(wB2Xtilde - wB1Xtilde)))) + (1 - aB)*pB1X*pA22*inv_logit((mu*(wB2Xtilde - wB1Xtilde))) + (1 - aB)*pB1X*pA2X*(0.5*0 + 0.5*inv_logit((mu*(wB2Xtilde - wB1Xtilde)))) + aB*pB11*pB22*(inv_logit((mu*(wB2Xtilde - wB11tilde)))*inv_logit((mu*(wB2Xtilde - wB1Xtilde))))/pB11tildetot + (1 - aB)*pB11*pA22*(inv_logit((mu*(wB2Xtilde - wB11tilde)))*inv_logit((mu*(wB2Xtilde - wB1Xtilde))))/pB11tildetot;
        pB11prime = aB*pB11*(pB11 + pB1X + pB2X + pB22*(inv_logit((mu*(wB11tilde - wB1Xtilde)))*inv_logit((mu*(wB11tilde - wB2Xtilde))))/pB11tildetot) + (1 - aB)*pB11*(pA11 + pA1X + pA2X + pA22*(inv_logit((mu*(wB11tilde - wB1Xtilde)))*inv_logit((mu*(wB11tilde - wB2Xtilde))))/pB11tildetot);
        pB1Xprime = 1 - pB22prime - pB2Xprime - pB11prime;


        //recurse
        pA11 = pA11prime;   
        pA1X = pA1Xprime;
        pA2X = pA2Xprime;
        pA22 = pA22prime;

        pB11 = pB11prime;
        pB1X = pB1Xprime;
        pB2X = pB2Xprime;
        pB22 = pB22prime;

    }; //for t
    
    return {pA11prime,pA1Xprime,pA2Xprime,pA22prime,pB11prime,pB1Xprime,pB2Xprime,pB22prime};
  
  } //cumu_phenos
}

data {
  int<lower=1> N_ma;                    // number of machi interviewees for question 9 (observations)
  int<lower=1> N_me;                    // number of mestizo interviewees for question 9 (observations)
  int<lower=1,upper=4> y_ma[N_ma];      // phenotype (1=11, 2=1X, 3=2X, 4=22) of machi interviewee n
  int<lower=1,upper=4> y_me[N_me];      // phenotype (1=11, 2=1X, 3=2X, 4=22) of mestizo interviewee n

  int<lower=1> t_in;                    // number of time steps to recurse theoretical model
  real<lower=0,upper=1> init_freqs[8];  // initial pheno freqs for theoretical model: [pA11,pA1X,pA2X,pA22,pB11,pB1X,pB2X,pB22]
}


parameters {
  real<lower=0,upper=1> aB;           // in-group affinity for group B
  real<lower=0> bB;                   // out-group coord bonus for group B
  real<lower=0> m;                    // xcult comp maint cost
  real<lower=0> i;                    // identity valuation
  real<lower=0,upper=1> c;            // cog diss cost
  real<lower=0> mu;                   // payoff bias for copying
  real<lower=0> bA_bonus;             // amount by which bA > bB
}

transformed parameters {
  simplex[4] model_output[2];               //temporary container matrices to hold output of model function (pheno freqs) for each ethnicity (row 1 = machi, row 2 = mestizo)

  real<lower=0,upper=1> a_constr[2];        // theoretical model: a's and b's with constraints
  real<lower=0> b_constr[2];

  a_constr[1] = 1 - 2*( 1 - aB );           // a_constr = [aA,aB] such that (1-aB)/(1-aA)=1/2
  a_constr[2] = aB;
  
  b_constr[1] = bB + bA_bonus;              // b_constr = [bA,bB] such that bA > bB
  b_constr[2] = bB; 

  model_output[1] = to_vector( cumu_phenos(t_in,a_constr,b_constr,m,i,c,mu,init_freqs) )[1:4];   //a 4-simplex with modeled pheno freqs: [pA11, pA1X, pA2X, pA22]
  model_output[2] = to_vector( cumu_phenos(t_in,a_constr,b_constr,m,i,c,mu,init_freqs) )[5:8];
}

model { 
  aB ~ uniform(0,1);                                                                                             
  bB ~ exponential(1);
  m ~ exponential(1);
  i ~ exponential(1);
  c ~ uniform(0,1);
  mu ~ exponential(1);
  bA_bonus ~ exponential(1);

  y_ma ~ categorical(model_output[1]);
  y_me ~ categorical(model_output[2]);
}



