############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


data_list <- list(
  N_ma = nrow(d9reduc[which(d9reduc$Machi==1),]),                             #number of machi interviewees (with phenotypes of interest)
  N_me = nrow(d9reduc[which(d9reduc$Machi==0),]),                             #number of mestizo interviewees (with phenotypes of interest)

  N_ma_edu = nrow(d9reduc[which(d9reduc$Machi==1 & d9reduc$EdMes==1),]),      #number of machi interviewees (with phenotypes of interest) and edu experience
  N_me_emp = nrow(d9reduc[which(d9reduc$Machi==0 & d9reduc$EmpMat==1),]),     #number of mestizo interviewees (with phenotypes of interest) and emp experience

  y_ma = d9reduc[which(d9reduc$Machi==1),"Pheno"],                            #vector of machi phenotypes
  y_me = d9reduc[which(d9reduc$Machi==0),"Pheno"],                            #vector of mestizo phenotypes

  y_ma_edu = d9reduc[which(d9reduc$Machi==1 & d9reduc$EdMes==1),"Pheno"],     #vector of machi phenotypes with edu experience
  y_me_emp = d9reduc[which(d9reduc$Machi==0 & d9reduc$EmpMat==1),"Pheno"],    #vector of mestizo phenotypes with emp experience

  t_in = 5,                                                                   #number of model function time steps to recurse
  init_freqs = c(0.9,0,0,0.1,                                                 #initial phenotype frequencies for theoretical model (A11,A1X,A2X,A22,B11,B1X,B2X,B22)
                 0.1,0,0,0.9)
)


start_list <- list(
  aB = 0.5,           # in-group affinity for group B
  bB = 0.01,          # out-group coordination bonus for group B
  m = 1,              # xcult comp learning cost
  i = 0.1,            # identity valuation
  c = 0.1,            # cog diss cost
  mu = 1,             # payoff bias for copying
  bA_bonus = 1        # amount by which bA > bB
)




############################### Run Stan models: m1, m2, where m1 models all machis and mestizos and m2 only those with inter-ethnic experience

####### m1

set.seed(1)
m1 <- stan( file=model_file[1],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.999, max_treedepth=15) #default treedepth is 10
            )

saveRDS(m1, "m1_fit.rds") #save stan output so you don't have re-run model
post1 <- extract.samples( m1 ) 

#pairs(m1, pars=c("aB","bB","m","i","c","mu")) #to see divergent transitions (in red)

print(m1, pars=c(
         "a_constr",
         "b_constr",
         "m",
         "i",
         "c",
         "mu",
         "bA_bonus",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

fitmod <- m1

#look at all traces
pdf(file="./Plots/traces_m1.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(traceplot(fitmod, pars="aB", inc_warmup=T))
    print(traceplot(fitmod, pars="bB", inc_warmup=T))
    print(traceplot(fitmod, pars="bA_bonus", inc_warmup=T))
    print(traceplot(fitmod, pars="m", inc_warmup=T))
    print(traceplot(fitmod, pars="i", inc_warmup=T))
    print(traceplot(fitmod, pars="c", inc_warmup=T))
    print(traceplot(fitmod, pars="mu", inc_warmup=T))
    print(traceplot(fitmod, pars="lp__", inc_warmup=T))

graphics.off()



####### m2

set.seed(1)
m2 <- stan( file=model_file[2],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.999, max_treedepth=15) #default treedepth is 10
            )

saveRDS(m2, "m2_fit.rds")
post2 <- extract.samples( m2 ) 

#pairs(m2, pars=c("aB","bB","m","i","c","mu"))

print(m2, pars=c(
         "a_constr",
         "b_constr",
         "m",
         "i",
         "c",
         "mu",
         "bA_bonus",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

fitmod <- m2

#look at all traces
pdf(file="./Plots/traces_m2.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(traceplot(fitmod, pars="aB", inc_warmup=T))
    print(traceplot(fitmod, pars="bB", inc_warmup=T))
    print(traceplot(fitmod, pars="bA_bonus", inc_warmup=T))
    print(traceplot(fitmod, pars="m", inc_warmup=T))
    print(traceplot(fitmod, pars="i", inc_warmup=T))
    print(traceplot(fitmod, pars="c", inc_warmup=T))
    print(traceplot(fitmod, pars="mu", inc_warmup=T))
    print(traceplot(fitmod, pars="lp__", inc_warmup=T))

graphics.off()



#### models m3 and m4 are just models m1 and m2 with the theoretical model recursed for 25 (rather than 5) time steps

data_list25 <- list(
  N_ma = nrow(d9reduc[which(d9reduc$Machi==1),]),                             #number of machi interviewees (with phenotypes of interest)
  N_me = nrow(d9reduc[which(d9reduc$Machi==0),]),                             #number of mestizo interviewees (with phenotypes of interest)

  N_ma_edu = nrow(d9reduc[which(d9reduc$Machi==1 & d9reduc$EdMes==1),]),      #number of machi interviewees (with phenotypes of interest) and edu experience
  N_me_emp = nrow(d9reduc[which(d9reduc$Machi==0 & d9reduc$EmpMat==1),]),     #number of mestizo interviewees (with phenotypes of interest) and emp experience

  y_ma = d9reduc[which(d9reduc$Machi==1),"Pheno"],                            #vector of machi phenotypes
  y_me = d9reduc[which(d9reduc$Machi==0),"Pheno"],                            #vector of mestizo phenotypes

  y_ma_edu = d9reduc[which(d9reduc$Machi==1 & d9reduc$EdMes==1),"Pheno"],     #vector of machi phenotypes with edu experience
  y_me_emp = d9reduc[which(d9reduc$Machi==0 & d9reduc$EmpMat==1),"Pheno"],    #vector of mestizo phenotypes with emp experience

  t_in = 25,                                                                  #number of model function time steps to recurse
  init_freqs = c(0.9,0,0,0.1,                                                 #initial phenotype frequencies for theoretical model (A11,A1X,A2X,A22,B11,B1X,B2X,B22)
                 0.1,0,0,0.9)
)


######  m3

set.seed(1)
m3 <- stan( file=model_file[1],
              data=data_list25,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.999, max_treedepth=15) #default treedepth is 10
            )

saveRDS(m3, "m3_fit.rds") #save stan output so you don't have re-run model
post3 <- extract.samples( m3 ) 

#pairs(m3, pars=c("aB","bB","m","i","c","mu")) #to see divergent transitions (in red)

print(m3, pars=c(
         "a_constr",
         "b_constr",
         "m",
         "i",
         "c",
         "mu",
         "bA_bonus",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

fitmod <- m3

#look at all traces
pdf(file="./Plots/traces_m3.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(traceplot(fitmod, pars="aB", inc_warmup=T))
    print(traceplot(fitmod, pars="bB", inc_warmup=T))
    print(traceplot(fitmod, pars="bA_bonus", inc_warmup=T))
    print(traceplot(fitmod, pars="m", inc_warmup=T))
    print(traceplot(fitmod, pars="i", inc_warmup=T))
    print(traceplot(fitmod, pars="c", inc_warmup=T))
    print(traceplot(fitmod, pars="mu", inc_warmup=T))
    print(traceplot(fitmod, pars="lp__", inc_warmup=T))

graphics.off()



####### m4

set.seed(1)
m4 <- stan( file=model_file[2],
              data=data_list25,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.999, max_treedepth=15) #default treedepth is 10
            )

saveRDS(m4, "m4_fit.rds")
post4 <- extract.samples( m4 ) 

#pairs(m4, pars=c("aB","bB","m","i","c","mu"))

print(m4, pars=c(
         "a_constr",
         "b_constr",
         "m",
         "i",
         "c",
         "mu",
         "bA_bonus",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

fitmod <- m4

#look at all traces
pdf(file="./Plots/traces_m4.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(traceplot(fitmod, pars="aB", inc_warmup=T))
    print(traceplot(fitmod, pars="bB", inc_warmup=T))
    print(traceplot(fitmod, pars="bA_bonus", inc_warmup=T))
    print(traceplot(fitmod, pars="m", inc_warmup=T))
    print(traceplot(fitmod, pars="i", inc_warmup=T))
    print(traceplot(fitmod, pars="c", inc_warmup=T))
    print(traceplot(fitmod, pars="mu", inc_warmup=T))
    print(traceplot(fitmod, pars="lp__", inc_warmup=T))

graphics.off()


