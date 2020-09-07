############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#define global variables
J <- length(unique(d$newID))		  #number people
K <- length(unique(d$question))		#number questions
T <- length(unique(d$target.num))	#number targets (ego, in-group, out-group)
Mach <- 2							            #number ethnicities
jj <- d$newID                     #vector of person IDs
kk <- d$question                  #vector of question IDs
y <- d$response                   #vector of responses



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


data_list <- list(
  J = length(unique(d$newID)),    			#number of people
  K = length(unique(d$question)),   		#number of questions
  T = length(unique(d$target.num)), 		#number of targets
  N = nrow(d),                          #total number of responses
  jj = d$newID,                         #N vector of person IDs
  kk = d$question,                      #N vector of question IDs
  tt = d$target.num,                    #N vector of target numbers
  y = d$response,                       #N vector of responses

  Machi = 2 - d[,"Machi"],              #ethnicity converted to index for each response

  MachiIndiv = 2 - unique(d[,c("newID","Machi")])[,2], #ethnicity for each individual

  EdMes = d$EdMes,                      #vector of education experience with mestizos for each guess
  EmpMat = d$EmpMat                    #vector of employment experience with machis for each guess
)


start_list <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), 

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aEdMes = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T))
)



############################### Run Stan models: m4, m5, and m19




####### m4

set.seed(1)
m4 <- stan(	file=model_file[1],
            	data=data_list,
            	iter=samps,
            	chains=num_chains, 
            	init=rep(list(start_list), num_chains),
            	control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post4 <- extract.samples( m4 ) 

print(m4, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",

				 "rBeta",
				 "muBeta",
				 "sigmaBeta",

				 "rGamma",
         "muGamma",
         "sigmaGamma",
         "L_R_q",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)


#look at all traces
pdf(file="./Plots/traces_m4.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m4)

graphics.off()



############################################### m5

set.seed(1)
m5 <- stan( file=model_file[2],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post5 <- extract.samples( m5 )


print(m5, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aEdMes",       

         "rBeta",
         "muBeta",
         "sigmaBeta",

         "rGamma",
         "muGamma",
         "sigmaGamma",
         "L_R_q",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m5.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m5)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m5, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()



############################################### m14

set.seed(1)
m14 <- stan( file=model_file[3],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post14 <- extract.samples( m14 )


print(m14, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aEmpMat",

         "rBeta",
         "muBeta",
         "sigmaBeta",

         "rGamma",
         "muGamma",
         "sigmaGamma",
         "L_R_q",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m14.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m14)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m14, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()




#### save stanfit output so you don't have to re-run models again
saveRDS(m4, "m4_fit.rds")
saveRDS(m5, "m5_fit.rds")
saveRDS(m14, "m14_fit.rds")


