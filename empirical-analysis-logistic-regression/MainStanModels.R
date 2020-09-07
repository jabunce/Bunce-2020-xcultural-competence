############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#define global variables
J <- length(unique(d$newID))		  #number people
T <- length(unique(d$target.num))	#number targets (ego, in-group, out-group)
Mach <- 2							            #number ethnicities
jj <- d$newID                     #vector of person IDs
y <- d$response                   #vector of responses



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


data_list <- list(
  J = length(unique(d$newID)),    			#number of people
  T = length(unique(d$target.num)), 		#number of targets
  N = nrow(d),                          #total number of responses
  jj = d$newID,                         #N vector of person IDs
  tt = d$target.num,                    #N vector of target numbers
  y = d$response,                       #N vector of responses

  Machi = 2 - d[,"Machi"],              #ethnicity converted to index for each response

  MachiIndiv = 2 - unique(d[,c("newID","Machi")])[,2], #ethnicity for each individual

  EdMes = d$EdMes,                      #vector of education experience with mestizos for each guess

  EmpMat = d$EmpMat                      #vector of employment experience with machis for each guess
)


start_list <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), 

  aEdMes = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T))
)




####### m1

set.seed(1)
m1 <- stan(	file=model_file[1],
            	data=data_list,
            	iter=samps,
            	chains=num_chains, 
            	init=rep(list(start_list), num_chains),
            	control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

saveRDS(m1, "m1_fit.rds") #save stanfit output

post1 <- extract.samples( m1 ) 

print(m1, pars=c(
				 "aInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",

         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)


#look at all traces
pdf(file="./Plots/traces_m1.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m1)

graphics.off()



############################################### m2

set.seed(1)
m2 <- stan( file=model_file[2],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

saveRDS(m2, "m2_fit.rds") #save stanfit output

post2 <- extract.samples( m2 )


print(m2, pars=c(
         "aInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aEdMes",       
         "aEmpMat",
         
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m2.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m2)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m2, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m2, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()


