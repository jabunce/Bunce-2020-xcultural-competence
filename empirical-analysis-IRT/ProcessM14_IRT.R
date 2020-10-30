#### m14 results

#m14 <- readRDS("m14_fit.rds") #to read in stanfit output
post14 <- extract.samples( m14 )
#str(post14)
#quest_names.e


################### Contrasts of probabilities of answer=positive

#str(post14)
post0 <- post14
num_samp <- length(post0$lp__)	

#initialize matrices
probs.e.me.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.me.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.me.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.me.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.ma.typ <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) ) #typical machi
probs.e.me.typ <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) ) #typical mestizo


#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

	probs.e.me.no[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] - post0$rBeta[,k,1] ) ) #muInt[sample, Mach, T]
	probs.i.me.no[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] - post0$rBeta[,k,2] ) )
  probs.o.me.no[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] - post0$rBeta[,k,3] ) )  

	probs.e.me.emp[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] + post0$aEmpMat[,2,1] - post0$rBeta[,k,1] ) )
	probs.i.me.emp[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] + post0$aEmpMat[,2,2] - post0$rBeta[,k,2] ) )
  probs.o.me.emp[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] + post0$aEmpMat[,2,3] - post0$rBeta[,k,3] ) )

  #typical average machi, no experience hiring other machis
	probs.e.ma.typ[,k] <- inv.logit( post0$rGamma[,k,1]*
							( post0$muInt[,1,1] - 
								post0$rBeta[,k,1] ) )

  #typical average mestizo
  probs.e.me.typ[,k] <- probs.e.me.emp[,k]*prop.ego.9.me.emp + probs.e.me.emp[,k]*(1-prop.ego.9.me.emp) #weight by sample proportion (from FormatData)



  if (k %in% c(12) ) { #differences in coding between ego and ingroup axes
    	probs.i.me.no[,k] <- 1 - probs.i.me.no[,k]
    	probs.i.me.emp[,k] <- 1 - probs.i.me.emp[,k]
  	} #if


	if (k %in% c(8,12,13) ) { #differences in coding between ego and outgroup axes
		probs.o.me.no[,k] <- 1 - probs.o.me.no[,k]
		probs.o.me.emp[,k] <- 1 - probs.o.me.emp[,k]
	} #if

} # for k




#only mestizos with employer experience

pheno_names_me <- c("pB1X","pB2X",
                    "pB11","pB1i","pB1o","pB1b","pB1notX",
                    "pB22","pB2i","pB2o","pB2b","pB2notX")

pheno_q9_me_emp <- matrix( data=-100, ncol=length(pheno_names_me), nrow=num_samp, dimnames=list(1:num_samp,pheno_names_me) )
colnames(pheno_q9_me_emp) <- pheno_names_me

k=9 #question 9

for (z in 1:num_samp) {

  pA1X <- 0 #re-initialize for each posterior sample set
  pA2X <- 0
  pA11 <- 0
  pA1i <- 0
  pA1o <- 0
  pA1b <- 0
  pA1notX <- 0
  pA22 <- 0
  pA2i <- 0
  pA2o <- 0
  pA2b <- 0
  pA2notX <- 0
  pB1X <- 0
  pB2X <- 0
  pB11 <- 0
  pB1i <- 0
  pB1o <- 0
  pB1b <- 0
  pB1notX <- 0
  pB22 <- 0
  pB2i <- 0
  pB2o <- 0
  pB2b <- 0
  pB2notX <- 0


  #if machi mean prob(yes)>0.5 and mest mean prob(yes)<0.5
  if ( probs.e.ma.typ[z,k] > 0.5 && probs.e.me.typ[z,k] < 0.5 ) {

    pB1X <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB2X <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB11 <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    pB1b <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    #pB1i <- pB11
    pB1o <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])    
    pB2i <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    #pB2o <- pB22
    pB2b <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    pB22 <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])

  } #if

  #if machi mean prob(yes)<0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z,k] < 0.5 && probs.e.me.typ[z,k] > 0.5 ) {

    pB1X <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    pB2X <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    pB11 <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    pB1b <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB1i <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])
    #pB1o <- pB11
    #pB2i <- pB22      
    pB2o <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    pB2b <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB22 <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])

  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)<0.5
  if ( probs.e.ma.typ[z,k] < 0.5 && probs.e.me.typ[z,k] < 0.5 ) {

    pB1X <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])
    #pB2X <- pA22
    pB11 <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    #pB1b <- pB11
    pB1i <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    pB1o <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]     
    pB2i <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    pB2o <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB2b <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    pB22 <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])

  } #if


  #if machi mean prob(yes)>0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z,k] > 0.5 && probs.e.me.typ[z,k] > 0.5 ) {

    #pB1X <- pB11
    pB2X <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    pB11 <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * probs.o.me.emp[z,k]
    pB1i <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB1o <- probs.e.me.emp[z,k] * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    pB1b <- probs.e.me.emp[z,k] * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])     
    pB2i <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * probs.o.me.emp[z,k]
    pB2o <- (1-probs.e.me.emp[z,k]) * probs.i.me.emp[z,k] * (1-probs.o.me.emp[z,k])
    #pB2b <- pB22
    pB22 <- (1-probs.e.me.emp[z,k]) * (1-probs.i.me.emp[z,k]) * (1-probs.o.me.emp[z,k])

  } #if

  pB1notX <- pB11+pB1i+pB1o+pB1b
  pB2notX <- pB22+pB2i+pB2o+pB2b

  pheno_q9_me_emp[z,1:(0.5*length(pheno_names))] <- c(pB1X,pB2X,pB11,pB1i,pB1o,pB1b,pB1notX,pB22,pB2i,pB2o,pB2b,pB2notX)

} #for z



