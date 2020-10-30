#### m5 results


#m11 <- readRDS("m5_fit.rds") #to read in stanfit output
post5 <- extract.samples( m5 )
#str(post5)
#quest_names.e


################### Contrasts of probabilities of answer=positive

#str(post5)
post0 <- post5
num_samp <- length(post0$lp__)	


#initialize matrices
probs.e.ma.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.ma.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.ma.ed <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.ed <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.ma.ed <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.me.typ <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) ) #typical mestizo
probs.e.ma.typ <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) ) #typical machi


#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

	#no inter-ethnic education experience
	probs.e.ma.no[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] - post0$rBeta[,k,1] ) ) #muInt[sample, Mach, T]
	probs.i.ma.no[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] - post0$rBeta[,k,2] ) )
	probs.o.ma.no[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] - post0$rBeta[,k,3] ) )

	#education
	probs.e.ma.ed[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] + post0$aEdMes[,1,1] - post0$rBeta[,k,1] ) )
	probs.i.ma.ed[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] + post0$aEdMes[,1,2] - post0$rBeta[,k,2] ) )	
	probs.o.ma.ed[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] + post0$aEdMes[,1,3] - post0$rBeta[,k,3] ) )

	#typical average mestizo
	probs.e.me.typ[,k] <- inv.logit( post0$rGamma[,k,1]*
							( post0$muInt[,2,1] + post0$aEdMes[,2,1] - 
								post0$rBeta[,k,1] ) )

  #typical average machi
  probs.e.ma.typ[,k] <- probs.e.ma.ed[,k]*prop.ego.9.ma.ed + probs.e.ma.no[,k]*(1-prop.ego.9.ma.ed) #weight by sample proportion (from FormatData)



	if (k %in% c(12) ) { #differences in coding between ego and ingroup axes
		probs.i.ma.no[,k] <- 1 - probs.i.ma.no[,k]
		probs.i.ma.ed[,k] <- 1 - probs.i.ma.ed[,k]
	} #if

	if (k %in% c(8,12,13) ) { #differences in coding between ego and outgroup axes
		probs.o.ma.no[,k] <- 1 - probs.o.ma.no[,k]
		probs.o.ma.ed[,k] <- 1 - probs.o.ma.ed[,k]
	} #if

} # for k



#only machis with education experience

pheno_names_ma <- c("pA1X","pA2X",
                    "pA11","pA1i","pA1o","pA1b","pA1notX",
                    "pA22","pA2i","pA2o","pA2b","pA2notX")

pheno_q9_ma_edu <- matrix( data=-100, ncol=length(pheno_names_ma), nrow=num_samp, dimnames=list(1:num_samp,pheno_names_ma) )
colnames(pheno_q9_ma_edu) <- pheno_names_ma 

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

    pA1X <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    pA2X <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    pA11 <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    pA1b <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA1i <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])
    #pA1o <- pA11
    #pA2i <- pA22
    pA2o <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    pA2b <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA22 <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])
           
  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z,k] < 0.5 && probs.e.me.typ[z,k] > 0.5 ) {

    pA1X <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA2X <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA11 <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    pA1b <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    #pA1i <- pA11
    pA1o <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])
    pA2i <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    #pA2o <- pA22
    pA2b <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    pA22 <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])

  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)<0.5
  if ( probs.e.ma.typ[z,k] < 0.5 && probs.e.me.typ[z,k] < 0.5 ) {

    pA1X <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])
    #pA2X <- pA22
    pA11 <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    #pA1b <- pA11
    pA1i <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    pA1o <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA2i <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    pA2o <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA2b <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    pA22 <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])

  } #if


  #if machi mean prob(yes)>0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z,k] > 0.5 && probs.e.me.typ[z,k] > 0.5 ) {

    #pA1X <- pA11
    pA2X <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    pA11 <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * probs.o.ma.ed[z,k]
    pA1b <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k]) 
    pA1i <- probs.e.ma.ed[z,k] * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA1o <- probs.e.ma.ed[z,k] * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    pA2i <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * probs.o.ma.ed[z,k]
    pA2o <- (1-probs.e.ma.ed[z,k]) * probs.i.ma.ed[z,k] * (1-probs.o.ma.ed[z,k])
    #pA2b <- pA22
    pA22 <- (1-probs.e.ma.ed[z,k]) * (1-probs.i.ma.ed[z,k]) * (1-probs.o.ma.ed[z,k])

  } #if

  pA1notX <- pA11+pA1i+pA1o+pA1b
  pA2notX <- pA22+pA2i+pA2o+pA2b

  pheno_q9_ma_edu[z,1:(0.5*length(pheno_names))] <- c(pA1X,pA2X,pA11,pA1i,pA1o,pA1b,pA1notX,pA22,pA2i,pA2o,pA2b,pA2notX)

} #for z



