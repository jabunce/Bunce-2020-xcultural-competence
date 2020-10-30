#### m2 results


post2 <- extract.samples( m2 )
#str(post2)



################### Phenotype frequency calculation


post0 <- post2
num_samp <- length(post0$lp__)	


#initialize matrices
probs.e.ma.no <- rep(0,num_samp) #machis no experience
probs.i.ma.no <- rep(0,num_samp)
probs.o.ma.no <- rep(0,num_samp)

probs.e.ma.ed <- rep(0,num_samp) #machis edu experience
probs.i.ma.ed <- rep(0,num_samp)
probs.o.ma.ed <- rep(0,num_samp)

probs.e.me.no <- rep(0,num_samp) #mestizos no experience
probs.i.me.no <- rep(0,num_samp)
probs.o.me.no <- rep(0,num_samp)

probs.e.me.emp <- rep(0,num_samp) #mestizos emp experience
probs.i.me.emp <- rep(0,num_samp)
probs.o.me.emp <- rep(0,num_samp)

probs.e.me.typ <- rep(0,num_samp) #typical mestizo
probs.e.ma.typ <- rep(0,num_samp) #typical machi


#mean posterior estimates of probability of each person answering the question as positive

	#machis no inter-ethnic education experience
	probs.e.ma.no <- inv.logit( post0$muInt[,1,1] ) #muInt[sample, Mach, T]
	probs.i.ma.no <- inv.logit( post0$muInt[,1,2] )
	probs.o.ma.no <- inv.logit( post0$muInt[,1,3] )

	#machis education
	probs.e.ma.ed <- inv.logit( post0$muInt[,1,1] + post0$aEdMes[,1,1] )
	probs.i.ma.ed <- inv.logit( post0$muInt[,1,2] + post0$aEdMes[,1,2] )	
	probs.o.ma.ed <- inv.logit( post0$muInt[,1,3] + post0$aEdMes[,1,3] )

  #mestizos no inter-ethnic employer experience, but all have education experience among mestizos
  probs.e.me.no <- inv.logit( post0$muInt[,2,1] + post0$aEdMes[,2,1]) #muInt[sample, Mach, T]
  probs.i.me.no <- inv.logit( post0$muInt[,2,2] + post0$aEdMes[,2,2])
  probs.o.me.no <- inv.logit( post0$muInt[,2,3] + post0$aEdMes[,2,3])  

  #mestizos employer
  probs.e.me.emp <- inv.logit( post0$muInt[,2,1] + post0$aEdMes[,2,1] + post0$aEmpMat[,2,1] )
  probs.i.me.emp <- inv.logit( post0$muInt[,2,2] + post0$aEdMes[,2,2] + post0$aEmpMat[,2,2] )
  probs.o.me.emp <- inv.logit( post0$muInt[,2,3] + post0$aEdMes[,2,3] + post0$aEmpMat[,2,3] )


	#typical average mestizo
	probs.e.me.typ <- probs.e.me.emp*prop.ego.9.me.emp + probs.e.me.no*(1-prop.ego.9.me.emp) #weight by sample proportion (from FormatData)

  #typical average machi
  probs.e.ma.typ <- probs.e.ma.ed*prop.ego.9.ma.ed + probs.e.ma.no*(1-prop.ego.9.ma.ed) 




#only machis with education experience

pheno_names_ma <- c("pA1X","pA2X",
                    "pA11","pA1i","pA1o","pA1b","pA1notX",
                    "pA22","pA2i","pA2o","pA2b","pA2notX")

pheno_q9_ma_edu <- matrix( data=-100, ncol=length(pheno_names_ma), nrow=num_samp, dimnames=list(1:num_samp,pheno_names_ma) )
colnames(pheno_q9_ma_edu) <- pheno_names_ma 


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
  if ( probs.e.ma.typ[z] > 0.5 && probs.e.me.typ[z] < 0.5 ) {

    pA1X <- probs.e.ma[z] * probs.i.ma[z] * (1-probs.o.ma[z])
    pA2X <- (1-probs.e.ma[z]) * probs.i.ma[z] * (1-probs.o.ma[z])
    pA11 <- probs.e.ma[z] * probs.i.ma[z] * probs.o.ma[z]
    pA1b <- probs.e.ma[z] * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA1i <- probs.e.ma[z] * (1-probs.i.ma[z]) * (1-probs.o.ma[z])
    #pA1o <- pA11
    #pA2i <- pA22
    pA2o <- (1-probs.e.ma[z]) * probs.i.ma[z] * probs.o.ma[z]
    pA2b <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA22 <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * (1-probs.o.ma[z])
      
           
  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z] < 0.5 && probs.e.me.typ[z] > 0.5 ) {

    pA1X <- probs.e.ma[z] * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA2X <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA11 <- probs.e.ma[z] * probs.i.ma[z] * probs.o.ma[z]
    pA1b <- probs.e.ma[z] * probs.i.ma[z] * (1-probs.o.ma[z])
    #pA1i <- pA11
    pA1o <- probs.e.ma[z] * (1-probs.i.ma[z]) * (1-probs.o.ma[z])
    pA2i <- (1-probs.e.ma[z]) * probs.i.ma[z] * probs.o.ma[z]
    #pA2o <- pA22
    pA2b <- (1-probs.e.ma[z]) * probs.i.ma[z] * (1-probs.o.ma[z])
    pA22 <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * (1-probs.o.ma[z])
      

  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)<0.5
  if ( probs.e.ma.typ[z] < 0.5 && probs.e.me.typ[z] < 0.5 ) {

    pA1X <- probs.e.ma[z] * (1-probs.i.ma[z]) * (1-probs.o.ma[z])
    #pA2X <- pA22
    pA11 <- probs.e.ma[z] * probs.i.ma[z] * probs.o.ma[z]
    #pA1b <- pA11
    pA1o <- probs.e.ma[z] * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA1i <- probs.e.ma[z] * probs.i.ma[z] * (1-probs.o.ma[z])    
    pA2b <- (1-probs.e.ma[z]) * probs.i.ma[z] * probs.o.ma[z]
    pA2o <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA2i <- (1-probs.e.ma[z]) * probs.i.ma[z] * (1-probs.o.ma[z])
    pA22 <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * (1-probs.o.ma[z])       


  } #if


  #if machi mean prob(yes)>0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z] > 0.5 && probs.e.me.typ[z] > 0.5 ) {

    #pA1X <- pA11
    pA2X <- (1-probs.e.ma[z]) * probs.i.ma[z] * probs.o.ma[z]
    pA11 <- probs.e.ma[z] * probs.i.ma[z] * probs.o.ma[z]
    pA1b <- probs.e.ma[z] * (1-probs.i.ma[z]) * (1-probs.o.ma[z]) 
    pA1i <- probs.e.ma[z] * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA1o <- probs.e.ma[z] * probs.i.ma[z] * (1-probs.o.ma[z])
    pA2i <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * probs.o.ma[z]
    pA2o <- (1-probs.e.ma[z]) * probs.i.ma[z] * (1-probs.o.ma[z])
    #pA2b <- pA22
    pA22 <- (1-probs.e.ma[z]) * (1-probs.i.ma[z]) * (1-probs.o.ma[z])
      

  } #if

  pA1notX <- pA11+pA1i+pA1o+pA1b
  pA2notX <- pA22+pA2i+pA2o+pA2b

  pheno_q9_ma_edu[z,1:(0.5*length(pheno_names))] <- c(pA1X,pA2X,pA11,pA1i,pA1o,pA1b,pA1notX,pA22,pA2i,pA2o,pA2b,pA2notX)

} #for z






#only mestizos with employer experience

pheno_names_me <- c("pB1X","pB2X",
                    "pB11","pB1i","pB1o","pB1b","pB1notX",
                    "pB22","pB2i","pB2o","pB2b","pB2notX")

pheno_q9_me_emp <- matrix( data=-100, ncol=length(pheno_names_me), nrow=num_samp, dimnames=list(1:num_samp,pheno_names_me) )
colnames(pheno_q9_me_emp) <- pheno_names_me


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
  if ( probs.e.ma.typ[z] > 0.5 && probs.e.me.typ[z] < 0.5 ) {

    pB1X <- probs.e.me[z] * (1-probs.i.me[z]) * probs.o.me[z]
    pB2X <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * probs.o.me[z]
    pB11 <- probs.e.me[z] * probs.i.me[z] * probs.o.me[z]
    pB1b <- probs.e.me[z] * probs.i.me[z] * (1-probs.o.me[z])
    #pB1i <- pB11
    pB1o <- probs.e.me[z] * (1-probs.i.me[z]) * (1-probs.o.me[z])    
    pB2i <- (1-probs.e.me[z]) * probs.i.me[z] * probs.o.me[z]
    #pB2o <- pB22
    pB2b <- (1-probs.e.me[z]) * probs.i.me[z] * (1-probs.o.me[z])
    pB22 <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * (1-probs.o.me[z])

  } #if

  #if machi mean prob(yes)<0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z] < 0.5 && probs.e.me.typ[z] > 0.5 ) {

    pB1X <- probs.e.me[z] * probs.i.me[z] * (1-probs.o.me[z])
    pB2X <- (1-probs.e.me[z]) * probs.i.me[z] * (1-probs.o.me[z])
    pB11 <- probs.e.me[z] * probs.i.me[z] * probs.o.me[z]
    pB1b <- probs.e.me[z] * (1-probs.i.me[z]) * probs.o.me[z]
    pB1i <- probs.e.me[z] * (1-probs.i.me[z]) * (1-probs.o.me[z])      
    #pB1o <- pB11
    #pB2i <- pB22  
    pB2o <- (1-probs.e.me[z]) * probs.i.me[z] * probs.o.me[z]
    pB2b <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * probs.o.me[z]
    pB22 <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * (1-probs.o.me[z])

  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)<0.5
  if ( probs.e.ma.typ[z] < 0.5 && probs.e.me.typ[z] < 0.5 ) {

    pB1X <- probs.e.me[z] * (1-probs.i.me[z]) * (1-probs.o.me[z])
    #pB2X <- pA22
    pB11 <- probs.e.me[z] * probs.i.me[z] * probs.o.me[z]
    #pB1b <- pB11
    pB1i <- probs.e.me[z] * probs.i.me[z] * (1-probs.o.me[z])
    pB1o <- probs.e.me[z] * (1-probs.i.me[z]) * probs.o.me[z]     
    pB2i <- (1-probs.e.me[z]) * probs.i.me[z] * (1-probs.o.me[z])
    pB2o <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * probs.o.me[z]
    pB2b <- (1-probs.e.me[z]) * probs.i.me[z] * probs.o.me[z]
    pB22 <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * (1-probs.o.me[z])

  } #if


  #if machi mean prob(yes)>0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma.typ[z] > 0.5 && probs.e.me.typ[z] > 0.5 ) {

    #pB1X <- pB11
    pB2X <- (1-probs.e.me[z]) * probs.i.me[z] * probs.o.me[z]
    pB11 <- probs.e.me[z] * probs.i.me[z] * probs.o.me[z]
    pB1i <- probs.e.me[z] * (1-probs.i.me[z]) * probs.o.me[z]
    pB1o <- probs.e.me[z] * probs.i.me[z] * (1-probs.o.me[z])
    pB1b <- probs.e.me[z] * (1-probs.i.me[z]) * (1-probs.o.me[z])     
    pB2i <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * probs.o.me[z]
    pB2o <- (1-probs.e.me[z]) * probs.i.me[z] * (1-probs.o.me[z])
    #pB2b <- pB22
    pB22 <- (1-probs.e.me[z]) * (1-probs.i.me[z]) * (1-probs.o.me[z])
  

  } #if

  pB1notX <- pB11+pB1i+pB1o+pB1b
  pB2notX <- pB22+pB2i+pB2o+pB2b

  pheno_q9_me_emp[z,1:(0.5*length(pheno_names))] <- c(pB1X,pB2X,pB11,pB1i,pB1o,pB1b,pB1notX,pB22,pB2i,pB2o,pB2b,pB2notX)

} #for z


