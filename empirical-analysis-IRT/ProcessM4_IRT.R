####### m4 results

#m4 <- readRDS("m4_fit.rds") #to read in stanfit output
post4 <- extract.samples( m4 )

# unique(d["question"])
# K
#quest_names.e
# k=9
# Ksub<-length(c(k)) #isolate question 9
# quest_names.e[k]



################### Contrasts of abilities

#str(post4)
post0 <- post4
num_samp <- length(post0$lp__)	

machiID <- 	unique(d[,c("newID","Machi")])
Machi <- machiID[,2]


#ego
probs.e.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )        #mean prob for machis
probs.e.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )  #prob with indiv-level variation
probs.e.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )		    #mean prob for mestizos
probs.e.me.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )

#ingroup
probs.i.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )

#outgroup
probs.o.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.o.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.o.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.o.me.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )



#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

	#ego
	probs.e.ma[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] - post0$rBeta[,k,1] ) )	#mean prob machi
	probs.e.me[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] - post0$rBeta[,k,1] ) ) #mean prob mestizo

	#ingroup
	probs.i.ma[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] - post0$rBeta[,k,2] ) )
	probs.i.me[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] - post0$rBeta[,k,2] ) )

	#outgroup
	probs.o.ma[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] - post0$rBeta[,k,3] ) )
	probs.o.me[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] - post0$rBeta[,k,3] ) )


	if (k %in% c(12) ) { #differences in coding between ego and ingroup axes
		probs.i.ma[,k] <- 1 - probs.i.ma[,k]
    	probs.i.ma.indiv[,k] <- 1 - probs.i.ma.indiv[,k]   
		probs.i.me[,k] <- 1 - probs.i.me[,k]
    	probs.i.me.indiv[,k] <- 1 - probs.i.me.indiv[,k]    
	} #if

	if (k %in% c(8,12,13) ) { #differences in coding between ego and outgroup axes
		probs.o.ma[,k] <- 1 - probs.o.ma[,k]
    	probs.o.ma.indiv[,k] <- 1 - probs.o.ma.indiv[,k]    
		probs.o.me[,k] <- 1 - probs.o.me[,k]
    	probs.o.me.indiv[,k] <- 1 - probs.o.me.indiv[,k]
	} #if

} # for k

#probs.e.ma[1:10,8:9]



pheno_names <- c("pA1X","pA2X",
                 "pA11","pA1i","pA1o","pA1b","pA1notX",
                 "pA22","pA2i","pA2o","pA2b","pA2notX",
                 "pB1X","pB2X",
                 "pB11","pB1i","pB1o","pB1b","pB1notX",
                 "pB22","pB2i","pB2o","pB2b","pB2notX")

pheno_q9 <- matrix( data=-100, ncol=length(pheno_names), nrow=num_samp, dimnames=list(1:num_samp,pheno_names) )

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
  if ( probs.e.ma[z,k] > 0.5 && probs.e.me[z,k] < 0.5 ) {

    pA1X <- probs.e.ma[z,k] * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    pA2X <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    pA11 <- probs.e.ma[z,k] * probs.i.ma[z,k] * probs.o.ma[z,k]
    pA1b <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA1i <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])
    #pA1o <- pA11
    #pA2i <- pA22
    pA2o <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * probs.o.ma[z,k]
    pA2b <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA22 <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])
           
    pB1X <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB2X <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB11 <- probs.e.me[z,k] * probs.i.me[z,k] * probs.o.me[z,k]
    pB1b <- probs.e.me[z,k] * probs.i.me[z,k] * (1-probs.o.me[z,k])
    #pB1i <- pB11
    pB1o <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])    
    pB2i <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * probs.o.me[z,k]
    #pB2o <- pB22
    pB2b <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * (1-probs.o.me[z,k])
    pB22 <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])
        
  } #if

  #if machi mean prob(yes)<0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma[z,k] < 0.5 && probs.e.me[z,k] > 0.5 ) {

    pA1X <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA2X <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA11 <- probs.e.ma[z,k] * probs.i.ma[z,k] * probs.o.ma[z,k]
    pA1b <- probs.e.ma[z,k] * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    #pA1i <- pA11
    pA1o <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])
    pA2i <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * probs.o.ma[z,k]
    #pA2o <- pA22
    pA2b <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    pA22 <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])
            
    pB1X <- probs.e.me[z,k] * probs.i.me[z,k] * (1-probs.o.me[z,k])
    pB2X <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * (1-probs.o.me[z,k])
    pB11 <- probs.e.me[z,k] * probs.i.me[z,k] * probs.o.me[z,k]
    pB1b <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB1i <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])      
    #pB1o <- pB11
    #pB2i <- pB22  
    pB2o <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * probs.o.me[z,k]
    pB2b <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB22 <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])
  
  } #if


  #if machi mean prob(yes)<0.5 and mest mean prob(yes)<0.5
  if ( probs.e.ma[z,k] < 0.5 && probs.e.me[z,k] < 0.5 ) {

    pA1X <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])
    #pA2X <- pA22
    pA11 <- probs.e.ma[z,k] * probs.i.ma[z,k] * probs.o.ma[z,k]
    #pA1b <- pA11
    pA1o <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA1i <- probs.e.ma[z,k] * probs.i.ma[z,k] * (1-probs.o.ma[z,k])    
    pA2b <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * probs.o.ma[z,k]
    pA2o <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA2i <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    pA22 <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])       

    pB1X <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])
    #pB2X <- pA22
    pB11 <- probs.e.me[z,k] * probs.i.me[z,k] * probs.o.me[z,k]
    #pB1b <- pB11
    pB1i <- probs.e.me[z,k] * probs.i.me[z,k] * (1-probs.o.me[z,k])
    pB1o <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * probs.o.me[z,k]     
    pB2i <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * (1-probs.o.me[z,k])
    pB2o <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB2b <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * probs.o.me[z,k]
    pB22 <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])
           
  } #if


  #if machi mean prob(yes)>0.5 and mest mean prob(yes)>0.5
  if ( probs.e.ma[z,k] > 0.5 && probs.e.me[z,k] > 0.5 ) {

    #pA1X <- pA11
    pA2X <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * probs.o.ma[z,k]
    pA11 <- probs.e.ma[z,k] * probs.i.ma[z,k] * probs.o.ma[z,k]
    pA1b <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k]) 
    pA1i <- probs.e.ma[z,k] * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA1o <- probs.e.ma[z,k] * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    pA2i <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * probs.o.ma[z,k]
    pA2o <- (1-probs.e.ma[z,k]) * probs.i.ma[z,k] * (1-probs.o.ma[z,k])
    #pA2b <- pA22
    pA22 <- (1-probs.e.ma[z,k]) * (1-probs.i.ma[z,k]) * (1-probs.o.ma[z,k])
            
    #pB1X <- pB11
    pB2X <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * probs.o.me[z,k]
    pB11 <- probs.e.me[z,k] * probs.i.me[z,k] * probs.o.me[z,k]
    pB1i <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB1o <- probs.e.me[z,k] * probs.i.me[z,k] * (1-probs.o.me[z,k])
    pB1b <- probs.e.me[z,k] * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])     
    pB2i <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * probs.o.me[z,k]
    pB2o <- (1-probs.e.me[z,k]) * probs.i.me[z,k] * (1-probs.o.me[z,k])
    #pB2b <- pB22
    pB22 <- (1-probs.e.me[z,k]) * (1-probs.i.me[z,k]) * (1-probs.o.me[z,k])
  
  } #if

  pA1notX <- pA11+pA1i+pA1o+pA1b
  pA2notX <- pA22+pA2i+pA2o+pA2b
  pB1notX <- pB11+pB1i+pB1o+pB1b
  pB2notX <- pB22+pB2i+pB2o+pB2b

  pheno_q9[z,1:24] <- c(pA1X,pA2X,pA11,pA1i,pA1o,pA1b,pA1notX,pA22,pA2i,pA2o,pA2b,pA2notX,
                        pB1X,pB2X,pB11,pB1i,pB1o,pB1b,pB1notX,pB22,pB2i,pB2o,pB2b,pB2notX)

} #for z


ego_q9 <- cbind(probs.e.ma[,9],probs.e.me[,9])
colnames(ego_q9) <- c("matsi q9 ego", "mest q9 ego")

pheno_q9_ma <- pheno_q9[,c(1:12)]
pheno_names_ma <- c("pA1X","pA2X",
                    "pA11","pA1i","pA1o","pA1b","pA1notX",
                    "pA22","pA2i","pA2o","pA2b","pA2notX")
colnames(pheno_q9_ma) <- pheno_names_ma


pheno_q9_me <- pheno_q9[,c(13:24)]
pheno_names_me <- c("pB1X","pB2X",
                    "pB11","pB1i","pB1o","pB1b","pB1notX",
                    "pB22","pB2i","pB2o","pB2b","pB2notX")
colnames(pheno_q9_me) <- pheno_names_me
