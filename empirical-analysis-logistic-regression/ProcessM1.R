####### m1 results

#m4 <- readRDS("m4_fit.rds") #to read in stanfit output
post1 <- extract.samples( m1 )
#str(post1)




################### Phenotype frequency calculation

post0 <- post1
num_samp <- length(post0$lp__)	

machiID <- 	unique(d[,c("newID","Machi")])
Machi <- machiID[,2]


#ego
probs.e.ma <- rep(0,num_samp)        #mean prob for machis
probs.e.me <- rep(0,num_samp)		    #mean prob for mestizos

#ingroup
probs.i.ma <- rep(0,num_samp)
probs.i.me <- rep(0,num_samp)

#outgroup
probs.o.ma <- rep(0,num_samp)
probs.o.me <- rep(0,num_samp)



#mean posterior estimates of probability of each person answering the question as positive

	#ego
	probs.e.ma <- inv.logit( post0$muInt[,1,1] ) #mean prob machi
	probs.e.me <- inv.logit( post0$muInt[,2,1] ) #mean prob mestizo

	#ingroup
	probs.i.ma <- inv.logit( post0$muInt[,1,2] )
	probs.i.me <- inv.logit( post0$muInt[,2,2] )

	#outgroup
	probs.o.ma <- inv.logit( post0$muInt[,1,3] )
	probs.o.me <- inv.logit( post0$muInt[,2,3] )




pheno_names <- c("pA1X","pA2X",
                 "pA11","pA1i","pA1o","pA1b","pA1notX",
                 "pA22","pA2i","pA2o","pA2b","pA2notX",
                 "pB1X","pB2X",
                 "pB11","pB1i","pB1o","pB1b","pB1notX",
                 "pB22","pB2i","pB2o","pB2b","pB2notX")

pheno_q9 <- matrix( data=-100, ncol=length(pheno_names), nrow=num_samp, dimnames=list(1:num_samp,pheno_names) )


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
  if ( probs.e.ma[z] > 0.5 && probs.e.me[z] < 0.5 ) {

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
  if ( probs.e.ma[z] < 0.5 && probs.e.me[z] > 0.5 ) {

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
  if ( probs.e.ma[z] < 0.5 && probs.e.me[z] < 0.5 ) {

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
  if ( probs.e.ma[z] > 0.5 && probs.e.me[z] > 0.5 ) {

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

  pA1notX <- pA11+pA1i+pA1o+pA1b
  pA2notX <- pA22+pA2i+pA2o+pA2b
  pB1notX <- pB11+pB1i+pB1o+pB1b
  pB2notX <- pB22+pB2i+pB2o+pB2b

  pheno_q9[z,1:24] <- c(pA1X,pA2X,pA11,pA1i,pA1o,pA1b,pA1notX,pA22,pA2i,pA2o,pA2b,pA2notX,
                        pB1X,pB2X,pB11,pB1i,pB1o,pB1b,pB1notX,pB22,pB2i,pB2o,pB2b,pB2notX)

} #for z


ego_q9 <- cbind(probs.e.ma,probs.e.me)
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
