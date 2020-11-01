#Read the data from the csv data file into R:
Interviews.raw <- read.csv(file="./Data/Manu_perceptions_11sep18.csv", header=TRUE)


#Check the variable names and dimensions in the data frame Interviews.raw
#names(Interviews.raw)
#dim(Interviews.raw)


#question names

quest_names <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.post.chest",
						"9.pot.each",
						"10.good.nonbaptized.heaven",
						"11.postpone.work.visit",
						"12.expensive.store",
						"13.daughter.not.marry",
						"14.laborer.not.drunk"
				)


d.wide <- Interviews.raw
names(d.wide)

#sort by Target (1,2,3), then by Machi (0,1), then by Community (A,B,T)
d.wide <- d.wide[order(d.wide$Target, d.wide$Machi, d.wide$Community), ]

#d.wide[,c(1,2,4,14,15,16,17)]

#add consecutive newID
num_indivs <- length( unique(d.wide$ID) )
ID_key <- cbind( unique(d.wide$ID), 1:num_indivs)

d.wide$newID <- as.numeric( factor(d.wide$ID, levels=unique(d.wide$ID)) ) #trick to assign consecutive numbers to IDs 
#d.wide[,c("ID","newID")]


###########################Data from question 9 to fit theoretical model parameters

#flip coding of question 9: now 0=pot each, and 1=both to same daughter
d.wide$q.9flip <- ifelse( d.wide$q9==1, 0, 1)

#reduce dataset to just question 9 and experience predictors, with ego, in, and out responses in separate columns
d9raw <- d.wide[which(d.wide$Target==1), c("newID","Machi","EdMes","EmpMat","q.9flip")]
names(d9raw)[5] <- "q.9"
d9raw$In <- rep(NA, times=dim(d9raw)[1])
d9raw$Out <- d9raw$In

for (n in 1:dim(d.wide)[1]) {
    if ( d.wide[n,"Target"]==2 ) d9raw[which(d9raw$newID==d.wide[n,"newID"]), "In"] <- d.wide[n,"q.9flip"]
    if ( d.wide[n,"Target"]==3 ) d9raw[which(d9raw$newID==d.wide[n,"newID"]), "Out"] <- d.wide[n,"q.9flip"] 
}

names(d9raw)[5] <- "Ego"

#remove NA responses
d9 <- d9raw[which( is.na(d9raw$Ego)==FALSE & is.na(d9raw$In)==FALSE & is.na(d9raw$Out)==FALSE ),]

#make new consecutive ID
d9$newID9 <- as.numeric( factor(d9$newID, levels=unique(d9$newID)) )

#compute phenotypes assuming > 50% mestizos ego answer q9=0 and > 50% machis ego answer q9=1

d9$Pheno <- rep(NA, times=dim(d9)[1])

for (n in 1:dim(d9)[1]) {
    if ( d9[n,"Ego"]==1 & d9[n,"In"]==1 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 1 #11
    if ( d9[n,"Ego"]==0 & d9[n,"In"]==0 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 4 #22

    if ( d9[n,"Machi"]==1 & d9[n,"Ego"]==1 & d9[n,"In"]==1 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 2 #1X
    if ( d9[n,"Machi"]==1 & d9[n,"Ego"]==0 & d9[n,"In"]==1 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 3 #2X
    if ( d9[n,"Machi"]==1 & d9[n,"Ego"]==1 & d9[n,"In"]==0 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 5 #1b
    if ( d9[n,"Machi"]==1 & d9[n,"Ego"]==1 & d9[n,"In"]==0 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 6 #1i
    if ( d9[n,"Machi"]==1 & d9[n,"Ego"]==0 & d9[n,"In"]==1 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 7 #2o
    if ( d9[n,"Machi"]==1 & d9[n,"Ego"]==0 & d9[n,"In"]==0 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 8 #2b

    if ( d9[n,"Machi"]==0 & d9[n,"Ego"]==1 & d9[n,"In"]==0 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 2 #1X
    if ( d9[n,"Machi"]==0 & d9[n,"Ego"]==0 & d9[n,"In"]==0 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 3 #2X
    if ( d9[n,"Machi"]==0 & d9[n,"Ego"]==1 & d9[n,"In"]==1 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 5 #1b
    if ( d9[n,"Machi"]==0 & d9[n,"Ego"]==1 & d9[n,"In"]==0 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 6 #1o
    if ( d9[n,"Machi"]==0 & d9[n,"Ego"]==0 & d9[n,"In"]==1 & d9[n,"Out"]==1 ) d9[n,"Pheno"] <- 7 #2i
    if ( d9[n,"Machi"]==0 & d9[n,"Ego"]==0 & d9[n,"In"]==1 & d9[n,"Out"]==0 ) d9[n,"Pheno"] <- 8 #2b
} 

#phenotype raw proportions among all machis and mestizos
#group S = machis
p9S11 <- length(d9[which(d9$Machi==1 & d9$Pheno==1),"newID9"])/length(d9[which(d9$Machi==1),"newID9"]) 
p9S1X <- length(d9[which(d9$Machi==1 & d9$Pheno==2),"newID9"])/length(d9[which(d9$Machi==1),"newID9"]) 
p9S2X <- length(d9[which(d9$Machi==1 & d9$Pheno==3),"newID9"])/length(d9[which(d9$Machi==1),"newID9"]) 
p9S22 <- length(d9[which(d9$Machi==1 & d9$Pheno==4),"newID9"])/length(d9[which(d9$Machi==1),"newID9"])

p9S1b <- length(d9[which(d9$Machi==1 & d9$Pheno==5),"newID9"])/length(d9[which(d9$Machi==1),"newID9"]) 
p9S1i <- length(d9[which(d9$Machi==1 & d9$Pheno==6),"newID9"])/length(d9[which(d9$Machi==1),"newID9"]) 
p9S2o <- length(d9[which(d9$Machi==1 & d9$Pheno==7),"newID9"])/length(d9[which(d9$Machi==1),"newID9"]) 
p9S2b <- length(d9[which(d9$Machi==1 & d9$Pheno==8),"newID9"])/length(d9[which(d9$Machi==1),"newID9"])

#group L = mestizos
p9L11 <- length(d9[which(d9$Machi==0 & d9$Pheno==1),"newID9"])/length(d9[which(d9$Machi==0),"newID9"]) 
p9L1X <- length(d9[which(d9$Machi==0 & d9$Pheno==2),"newID9"])/length(d9[which(d9$Machi==0),"newID9"]) 
p9L2X <- length(d9[which(d9$Machi==0 & d9$Pheno==3),"newID9"])/length(d9[which(d9$Machi==0),"newID9"]) 
p9L22 <- length(d9[which(d9$Machi==0 & d9$Pheno==4),"newID9"])/length(d9[which(d9$Machi==0),"newID9"])

p9L1b <- length(d9[which(d9$Machi==0 & d9$Pheno==5),"newID9"])/length(d9[which(d9$Machi==0),"newID9"]) 
p9L1o <- length(d9[which(d9$Machi==0 & d9$Pheno==6),"newID9"])/length(d9[which(d9$Machi==0),"newID9"]) 
p9L2i <- length(d9[which(d9$Machi==0 & d9$Pheno==7),"newID9"])/length(d9[which(d9$Machi==0),"newID9"]) 
p9L2b <- length(d9[which(d9$Machi==0 & d9$Pheno==8),"newID9"])/length(d9[which(d9$Machi==0),"newID9"])

pheno9list <- c(p9S11,p9S1X,p9S2X,p9S22,p9S1b,p9S1i,p9S2o,p9S2b,
                p9L11,p9L1X,p9L2X,p9L22,p9L1b,p9L1o,p9L2i,p9L2b)
names(pheno9list) <- c("p9S11","p9S1X","p9S2X","p9S22","p9S1b","p9S1i","p9S2o","p9S2b",
                        "p9L11","p9L1X","p9L2X","p9L22","p9L1b","p9L1o","p9L2i","p9L2b")

#phenotype raw proportions among machis and mestizos with inter-ethnic experience
#group S = machis
p9S11_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==1),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"]) 
p9S1X_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==2),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"]) 
p9S2X_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==3),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"]) 
p9S22_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==4),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"])

p9S1b_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==5),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"]) 
p9S1i_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==6),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"]) 
p9S2o_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==7),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"]) 
p9S2b_edu <- length(d9[which(d9$Machi==1 & d9$EdMes==1 & d9$Pheno==8),"newID9"])/length(d9[which(d9$Machi==1 & d9$EdMes==1),"newID9"])

#group L = mestizos
p9L11_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==1),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"]) 
p9L1X_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==2),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"]) 
p9L2X_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==3),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"]) 
p9L22_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==4),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"])

p9L1b_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==5),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"]) 
p9L1o_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==6),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"]) 
p9L2i_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==7),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"]) 
p9L2b_emp <- length(d9[which(d9$Machi==0 & d9$EmpMat==1 & d9$Pheno==8),"newID9"])/length(d9[which(d9$Machi==0 & d9$EmpMat==1),"newID9"])

pheno9list_exp <- c(p9S11_edu,p9S1X_edu,p9S2X_edu,p9S22_edu,p9S1b_edu,p9S1i_edu,p9S2o_edu,p9S2b_edu,
                    p9L11_emp,p9L1X_emp,p9L2X_emp,p9L22_emp,p9L1b_emp,p9L1o_emp,p9L2i_emp,p9L2b_emp)
names(pheno9list_exp) <- c("p9S11_edu","p9S1X_edu","p9S2X_edu","p9S22_edu","p9S1b_edu","p9S1i_edu","p9S2o_edu","p9S2b_edu",
                           "p9L11_emp","p9L1X_emp","p9L2X_emp","p9L22_emp","p9L1b_emp","p9L1o_emp","p9L2i_emp","p9L2b_emp")

d9reduc <- d9[which(d9$Pheno %in% 1:4),] #take only phenotypes 11, 1X, 2X, and 22

#make new consecutive ID
d9reduc$newID9reduc <- as.numeric( factor(d9reduc$newID9, levels=unique(d9reduc$newID9)) )

d.wide <- d.wide[,-30] #remove added column from d.wide


