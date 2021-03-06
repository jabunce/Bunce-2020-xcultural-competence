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


############################### convert dataframe from wide to long format

#names(d.wide)

question_cols <- grep("^q\\d+", colnames(d.wide)) 			#regular expression to get indices for all columns beginning with "q" followed by a number

names(d.wide)[question_cols] <- sprintf( "q.%d", 1:14 ) 	##rename question columns with "." separator before number
d.wide$respID <- 1:nrow(d.wide)								##add column with unique ID for each person-target combination
names(d.wide)							
#d.wide[,c("ID","newID","respID")]

d.long <- reshape(d.wide, varying=names(d.wide)[question_cols], sep=".",	### column indices of the questions
					 idvar="respID",
					 direction="long")

#dim(d.long)
#names(d.long)
d.long <- d.long[order(d.long$newID),]					#order questions by person ID
#d.long[1:100, c("ID","Machi","time","q")]


#re-name columns
names(d.long)[c(length(names(d.long)) - 4,
				length(names(d.long)) - 1,
				length(names(d.long)))] <- c("target.num", "question", "response") #rename


#dim(d.long)
#names(d.long)
#d.long[1:100, c("newID","Machi","target.num","question","response" )]


d.na.resp <- d.long[which(is.na(d.long$response)==TRUE),] 		#NA responses
#unique(d.na.resp$ID)

num.na.resp.ego <- length(which(d.na.resp$target.num==1)) 		#number of NA responses by target, conditional on people being asked the questions
num.na.resp.in <- length(which(d.na.resp$target.num==2))
num.na.resp.out <- length(which(d.na.resp$target.num==3))

num.all.resp.ego <- length(which(d.long$target.num==1))			#number of non-NA ego responses
#num.na.resp.ego/num.all.resp.ego 								#proportion of NA ego responses
 
d <- d.long[which(is.na(d.long$response)==FALSE),] 				#NA responses removed



############### dataset characteristics

K <- length(unique(d$question)) 						#number of questions
J <- length(unique(d$newID)) 							#number of people
N <- nrow(d) 											#number of responses across all targets
nummach <- length(unique(d[which(d$Machi==1),"ID"]))	#number machis
nummest <- length(unique(d[which(d$Machi==0),"ID"]))	#number mestizos






#######################flip coding of some questions to polarize latent axes

#index of questions to flip
Flip.e <- ifelse( 
				#ego
				(d$target.num==1 & d$question == 3) | 	#wear dead hat ok, changed=0
               	(d$target.num==1 & d$question == 8) | 	#chest pain go to post, changed=0
               	(d$target.num==1 & d$question == 9) |	#each gets a pot, changed=0
               	(d$target.num==1 & d$question == 10) |	#good nonbaptized heaven, changed=0
               	(d$target.num==1 & d$question == 12) |	#expensive store, changed=0
               	(d$target.num==1 & d$question == 13) |	#daughter not marry disobeys parents, changed=0
               	(d$target.num==1 & d$question == 14) |	#laborer not drunk, changed=0

               	#ingroup
                (d$target.num==2 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==2 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==2 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==2 & d$question == 10) |  #good nonbaptized heaven, changed=0
                (d$target.num==2 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==2 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==2 & d$question == 14) |  #laborer not drunk, changed=0

               	#outgroup
                (d$target.num==3 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==3 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==3 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==3 & d$question == 10) |  #good nonbaptized heaven, changed=0
                (d$target.num==3 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==3 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==3 & d$question == 14) ,  #laborer not drunk, changed=0, 	#laborer not drunk, changed=0
               	1 , 0 )



#allow coding of questions to differ by axis
Flip.io <- ifelse( 
				        #ego
                (d$target.num==1 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==1 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==1 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==1 & d$question == 10) |  #good nonbaptized heaven, changed=0
                (d$target.num==1 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==1 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==1 & d$question == 14) |  #laborer not drunk, changed=0

                #ingroup
                (d$target.num==2 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==2 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==2 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==2 & d$question == 10) |  #good nonbaptized heaven, changed=0
                #(d$target.num==2 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==2 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==2 & d$question == 14) |  #laborer not drunk, changed=0

                #outgroup
                (d$target.num==3 & d$question == 3) |   #wear dead hat ok, changed=0
                #(d$target.num==3 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==3 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==3 & d$question == 10) |  #good nonbaptized heaven, changed=0
                #(d$target.num==3 & d$question == 12) |  #expensive store, changed=0
                #(d$target.num==3 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==3 & d$question == 14) , 	#laborer not drunk, changed=0
               	1 , 0 )


#modify question names for flipped coding
quest_names.e <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.not.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.not.post.chest",
						"9.not.pot.each",
						"10.good.nonbaptized.not.heaven",
						"11.postpone.work.visit",
						"12.cheap.store",
						"13.daughter.must.marry",
						"14.laborer.is.drunk"
				)

quest_names.i <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.not.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.not.post.chest",
						"9.not.pot.each",
						"10.good.nonbaptized.not.heaven",
						"11.postpone.work.visit",
						"12.expensive.store",
						"13.daughter.must.marry",
						"14.laborer.is.drunk"
				)

quest_names.o <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.not.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.post.chest",
						"9.not.pot.each",
						"10.good.nonbaptized.not.heaven",
						"11.postpone.work.visit",
						"12.expensive.store",
						"13.daughter.not.marry",
						"14.laborer.is.drunk"
				)

d$ResponseFlipped.e <- ifelse( Flip.e == 1, ifelse( d$response==1, 0, 1), d$response )
d$ResponseFlipped.io <- ifelse( Flip.io == 1, ifelse( d$response==1, 0, 1), d$response )

#check flipping
#d[173:220, c("target.num","question", "response", "ResponseFlipped.e", "ResponseFlipped.io")]


pdf(file="./Plots/Flip_check.e.pdf", 
height=4, width=4)
par(mfrow=c(1,1))

plot( jitter(d$ResponseFlipped.e) ~ jitter(d$response), 
  col=ifelse(Flip.e==1,"red","black") )

graphics.off()

pdf(file="./Plots/Flip_check.io.pdf", 
height=4, width=4)
par(mfrow=c(1,1))

plot( jitter(d$ResponseFlipped.io) ~ jitter(d$response), 
  col=ifelse(Flip.io==1,"red","black") )

graphics.off()


#rename response columns
#names(d)
names(d)[c(length(names(d))-2, length(names(d))-1, length(names(d)))] <- c("resp.original", "response.e", "response")
#names(d)






#######################################remove people with NA predictors for the predictors in this analysis: EdMes and EmpMat

#get row indices with NAs
na.rows <- 0
for (n in 1:N) {
	if ( is.na(d[n,"EdMes"]) || is.na(d[n,"EmpMat"])) na.rows <- c(na.rows, n)
}


na.rows <- unique(na.rows[-1]) #get rid of initial 0
# d[c(112:152, 1265:1280, 1538:1553, 4748:4763), #look at NA rows
# 	c("ID", "Machi",
# 		"EdMes", "LabMes", "ComMes",
# 		"FamMat", "EmpMat", "CtyMat",
# 		"target.num", "question")]

#delete NA rows
d_all <- d
d <- d[-na.rows,]
#d[d$newID %in% c(1,2),]
#d[d$newID %in% c(5,44,54,157),]

#get new consecutive newIDs
d$newID <- as.numeric( factor(d$ID, levels=unique(d$ID)) )


#key to match ID and newID
ID_key <- cbind( unique(d$ID), unique(d$newID))
colnames(ID_key) <- c("ID", "newID")
ID_key2 <- ID_key[order(ID_key[,"ID"]),]

#d[1:20,c("ID","newID")]
#ID_key





##### samples sizes for different combinations of interviewees, regarding Question 9 for fair division of inheritance


num.indiv.ego.9 <- length(unique(d[which(d$target.num==1 & d$question==9),"ID"]))    #number indivs with ego responses
num.indiv.ego.9.ma <- length(unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1),"ID"]))    #number machis with ego responses
num.indiv.ego.9.me <- length(unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0),"ID"]))    #number mestizos with ego responses
num.indiv.ego.9.ma.ed <- length(unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1 & d$EdMes==1),"ID"]))    #number machis with ego responses and edu experience
num.indiv.ego.9.me.emp <- length(unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0 & d$EmpMat==1),"ID"]))    #number mestizos with ego responses and emp expeience

num.indiv.in.9 <- length(unique(d[which(d$target.num==2 & d$question==9),"ID"]))    #number indivs with in-group guesses
num.indiv.in.9.ma <- length(unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1),"ID"]))    #number machis with in-group guesses
num.indiv.in.9.me <- length(unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0),"ID"]))    #number mestizos with in-group guesses
num.indiv.in.9.ma.ed <- length(unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1 & d$EdMes==1),"ID"]))    #number machis with in-group guesses and edu experience
num.indiv.in.9.me.emp <- length(unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0 & d$EmpMat==1),"ID"]))    #number mestizos with in-group guesses and emp expeience 

num.indiv.out.9 <- length(unique(d[which(d$target.num==3 & d$question==9),"ID"]))    #number indivs with out-group guesses
num.indiv.out.9.ma <- length(unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1),"ID"]))    #number machis with out-group guesses
num.indiv.out.9.me <- length(unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0),"ID"]))    #number mestizos with out-group guesses
num.indiv.out.9.ma.ed <- length(unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1 & d$EdMes==1),"ID"]))    #number machis with out-group guesses and edu experience
num.indiv.out.9.me.emp <- length(unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0 & d$EmpMat==1),"ID"]))    #number mestizos with out-group guesses and emp experience

prop.ego.9.ma.ed <- num.indiv.ego.9.ma.ed/num.indiv.ego.9.ma    #proportion of indivs w/ inter-ethnic experience
prop.ego.9.me.emp <- num.indiv.ego.9.me.emp/num.indiv.ego.9.me

prop.in.9.ma.ed <- num.indiv.in.9.ma.ed/num.indiv.in.9.ma
prop.in.9.me.emp <- num.indiv.in.9.me.emp/num.indiv.in.9.me

prop.out.9.ma.ed <- num.indiv.out.9.ma.ed/num.indiv.out.9.ma
prop.out.9.me.emp <- num.indiv.out.9.me.emp/num.indiv.out.9.me

ego.9.ma <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1),"ID"])
ego.9.1.ma <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1 & d$response==1),"ID"])  #IDs of machis with positive ego response
ego.9.0.ma <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1 & d$response==0),"ID"])  #IDs of machis with negative ego response
ego.9.me <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0),"ID"])
ego.9.1.me <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0 & d$response==1),"ID"])  #IDs of mestizos with positive ego response
ego.9.0.me <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0 & d$response==0),"ID"])  #IDs of mestizos with negative ego response

ing.9.ma <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1),"ID"])
ing.9.1.ma <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1 & d$response==1),"ID"])  #IDs of machis with positive in guess
ing.9.0.ma <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1 & d$response==0),"ID"])  #IDs of machis with negative in guess
ing.9.me <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0),"ID"])
ing.9.1.me <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0 & d$response==1),"ID"])  #IDs of mestizos with positive in guess
ing.9.0.me <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0 & d$response==0),"ID"])  #IDs of mestizos with negative in guess

out.9.ma <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1),"ID"])
out.9.1.ma <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1 & d$response==1),"ID"])  #IDs of machis with positive out guess
out.9.0.ma <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1 & d$response==0),"ID"])  #IDs of machis with negative out guess
out.9.me <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0),"ID"])
out.9.1.me <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0 & d$response==1),"ID"])  #IDs of mestizos with positive out guess
out.9.0.me <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0 & d$response==0),"ID"])  #IDs of mestizos with negative out guess

#proportion of machis and mestizos answering ego=1
prop.ego.9.1.ma <- length(ego.9.1.ma)/num.indiv.ego.9.ma   
prop.ego.9.1.me <- length(ego.9.1.me)/num.indiv.ego.9.me

#proportion of phenotypes assuming >50% of group A ego response=1 and <50% of group B ego response=1
num.indiv.all.9.ma <- length(Reduce(intersect, list(ego.9.ma, ing.9.ma, out.9.ma)))
num.indiv.all.9.me <- length(Reduce(intersect, list(ego.9.me, ing.9.me, out.9.me)))

pA11_all <- length(Reduce(intersect, list(ego.9.1.ma, ing.9.1.ma, out.9.1.ma)))/num.indiv.all.9.ma #reduce applies the function "intersect" to the cumulative output of each list element 
pA22_all <- length(Reduce(intersect, list(ego.9.0.ma, ing.9.0.ma, out.9.0.ma)))/num.indiv.all.9.ma
pA1X_all <- length(Reduce(intersect, list(ego.9.1.ma, ing.9.1.ma, out.9.0.ma)))/num.indiv.all.9.ma
pA2X_all <- length(Reduce(intersect, list(ego.9.0.ma, ing.9.1.ma, out.9.0.ma)))/num.indiv.all.9.ma

pA1i_all <- length(Reduce(intersect, list(ego.9.1.ma, ing.9.0.ma, out.9.0.ma)))/num.indiv.all.9.ma
pA1b_all <- length(Reduce(intersect, list(ego.9.1.ma, ing.9.0.ma, out.9.1.ma)))/num.indiv.all.9.ma

pA2o_all <- length(Reduce(intersect, list(ego.9.0.ma, ing.9.1.ma, out.9.1.ma)))/num.indiv.all.9.ma
pA2b_all <- length(Reduce(intersect, list(ego.9.0.ma, ing.9.0.ma, out.9.1.ma)))/num.indiv.all.9.ma

#pA11_all + pA22_all + pA1X_all + pA2X_all + pA1i_all + pA1b_all + pA2o_all + pA2b_all

pB11_all <- length(Reduce(intersect, list(ego.9.1.me, ing.9.1.me, out.9.1.me)))/num.indiv.all.9.me
pB22_all <- length(Reduce(intersect, list(ego.9.0.me, ing.9.0.me, out.9.0.me)))/num.indiv.all.9.me
pB1X_all <- length(Reduce(intersect, list(ego.9.1.me, ing.9.0.me, out.9.1.me)))/num.indiv.all.9.me
pB2X_all <- length(Reduce(intersect, list(ego.9.0.me, ing.9.0.me, out.9.1.me)))/num.indiv.all.9.me

pB1o_all <- length(Reduce(intersect, list(ego.9.1.me, ing.9.0.me, out.9.0.me)))/num.indiv.all.9.me
pB1b_all <- length(Reduce(intersect, list(ego.9.1.me, ing.9.1.me, out.9.0.me)))/num.indiv.all.9.me

pB2i_all <- length(Reduce(intersect, list(ego.9.0.me, ing.9.1.me, out.9.1.me)))/num.indiv.all.9.me
pB2b_all <- length(Reduce(intersect, list(ego.9.0.me, ing.9.1.me, out.9.0.me)))/num.indiv.all.9.me

#pB11_all+pB22_all+pB1X_all+pB2X_all+pB1o_all+pB1b_all+pB2i_all+pB2b_all


ego.9.ma.edu <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1 & d$EdMes==1),"ID"])
ego.9.1.ma.edu <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1 & d$EdMes==1 & d$response==1),"ID"])  #IDs of machis with edu experience with positive ego response
ego.9.0.ma.edu <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==1 & d$EdMes==1 & d$response==0),"ID"])  #IDs of machis with edu experience with negative ego response
ego.9.me.emp <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0 & d$EmpMat==1),"ID"]) 
ego.9.1.me.emp <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0 & d$EmpMat==1 & d$response==1),"ID"])  #IDs of mestizos with emp experience with positive ego response
ego.9.0.me.emp <- unique(d[which(d$target.num==1 & d$question==9 & d$Machi==0 & d$EmpMat==1 & d$response==0),"ID"])  #IDs of mestizos with emp experience with negative ego response

ing.9.ma.edu <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1 & d$EdMes==1),"ID"])
ing.9.1.ma.edu <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1 & d$EdMes==1 & d$response==1),"ID"])
ing.9.0.ma.edu <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==1 & d$EdMes==1 & d$response==0),"ID"])
ing.9.me.emp <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0 & d$EmpMat==1),"ID"])
ing.9.1.me.emp <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0 & d$EmpMat==1 & d$response==1),"ID"])
ing.9.0.me.emp <- unique(d[which(d$target.num==2 & d$question==9 & d$Machi==0 & d$EmpMat==1 & d$response==0),"ID"])

out.9.ma.edu <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1 & d$EdMes==1),"ID"])
out.9.1.ma.edu <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1 & d$EdMes==1 & d$response==1),"ID"])
out.9.0.ma.edu <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==1 & d$EdMes==1 & d$response==0),"ID"])
out.9.me.emp <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0 & d$EmpMat==1),"ID"])
out.9.1.me.emp <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0 & d$EmpMat==1 & d$response==1),"ID"])
out.9.0.me.emp <- unique(d[which(d$target.num==3 & d$question==9 & d$Machi==0 & d$EmpMat==1 & d$response==0),"ID"])

#proportion of phenotypes among machis and mestizos with inter-ethnic experience assuming >50% of group A ego response=1 and <50% of group B ego response=1
num.indiv.all.9.ma.edu <- length(Reduce(intersect, list(ego.9.ma.edu, ing.9.ma.edu, out.9.ma.edu)))
num.indiv.all.9.me.emp <- length(Reduce(intersect, list(ego.9.me.emp, ing.9.me.emp, out.9.me.emp)))

pA11_edu <- length(Reduce(intersect, list(ego.9.1.ma.edu, ing.9.1.ma.edu, out.9.1.ma.edu)))/num.indiv.all.9.ma.edu #reduce applies the function "intersect" to the cumulative output of each list element 
pA22_edu <- length(Reduce(intersect, list(ego.9.0.ma.edu, ing.9.0.ma.edu, out.9.0.ma.edu)))/num.indiv.all.9.ma.edu
pA1X_edu <- length(Reduce(intersect, list(ego.9.1.ma.edu, ing.9.1.ma.edu, out.9.0.ma.edu)))/num.indiv.all.9.ma.edu
pA2X_edu <- length(Reduce(intersect, list(ego.9.0.ma.edu, ing.9.1.ma.edu, out.9.0.ma.edu)))/num.indiv.all.9.ma.edu

pA1i_edu <- length(Reduce(intersect, list(ego.9.1.ma.edu, ing.9.0.ma.edu, out.9.0.ma.edu)))/num.indiv.all.9.ma.edu
pA1b_edu <- length(Reduce(intersect, list(ego.9.1.ma.edu, ing.9.0.ma.edu, out.9.1.ma.edu)))/num.indiv.all.9.ma.edu

pA2o_edu <- length(Reduce(intersect, list(ego.9.0.ma.edu, ing.9.1.ma.edu, out.9.1.ma.edu)))/num.indiv.all.9.ma.edu
pA2b_edu <- length(Reduce(intersect, list(ego.9.0.ma.edu, ing.9.0.ma.edu, out.9.1.ma.edu)))/num.indiv.all.9.ma.edu

#pA11_edu+pA22_edu+pA1X_edu+pA2X_edu+pA1i_edu+pA1b_edu+pA2o_edu+pA2b_edu

pB11_emp <- length(Reduce(intersect, list(ego.9.1.me.emp, ing.9.1.me.emp, out.9.1.me.emp)))/num.indiv.all.9.me.emp
pB22_emp <- length(Reduce(intersect, list(ego.9.0.me.emp, ing.9.0.me.emp, out.9.0.me.emp)))/num.indiv.all.9.me.emp
pB1X_emp <- length(Reduce(intersect, list(ego.9.1.me.emp, ing.9.0.me.emp, out.9.1.me.emp)))/num.indiv.all.9.me.emp
pB2X_emp <- length(Reduce(intersect, list(ego.9.0.me.emp, ing.9.0.me.emp, out.9.1.me.emp)))/num.indiv.all.9.me.emp

pB1o_emp <- length(Reduce(intersect, list(ego.9.1.me.emp, ing.9.0.me.emp, out.9.0.me.emp)))/num.indiv.all.9.me.emp
pB1b_emp <- length(Reduce(intersect, list(ego.9.1.me.emp, ing.9.1.me.emp, out.9.0.me.emp)))/num.indiv.all.9.me.emp

pB2i_emp <- length(Reduce(intersect, list(ego.9.0.me.emp, ing.9.1.me.emp, out.9.1.me.emp)))/num.indiv.all.9.me.emp
pB2b_emp <- length(Reduce(intersect, list(ego.9.0.me.emp, ing.9.1.me.emp, out.9.0.me.emp)))/num.indiv.all.9.me.emp

#pB11_emp+pB22_emp+pB1X_emp+pB2X_emp+pB1o_emp+pB1b_emp+pB2i_emp+pB2b_emp

samp_char <- matrix(data=c(num.indiv.ego.9, num.indiv.ego.9.ma, num.indiv.ego.9.ma.ed, prop.ego.9.ma.ed, num.indiv.ego.9.me, num.indiv.ego.9.me.emp, prop.ego.9.me.emp,
                           num.indiv.in.9, num.indiv.in.9.ma, num.indiv.in.9.ma.ed, prop.in.9.ma.ed, num.indiv.in.9.me, num.indiv.in.9.me.emp, prop.in.9.me.emp,
                           num.indiv.out.9, num.indiv.out.9.ma, num.indiv.out.9.ma.ed, prop.out.9.ma.ed, num.indiv.out.9.me, num.indiv.out.9.me.emp, prop.out.9.me.emp),
                    nrow=7, ncol=3, byrow=F, dimnames=list(c("num_indivs","num_mach","num_mach_ed","prop_mach_ed","num_mest","num_mest_emp","prop_mest_emp"),
                                                           c("ego","in","out"))
             )






#recalculate dataset characteristics
K <- length(unique(d$question)) 						#number of questions
J <- length(unique(d$newID)) 							#number of people
N <- nrow(d) 											#number of responses across all targets
nummach <- length(unique(d[which(d$Machi==1),"ID"]))	#number machis
nummest <- length(unique(d[which(d$Machi==0),"ID"]))	#number mestizos

