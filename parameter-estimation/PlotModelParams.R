
######### Combined plot using results of m1 and m2, 5 iterations of theoretical model

#str(post1)
#str(post2)

#parameter posteriors using data for all machis and mestizo
all_ma_a <- post1$a_constr[,1]
all_me_a <- post1$a_constr[,2]
all_ma_b <- post1$b_constr[,1]
all_me_b <- post1$b_constr[,2]
all_m <- post1$m
all_i <- post1$i
all_c <- post1$c
all_mu <- post1$mu

#parameter posteriors using data for only machis and mestizo with inter-ethnic experience
exp_ma_a <- post2$a_constr[,1]
exp_me_a <- post2$a_constr[,2]
exp_ma_b <- post2$b_constr[,1]
exp_me_b <- post2$b_constr[,2]
exp_m <- post2$m
exp_i <- post2$i
exp_c <- post2$c
exp_mu <- post2$mu

#create lists for plotting

plotlist <- list( all_ma_b, exp_ma_b,
                  all_me_b, exp_me_b,
                  all_m, exp_m,
                  all_i, exp_i,
                  all_mu, exp_mu,
                  all_c, exp_c,
                  all_ma_a, exp_ma_a,
                  all_me_a, exp_me_a )

names(plotlist) <- c("bS", "bSexp",
                     "bL", "bLexp",
                     "m", "mexp",
                     "i", "iexp",
                     "mu", "muexp",
                     "c", "cexp",
                     "aS", "aSexp",
                     "aL", "aLexp" )


#### density plots of ego responses
pdf(file="./Plots/modelparams_5.pdf", 
height=7, width=7)
par(mar = c(2, 0, 0, 0), oma = c(3, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

denschart3( rev(plotlist),
            #labels=rev(quest_names)
            labels="",
            adjust=1, #0.05,
            color=rep( c( #grey(0.45),
                          rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                          NA
                          
                            ), times=8 ),
            colorHPDI=rep( c( NA,
                              NA          
                            ), times=8 ),
            polyborder=rep(c(NA,"Black"), times=8),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.7,
            xlim=range( 0, 5),
            yvals = c(0.5,0.5,
                      2,2,
                      3.5,3.5,
                      5,5,
                      6.5,6.5,
                      8,8,
                      9.5,9.5,
                      11,11)
 )
axis(side=2,
  col="white",
  at=c(0.5, 2, 3.5, 5, 6.5, 8, 9.5, 11), 
  labels=rev( c( expression('b'['S']),expression('b'['L']),"m","i",expression(mu),"c",expression('a'['S']),expression('a'['L']) ) ), 
                 las=1, cex.axis=1.5) #left
axis(side=1, at=c(0,0.5,1,2,3,4,5), labels=c(0,0.5,1,2,3,4,5), cex.axis=1)

lines(x=list( x=c(mean(plotlist$aL),mean(plotlist$aL)), y=c(0,1) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$aLexp),mean(plotlist$aLexp)), y=c(0,1) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$aS),mean(plotlist$aS)), y=c(1.7,2.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$aSexp),mean(plotlist$aSexp)), y=c(1.7,2.5) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$c),mean(plotlist$c)), y=c(3.2,4) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$cexp),mean(plotlist$cexp)), y=c(3.2,4) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$mu),mean(plotlist$mu)), y=c(4.7,5.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$muexp),mean(plotlist$muexp)), y=c(4.7,5.5) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$i),mean(plotlist$i)), y=c(6.2,7) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$iexp),mean(plotlist$iexp)), y=c(6.2,7) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$m),mean(plotlist$m)), y=c(7.7,8.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$mexp),mean(plotlist$mexp)), y=c(7.7,8.5) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$bL),mean(plotlist$bL)), y=c(9.2,10) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$bLexp),mean(plotlist$bLexp)), y=c(9.2,10) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$bS),mean(plotlist$bS)), y=c(10.7,11.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$bSexp),mean(plotlist$bSexp)), y=c(10.7,11.5) ), lty=1, lwd=2, col="blue")

rect(-0.5,0,0,12, col="white", border = NA) #cut at 0 and 1, (xleft, ybottom, xright, ytop)
rect(1,0,1.5,4, col="white", border = NA)

lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=1) #vertical line
lines(x=list( x=c(1,1), y=c(0,4) ), lty=2, lwd=1) #vertical line

lines(x=list( x=c(0,5), y=c(0.2,0.2) ), lty=1, lwd=2, col="black", lend="butt") #horizontal line
lines(x=list( x=c(2,2.5), y=c(2.5,2.5) ), lty=1, lwd=3, col="black", lend="butt") #horizontal line
lines(x=list( x=c(2,2.5), y=c(1.5,1.5) ), lty=1, lwd=3, col="blue", lend="butt") #horizontal line

mtext(side=1, line=-8.2, adj=0.63, text="All Individuals", cex=1)
mtext(side=1, line=-4.9, adj=0.98, text="Individuals w/ inter-ethnic experience", cex=1)

mtext(side=1, line=3, adj=0.5, text="Posterior Parameter Estimate", cex=1)

graphics.off()



######### Combined plot using results of m3 and m4, 25 interations of model

#str(post3)
#str(post4)

all_ma_a <- post3$a_constr[,1]
all_me_a <- post3$a_constr[,2]
all_ma_b <- post3$b_constr[,1]
all_me_b <- post3$b_constr[,2]
all_m <- post3$m
all_i <- post3$i
all_c <- post3$c
all_mu <- post3$mu

exp_ma_a <- post4$a_constr[,1]
exp_me_a <- post4$a_constr[,2]
exp_ma_b <- post4$b_constr[,1]
exp_me_b <- post4$b_constr[,2]
exp_m <- post4$m
exp_i <- post4$i
exp_c <- post4$c
exp_mu <- post4$mu

#create lists for plotting

plotlist <- list( all_ma_b, exp_ma_b,
                  all_me_b, exp_me_b,
                  all_m, exp_m,
                  all_i, exp_i,
                  all_mu, exp_mu,
                  all_c, exp_c,
                  all_ma_a, exp_ma_a,
                  all_me_a, exp_me_a )

names(plotlist) <- c("bS", "bSexp",
                     "bL", "bLexp",
                     "m", "mexp",
                     "i", "iexp",
                     "mu", "muexp",
                     "c", "cexp",
                     "aS", "aSexp",
                     "aL", "aLexp" )


#### density plots of ego responses
pdf(file="./Plots/modelparams_25.pdf", 
height=7, width=7)
par(mar = c(2, 0, 0, 0), oma = c(3, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

denschart3( rev(plotlist),
            labels="",
            adjust=1,
            color=rep( c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                          NA
                        ), times=8 ),
            colorHPDI=rep( c( NA,
                              NA         
                            ), times=8 ),
            polyborder=rep(c(NA,"Black"), times=8),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.7,
            xlim=range( 0, 5),
            yvals = c(0.5,0.5,
                      2,2,
                      3.5,3.5,
                      5,5,
                      6.5,6.5,
                      8,8,
                      9.5,9.5,
                      11,11)
 )
axis(side=2,
  col="white",
  at=c(0.5, 2, 3.5, 5, 6.5, 8, 9.5, 11), 
  labels=rev( c( expression('b'['S']),expression('b'['L']),"m","i",expression(mu),"c",expression('a'['S']),expression('a'['L']) ) ), 
                 las=1, cex.axis=1.5) #left
axis(side=1, at=c(0,0.5,1,2,3,4,5), labels=c(0,0.5,1,2,3,4,5), cex.axis=1)

lines(x=list( x=c(mean(plotlist$aL),mean(plotlist$aL)), y=c(0,1) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$aLexp),mean(plotlist$aLexp)), y=c(0,1) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$aS),mean(plotlist$aS)), y=c(1.7,2.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$aSexp),mean(plotlist$aSexp)), y=c(1.7,2.5) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$c),mean(plotlist$c)), y=c(3.2,4) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$cexp),mean(plotlist$cexp)), y=c(3.2,4) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$mu),mean(plotlist$mu)), y=c(4.7,5.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$muexp),mean(plotlist$muexp)), y=c(4.7,5.5) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$i),mean(plotlist$i)), y=c(6.2,7) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$iexp),mean(plotlist$iexp)), y=c(6.2,7) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$m),mean(plotlist$m)), y=c(7.7,8.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$mexp),mean(plotlist$mexp)), y=c(7.7,8.5) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$bL),mean(plotlist$bL)), y=c(9.2,10) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$bLexp),mean(plotlist$bLexp)), y=c(9.2,10) ), lty=1, lwd=2, col="blue")

lines(x=list( x=c(mean(plotlist$bS),mean(plotlist$bS)), y=c(10.7,11.5) ), lty=1, lwd=2, col="black")
lines(x=list( x=c(mean(plotlist$bSexp),mean(plotlist$bSexp)), y=c(10.7,11.5) ), lty=1, lwd=2, col="blue")

rect(-0.5,0,0,12, col="white", border = NA) #cut at 0 and 1, (xleft, ybottom, xright, ytop)
rect(1,0,1.5,4, col="white", border = NA)

lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=1) #vertical line
lines(x=list( x=c(1,1), y=c(0,4) ), lty=2, lwd=1) #vertical line

lines(x=list( x=c(0,5), y=c(0.2,0.2) ), lty=1, lwd=2, col="black", lend="butt") #horizontal line
lines(x=list( x=c(2,2.5), y=c(2.5,2.5) ), lty=1, lwd=3, col="black", lend="butt") #horizontal line
lines(x=list( x=c(2,2.5), y=c(1.5,1.5) ), lty=1, lwd=3, col="blue", lend="butt") #horizontal line

mtext(side=1, line=-8.2, adj=0.63, text="All Individuals", cex=1)
mtext(side=1, line=-4.9, adj=0.98, text="Individuals w/ inter-ethnic experience", cex=1)

mtext(side=1, line=3, adj=0.5, text="Posterior Parameter Estimate", cex=1)

graphics.off()



