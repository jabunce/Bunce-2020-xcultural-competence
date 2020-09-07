

######### Combined plots using results of m1 and m2

#### density plots of ego responses
pdf(file="./Plots/response_dens.pdf", 
height=7, width=7)
#layout( matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect=TRUE)#widths=c(1.9,1,1) )
par(mar = c(2, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart4( split(ego_q9, col(ego_q9)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
      colorHPDI=c(grey(0.45),grey(0.45)),
      polyborder=NA,
      HPDI=0.9,
      border=NA, yaxt="n",
      cex=0.8, height=0.7,
      xlim=range( 0, 1),
      hline=c(0.25,0.5,0.75),
      hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(ego_q9)), 
  labels=c("Matsigenka","Mestizos"), las=1, cex.axis=1.5) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(prop.ego.9.1.ma, prop.ego.9.1.ma), x=c(0.5,1.5) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(prop.ego.9.1.me, prop.ego.9.1.me), x=c(1.5,2.5) ), lty=1, lwd=4, col="green3")

#mtext(side=3, line=-1, adj=0.5, text="Matsigenka", cex=1)
mtext(side=2, line=3, adj=0.5, text="Pr(Response=1)", cex=1)

graphics.off()



#phenotype decomposition, all machis and mestizos
pheno_q9_ma_plot <- pheno_q9_ma[,c(3,8,1,2,
                                   4,6,10,11)]
pheno_names_ma_plot <- c("S11","S22","S1X","S2X",
                         "S1in","S1both","S2out","S2both")

pheno_q9_me_plot <- pheno_q9_me[,c(3,8,1,2,
                                   5,6,9,11)]
pheno_names_me_plot <- c("L11","L22","L1X","L2X",
                         "L1out","L1both","L2in","L2both")

#### density plots of xcult comp
pdf(file="./Plots/decomp_pheno_dens.pdf", 
height=7, width=8)
layout( matrix(c(1,2), 2, 1, byrow = TRUE), respect=TRUE, widths = lcm(16))
par(mar = c(2, 0, 0, 0), oma = c(1, 1, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart4( split(pheno_q9_ma_plot, col(pheno_q9_ma_plot)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
      colorHPDI=c(grey(0.45),"White","Red","Blue",rep("yellow",4),
                  grey(0.45),"White","Red","Blue",rep("yellow",4)),
      polyborder=c(NA,"Black",NA,NA,rep(NA,4),
                   NA,"Black",NA,NA,rep(NA,4)),
      HPDI=0.9,
      border=NA, yaxt="n",
      cex=0.8, height=0.7,
      xlim=range( 0, 1),
      hline=c(0.25,0.5,0.75),
      hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_plot)), 
  labels=pheno_names_ma_plot, las=1, cex.axis=0.70) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,10) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_all, pA11_all), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3") #pA11_all from FormatData
lines(x=list( y=c(pA22_all, pA22_all), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_all, pA1X_all), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_all, pA2X_all), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

lines(x=list( y=c(pA1i_all, pA1i_all), x=c(4.7,5.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1b_all, pA1b_all), x=c(5.7,6.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2o_all, pA2o_all), x=c(6.7,7.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2b_all, pA2b_all), x=c(7.7,8.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-1, adj=0.5, text="Matsigenka", cex=1.5)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)



denschart4( split(pheno_q9_me_plot, col(pheno_q9_me_plot)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
      colorHPDI=c(grey(0.45),"White","Red","Blue",rep("yellow",4),
                  grey(0.45),"White","Red","Blue",rep("yellow",4)),
      polyborder=c(NA,"Black",NA,NA,rep(NA,4),
                   NA,"Black",NA,NA,rep(NA,4)),
      HPDI=0.9,
      border=NA, yaxt="n",
      cex=0.8, height=0.7,
      xlim=range( 0, 1),
      hline=c(0.25,0.5,0.75),
      hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_plot)), 
  labels=pheno_names_me_plot, las=1, cex.axis=0.70) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,10) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_all, pB11_all), x=c(0.7,1.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB22_all, pB22_all), x=c(1.7,2.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB1X_all, pB1X_all), x=c(2.7,3.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB2X_all, pB2X_all), x=c(3.7,4.3) ), lty=1, lwd=4, col="green3")

lines(x=list( y=c(pB1o_all, pB1o_all), x=c(4.7,5.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB1b_all, pB1b_all), x=c(5.7,6.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB2i_all, pB2i_all), x=c(6.7,7.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB2b_all, pB2b_all), x=c(7.7,8.3) ), lty=1, lwd=4, col="green3")

mtext(side=3, line=-3, adj=0.5, text="Mestizos", cex=1.5)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)

graphics.off()







#phenotype decomposition, edu and emp experience
pheno_q9_ma_edu_plot <- pheno_q9_ma_edu[,c(3,8,1,2,
                                           4,6,10,11)]
pheno_names_ma_edu_plot <- c("S11","S22","S1X","S2X",
                             "S1in","S1both","S2out","S2both")

pheno_q9_me_emp_plot <- pheno_q9_me_emp[,c(3,8,1,2,
                                           5,6,9,11)]
pheno_names_me_emp_plot <- c("L11","L22","L1X","L2X",
                             "L1out","L1both","L2in","L2both")

#### density plots of xcult comp
pdf(file="./Plots/decomp_pheno_eduemp_dens.pdf", 
height=7, width=8)
layout( matrix(c(1,2), 2, 1, byrow = TRUE), respect=TRUE, widths = lcm(16))
par(mar = c(2, 0, 0, 0), oma = c(1, 1, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart4( split(pheno_q9_ma_edu_plot, col(pheno_q9_ma_edu_plot)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
      colorHPDI=c(grey(0.45),"White","Red","Blue",rep("yellow",4),
                  grey(0.45),"White","Red","Blue",rep("yellow",4)),
      polyborder=c(NA,"Black",NA,NA,rep(NA,4),
                   NA,"Black",NA,NA,rep(NA,4)),
      HPDI=0.9,
      border=NA, yaxt="n",
      cex=0.8, height=0.7,
      xlim=range( 0, 1),
      hline=c(0.25,0.5,0.75),
      hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_edu_plot)), 
  labels=pheno_names_ma_edu_plot, las=1, cex.axis=0.7) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,10) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_edu, pA11_edu), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_edu, pA22_edu), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_edu, pA1X_edu), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_edu, pA2X_edu), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

lines(x=list( y=c(pA1i_edu, pA1i_edu), x=c(4.7,5.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1b_edu, pA1b_edu), x=c(5.7,6.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2o_edu, pA2o_edu), x=c(6.7,7.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2b_edu, pA2b_edu), x=c(7.7,8.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-1, adj=0.5, text="Matsigenka educated with Mestizos", cex=1.5)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)



denschart4( split(pheno_q9_me_emp_plot, col(pheno_q9_me_emp_plot)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
      colorHPDI=c(grey(0.45),"White","Red","Blue",rep("yellow",4),
                  grey(0.45),"White","Red","Blue",rep("yellow",4)),
      polyborder=c(NA,"Black",NA,NA,rep(NA,4),
                   NA,"Black",NA,NA,rep(NA,4)),
      HPDI=0.9,
      border=NA, yaxt="n",
      cex=0.8, height=0.7,
      xlim=range( 0, 1),
      hline=c(0.25,0.5,0.75),
      hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_emp_plot)), 
  labels=pheno_names_me_emp_plot, las=1, cex.axis=0.7) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,10) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,10) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_emp, pB11_emp), x=c(0.7,1.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB22_emp, pB22_emp), x=c(1.7,2.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB1X_emp, pB1X_emp), x=c(2.7,3.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB2X_emp, pB2X_emp), x=c(3.7,4.3) ), lty=1, lwd=4, col="green3")

lines(x=list( y=c(pB1o_emp, pB1o_emp), x=c(4.7,5.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB1b_emp, pB1b_emp), x=c(5.7,6.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB2i_emp, pB2i_emp), x=c(6.7,7.3) ), lty=1, lwd=4, col="green3")
lines(x=list( y=c(pB2b_emp, pB2b_emp), x=c(7.7,8.3) ), lty=1, lwd=4, col="green3")

mtext(side=3, line=-3, adj=0.5, text="Mestizo employers of Matsigenka", cex=1.5)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)

graphics.off()








#### composite non-cross-cultural competence
pheno_q9_ma_plot_cons <- pheno_q9_ma[,c(7,12,1,2)]
pheno_names_ma_plot_cons <- c("S1notX","S2notX","S1X","S2X")

pheno_q9_me_plot_cons <- pheno_q9_me[,c(7,12,1,2)]
pheno_names_me_plot_cons <- c("L1notX","L2notX","L1X","L2X")


pheno_q9_ma_edu_plot_cons <- pheno_q9_ma_edu[,c(7,12,1,2)]
pheno_names_ma_edu_plot_cons <- c("S1notX","S2notX","S1X","S2X")

pheno_q9_me_emp_plot_cons <- pheno_q9_me_emp[,c(7,12,1,2)]
pheno_names_me_emp_plot_cons <- c("L1notX","L2notX","L1X","L2X")



pdf(file="./Plots/pheno_dens_non_xcult.pdf", 
height=7, width=7)
layout( matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect=TRUE)#widths=c(1.9,1,1) )
par(mar = c(2, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



denschart4( split(pheno_q9_ma_plot_cons, col(pheno_q9_ma_plot_cons)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_plot_cons)), 
  labels=c(" "," "," "," "), las=1, cex.axis=1) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_all+pA1i_all+pA1b_all, pA11_all+pA1i_all+pA1b_all), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_all+pA2o_all+pA2b_all, pA22_all+pA2o_all+pA2b_all), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_all, pA1X_all), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_all, pA2X_all), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-1, adj=0.5, text="All Matsigenka", cex=1)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)


denschart4( split(pheno_q9_me_plot_cons, col(pheno_q9_me_plot_cons)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_plot_cons)), 
  labels=c(" "," "," "," "), las=1, cex.axis=1) #left
#axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_all+pB1o_all+pB1b_all, pB11_all+pB1o_all+pB1b_all), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB22_all+pB2i_all+pB2b_all, pB22_all+pB2i_all+pB2b_all), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB1X_all, pB1X_all), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB2X_all, pB2X_all), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-1, adj=0.5, text="All Mestizos", cex=1)


denschart4( split(pheno_q9_ma_edu_plot_cons, col(pheno_q9_ma_edu_plot_cons)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_edu_plot_cons)), 
  labels=pheno_names_ma_edu_plot_cons, las=1, cex.axis=0.85) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_edu+pA1i_edu+pA1b_edu, pA11_edu+pA1i_edu+pA1b_edu), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_edu+pA2o_edu+pA2b_edu, pA22_edu+pA2o_edu+pA2b_edu), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_edu, pA1X_edu), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_edu, pA2X_edu), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-2, adj=0.05, text="Matsigenka educated with Mestizos", cex=0.85)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)


denschart4( split(pheno_q9_me_emp_plot_cons, col(pheno_q9_me_emp_plot_cons)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75) 
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_emp_plot_cons)), 
  labels=pheno_names_me_emp_plot_cons, las=1, cex.axis=0.85) #left
#axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_emp+pB1o_emp+pB1b_emp, pB11_emp+pB1o_emp+pB1b_emp), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB22_emp+pB2i_emp+pB2b_emp, pB22_emp+pB2i_emp+pB2b_emp), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB1X_emp, pB1X_emp), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB2X_emp, pB2X_emp), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-2, adj=0.2, text="Mestizo employers of Matsigenka", cex=0.85)

graphics.off()







#### composite non-uni-cultural competence
pheno_q9_ma_plot_decom <- cbind(pheno_q9_ma[,"pA11"],
                                pheno_q9_ma[,"pA22"],
                                pheno_q9_ma[,"pA1X"]+pheno_q9_ma[,"pA1i"]+pheno_q9_ma[,"pA1b"],
                                pheno_q9_ma[,"pA2X"]+pheno_q9_ma[,"pA2o"]+pheno_q9_ma[,"pA2b"] )
pheno_names_ma_plot_decom <- c("S11","S22","S1not1","S2not2")

pheno_q9_me_plot_decom <- cbind(pheno_q9_me[,"pB11"],
                                pheno_q9_me[,"pB22"],
                                pheno_q9_me[,"pB1X"]+pheno_q9_me[,"pB1o"]+pheno_q9_me[,"pB1b"],
                                pheno_q9_me[,"pB2X"]+pheno_q9_me[,"pB2i"]+pheno_q9_me[,"pB2b"] )
pheno_names_me_plot_decom <- c("L11","L22","L1not1","L2not2")

pheno_q9_ma_edu_plot_decom <- cbind(pheno_q9_ma_edu[,"pA11"],
                                pheno_q9_ma_edu[,"pA22"],
                                pheno_q9_ma_edu[,"pA1X"]+pheno_q9_ma_edu[,"pA1i"]+pheno_q9_ma_edu[,"pA1b"],
                                pheno_q9_ma_edu[,"pA2X"]+pheno_q9_ma_edu[,"pA2o"]+pheno_q9_ma_edu[,"pA2b"] )
pheno_names_ma_edu_plot_decom <- c("S11","S22","S1not1","S2not2")

pheno_q9_me_emp_plot_decom <- cbind(pheno_q9_me_emp[,"pB11"],
                                pheno_q9_me_emp[,"pB22"],
                                pheno_q9_me_emp[,"pB1X"]+pheno_q9_me_emp[,"pB1o"]+pheno_q9_me_emp[,"pB1b"],
                                pheno_q9_me_emp[,"pB2X"]+pheno_q9_me_emp[,"pB2i"]+pheno_q9_me_emp[,"pB2b"] )
pheno_names_me_emp_plot_decom <- c("L11","L22","L1not1","L2not2")



pdf(file="./Plots/pheno_dens_non_uni.pdf", 
height=7, width=7)
layout( matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect=TRUE)#widths=c(1.9,1,1) )
par(mar = c(2, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



denschart4( split(pheno_q9_ma_plot_decom, col(pheno_q9_ma_plot_decom)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_plot_decom)), 
  labels=c(" "," "," "," "), las=1, cex.axis=1) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_all, pA11_all), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_all, pA22_all), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_all+pA1i_all+pA1b_all, pA1X_all+pA1i_all+pA1b_all), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_all+pA2o_all+pA2b_all, pA2X_all+pA2o_all+pA2b_all), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-1, adj=0.5, text="All Matsigenka", cex=1)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)


denschart4( split(pheno_q9_me_plot_decom, col(pheno_q9_me_plot_decom)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_plot_decom)), 
  labels=c(" "," "," "," "), las=1, cex.axis=1) #left
#axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_all, pB11_all), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB22_all, pB22_all), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB1X_all+pB1o_all+pB1b_all, pB1X_all+pB1o_all+pB1b_all), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB2X_all+pB2i_all+pB2b_all, pB2X_all+pB2i_all+pB2b_all), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-1, adj=0.5, text="All Mestizos", cex=1)


denschart4( split(pheno_q9_ma_edu_plot_decom, col(pheno_q9_ma_edu_plot_decom)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_edu_plot_decom)), 
  labels=pheno_names_ma_edu_plot_decom, las=1, cex.axis=0.85) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_edu, pA11_edu), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_edu, pA22_edu), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_edu+pA1i_edu+pA1b_edu, pA1X_edu+pA1i_edu+pA1b_edu), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_edu+pA2o_edu+pA2b_edu, pA2X_edu+pA2o_edu+pA2b_edu), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-2, adj=0.05, text="Matsigenka educated with Mestizos", cex=0.85)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)


denschart4( split(pheno_q9_me_emp_plot_decom, col(pheno_q9_me_emp_plot_decom)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75) 
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_emp_plot_decom)), 
  labels=pheno_names_me_emp_plot_decom, las=1, cex.axis=0.85) #left
#axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_emp, pB11_emp), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB22_emp, pB22_emp), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB1X_emp+pB1o_emp+pB1b_emp, pB1X_emp+pB1o_emp+pB1b_emp), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB2X_emp+pB2i_emp+pB2b_emp, pB2X_emp+pB2i_emp+pB2b_emp), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=-2, adj=0.2, text="Mestizo employers of Matsigenka", cex=0.85)

graphics.off()








#### xcult and uni-cult competence
denom_ma <- rep(0,dim(pheno_q9_ma)[1])
denom_me <- rep(0,dim(pheno_q9_me)[1])
denom_ma_edu <- rep(0,dim(pheno_q9_ma_edu)[1])
denom_me_emp <- rep(0,dim(pheno_q9_me_emp)[1])

denom_ma <- (pheno_q9_ma[,"pA1X"]+pheno_q9_ma[,"pA2X"]+pheno_q9_ma[,"pA11"]+pheno_q9_ma[,"pA22"])
denom_me <- (pheno_q9_me[,"pB1X"]+pheno_q9_me[,"pB2X"]+pheno_q9_me[,"pB11"]+pheno_q9_me[,"pB22"])
denom_ma_edu <- (pheno_q9_ma_edu[,"pA1X"]+pheno_q9_ma_edu[,"pA2X"]+pheno_q9_ma_edu[,"pA11"]+pheno_q9_ma_edu[,"pA22"])
denom_me_emp <- (pheno_q9_me_emp[,"pB1X"]+pheno_q9_me_emp[,"pB2X"]+pheno_q9_me_emp[,"pB11"]+pheno_q9_me_emp[,"pB22"])

pheno_q9_ma_plot_reduc <- cbind(pheno_q9_ma[,"pA11"]/denom_ma,
                                pheno_q9_ma[,"pA22"]/denom_ma,
                                pheno_q9_ma[,"pA1X"]/denom_ma,
                                pheno_q9_ma[,"pA2X"]/denom_ma )
pheno_names_ma_plot_reduc <- c("S11","S22","S1X","S2X")

pheno_q9_me_plot_reduc <- cbind(pheno_q9_me[,"pB11"]/denom_me,
                                pheno_q9_me[,"pB22"]/denom_me,
                                pheno_q9_me[,"pB1X"]/denom_me,
                                pheno_q9_me[,"pB2X"]/denom_me )
pheno_names_me_plot_reduc <- c("L11","L22","L1X","L2X")

pheno_q9_ma_edu_plot_reduc <- cbind(pheno_q9_ma_edu[,"pA11"]/denom_ma_edu,
                                pheno_q9_ma_edu[,"pA22"]/denom_ma_edu,
                                pheno_q9_ma_edu[,"pA1X"]/denom_ma_edu,
                                pheno_q9_ma_edu[,"pA2X"]/denom_ma_edu )
pheno_names_ma_edu_plot_reduc <- c("S11","S22","S1X","S2X")

pheno_q9_me_emp_plot_reduc <- cbind(pheno_q9_me_emp[,"pB11"]/denom_me_emp,
                                pheno_q9_me_emp[,"pB22"]/denom_me_emp,
                                pheno_q9_me_emp[,"pB1X"]/denom_me_emp,
                                pheno_q9_me_emp[,"pB2X"]/denom_me_emp )
pheno_names_me_emp_plot_reduc <- c("L11","L22","L1X","L2X")


denom_ma_raw <- pA11_all+pA22_all+pA1X_all+pA2X_all
denom_me_raw <- pB11_all+pB22_all+pB1X_all+pB2X_all
denom_ma_edu <- pA11_edu+pA22_edu+pA1X_edu+pA2X_edu
denom_me_emp <- pB11_emp+pB22_emp+pB1X_emp+pB2X_emp



pdf(file="./Plots/pheno_dens_reduc.pdf", 
height=7, width=7)
layout( matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect=TRUE)#widths=c(1.9,1,1) )
par(mar = c(2, 0, 2, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



denschart4( split(pheno_q9_ma_plot_reduc, col(pheno_q9_ma_plot_reduc)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_plot_reduc)), 
  labels=c(" "," "," "," "), las=1, cex.axis=1) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1)
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_all/denom_ma_raw, pA11_all/denom_ma_raw), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_all/denom_ma_raw, pA22_all/denom_ma_raw), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_all/denom_ma_raw, pA1X_all/denom_ma_raw), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_all/denom_ma_raw, pA2X_all/denom_ma_raw), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=0, adj=0.5, text="All Matsigenka", cex=1)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)


denschart4( split(pheno_q9_me_plot_reduc, col(pheno_q9_me_plot_reduc)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_plot_reduc)), 
  labels=c(" "," "," "," "), las=1, cex.axis=1) #left
#axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,0) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,0) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,0) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_all/denom_me_raw, pB11_all/denom_me_raw), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB22_all/denom_me_raw, pB22_all/denom_me_raw), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB1X_all/denom_me_raw, pB1X_all/denom_me_raw), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB2X_all/denom_me_raw, pB2X_all/denom_me_raw), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=0, adj=0.5, text="All Mestizos", cex=1)


denschart4( split(pheno_q9_ma_edu_plot_reduc, col(pheno_q9_ma_edu_plot_reduc)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75)  
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_ma_edu_plot_reduc)), 
  labels=pheno_names_ma_edu_plot_reduc, las=1, cex.axis=0.85) #left
axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75, las=1) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,14.5) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,14.5) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pA11_edu/denom_ma_edu, pA11_edu/denom_ma_edu), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA22_edu/denom_ma_edu, pA22_edu/denom_ma_edu), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA1X_edu/denom_ma_edu, pA1X_edu/denom_ma_edu), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pA2X_edu/denom_ma_edu, pA2X_edu/denom_ma_edu), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=0.5, adj=0.05, text="Matsigenka educated with Mestizos", cex=0.85)
mtext(side=2, line=3, adj=0.5, text="Frequency", cex=0.75)


denschart4( split(pheno_q9_me_emp_plot_reduc, col(pheno_q9_me_emp_plot_reduc)), #splits the columns of a matrix into a list for plotting
      #labels=rev(pheno_names),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=c(grey(0.45),"White","Red","Blue"),
          polyborder=c(NA, "Black", NA, NA),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1),
          hline=c(0.25,0.5,0.75),
          hlinelwd=c(0.75,1.25,0.75) 
 )
#text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=1, #y
  col="white",
  at=c(1:ncol(pheno_q9_me_emp_plot_reduc)), 
  labels=pheno_names_me_emp_plot_reduc, las=1, cex.axis=0.85) #left
#axis(side=2, at=c(0,0.25,0.5,0.75,1), labels=c(0,0.25,0.5,0.75,1), cex.axis=0.75) #x
lines(x=list( y=c(0,0), x=c(-0.2,14.5) ), lty=1, lwd=0.75)
#lines(x=list( y=c(0.25,0.25), x=c(-0.2,0) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.75,0.75), x=c(-0.2,0) ), lty=2, lwd=0.75)
#lines(x=list( y=c(0.5,0.5), x=c(-0.2,0) ), lty=2, lwd=1.25)

#raw proportions
lines(x=list( y=c(pB11_emp/denom_me_emp, pB11_emp/denom_me_emp), x=c(0.7,1.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB22_emp/denom_me_emp, pB22_emp/denom_me_emp), x=c(1.7,2.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB1X_emp/denom_me_emp, pB1X_emp/denom_me_emp), x=c(2.7,3.3) ), lty=1, lwd=3, col="green3")
lines(x=list( y=c(pB2X_emp/denom_me_emp, pB2X_emp/denom_me_emp), x=c(3.7,4.3) ), lty=1, lwd=3, col="green3")

mtext(side=3, line=0.5, adj=0.2, text="Mestizo employers of Matsigenka", cex=0.85)

graphics.off()





