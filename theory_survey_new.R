### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Plot figure 1


library(truncnorm)

#define length of p_grid
p_grid <- seq(from=0.01, to=0.99, length.out=100)
#plots 
#signals
FEI_int_signal_G <-  dtruncnorm(p_grid,a=0,b=1, 0.8, 0.05)
FEI_int_signal_B <-  dtruncnorm(p_grid,a=0,b=1, 0.2, 0.05)
FEI_dis_signal_G <-  dtruncnorm(p_grid,a=0,b=1, 0.8, 0.6)
FEI_dis_signal_B <-  dtruncnorm(p_grid,a=0,b=1, 0.2, 0.6)

FEL_int_signal_G <-  dtruncnorm(p_grid,a=0,b=1, 0.8, 0.6)
FEL_int_signal_B <-  dtruncnorm(p_grid,a=0,b=1, 0.2, 0.6)
FEL_dis_signal_G <-  dtruncnorm(p_grid,a=0,b=1, 0.8, 0.05)
FEL_dis_signal_B <-  dtruncnorm(p_grid,a=0,b=1, 0.2, 0.05)


#plots 

#define prior
priorL <- dtruncnorm(p_grid,a=0,b=1, 0.5, 0.3)

priorI <- dtruncnorm(p_grid,a=0,b=1, 0.5, 0.3)

plot(p_grid, priorL,type='l')
plot(p_grid, priorI,type='l')


#posteriors

## FEL G

unstd.posterior_FEI_int_G <- priorI*FEI_int_signal_G
unstd.posterior_FEI_int_B <- priorI*FEI_int_signal_B
unstd.posterior_FEI_dis_G <- priorI*FEI_dis_signal_G
unstd.posterior_FEI_dis_B <- priorI*FEI_dis_signal_B


unstd.posterior_FEL_int_G <- priorL*FEL_int_signal_G
unstd.posterior_FEL_int_B <- priorL*FEL_int_signal_B
unstd.posterior_FEL_dis_G <- priorL*FEL_dis_signal_G
unstd.posterior_FEL_dis_B <- priorL*FEL_dis_signal_B

#posteriors

posterior_FEI_int_G <- unstd.posterior_FEI_int_G/sum(unstd.posterior_FEI_int_G)
posterior_FEI_int_B <- unstd.posterior_FEI_int_B/sum(unstd.posterior_FEI_int_B)
posterior_FEI_dis_G <- unstd.posterior_FEI_dis_G/sum(unstd.posterior_FEI_dis_G)
posterior_FEI_dis_B <- unstd.posterior_FEI_dis_B/sum(unstd.posterior_FEI_dis_B)

posterior_FEL_int_G <- unstd.posterior_FEL_int_G/sum(unstd.posterior_FEL_int_G)
posterior_FEL_int_B <- unstd.posterior_FEL_int_B/sum(unstd.posterior_FEL_int_B)
posterior_FEL_dis_G <- unstd.posterior_FEL_dis_G/sum(unstd.posterior_FEL_dis_G)
posterior_FEL_dis_B <- unstd.posterior_FEL_dis_B/sum(unstd.posterior_FEL_dis_B)




### plot posteriors 

plot(p_grid , posterior_FEI_int_G, type="l")

plot(p_grid , posterior_FEI_int_B, type="l")

plot(p_grid , posterior_FEI_dis_G, type="l")

plot(p_grid , posterior_FEI_dis_B, type="l")

pdf("Posterior_FEI.pdf", width = 8, height = 6)
plot(p_grid , posterior_FEI_int_G, type="l",col="lightgray", lty="dashed",
     ylab=("Posterior - FEI"),
     xlab=("Utility"), ylim=c(0,0.10),lwd =3, cex.axis=1, cex.lab=1.4)
lines(p_grid , posterior_FEI_int_B,col="lightgray", lwd=3)
lines(p_grid , posterior_FEI_dis_G,col="black", lty="dashed", lwd=3)
lines(p_grid , posterior_FEI_dis_B,col="black", lwd=3)
lines(p_grid, priorI/60, col="darkgray", lwd=3)
legend(0.30, 0.09, legend=c("Par. Good", "Par. Bad", "Non-par. Good","Non-par. Bad", "Prior"),
       col=c("lightgray","lightgray", "black", "black", "darkgray"),lty=c(2,1,2,1,1), cex=1, lwd=3,
       box.lty=0)
dev.off()

png("Posterior_FEI_color.png", width = 200, height = 150, units='mm',  res = 300)
plot(p_grid , posterior_FEI_int_G, type="l",col="red",
     ylab=("Posterior - FEI"),
     xlab=("Utility"), ylim=c(0,0.10),lwd =3, cex.axis=1, cex.lab=1.4)
lines(p_grid , posterior_FEI_int_B,col="blue", lwd=3)
lines(p_grid , posterior_FEI_dis_G,col="green", lwd=3)
lines(p_grid , posterior_FEI_dis_B,col="black", lwd=3)
lines(p_grid, priorI/60, col="darkgray", lwd=3)
legend(0.30, 0.09, legend=c("Par. Good", "Par. Bad", "Non-par. Good","Non-par. Bad", "Prior"),
       col=c("red","blue", "green", "black", "darkgray"),cex=1, lwd=3,
       box.lty=0)
dev.off()

plot(p_grid , posterior_FEL_G_int_G, type="l")

plot(p_grid , posterior_FEL_G_int_B, type="l")

plot(p_grid , posterior_FEL_G_dis_G, type="l")

plot(p_grid , posterior_FEL_G_dis_B, type="l")
# Figure 1

pdf("Posterior_FEL.pdf", width = 8, height = 6)
plot(p_grid , posterior_FEL_int_G, type="l",col="lightgray",lty="dashed", ylab=("Posterior - FEL"),
     xlab=("Utility"), ylim=c(0,0.10),lwd =3, cex.axis=1, cex.lab=1.4)
lines(p_grid , posterior_FEL_int_B,col="lightgray", lwd=3)
lines(p_grid , posterior_FEL_dis_G,col="black", lty="dashed", lwd=3)
lines(p_grid , posterior_FEL_dis_B,col="black", lwd=3)
lines(p_grid, priorL/60, col="darkgray", lwd=3)
legend(0.32, 0.09, legend=c("Par. Good", "Par. Bad", "Non-par. Good","Non-par. Bad", "Prior"),
       col=c("lightgray","lightgray", "black", "black", "darkgray"),lty=c(2,1,2,1,1), cex=1, lwd=3,
       box.lty=0)
dev.off()

png("Posterior_FEL_color.png", width = 200, height = 150, units='mm',  res = 300)
plot(p_grid , posterior_FEL_int_G, type="l",col="red",ylab=("Posterior - FEL"),
     xlab=("Utility"), ylim=c(0,0.10),lwd =3, cex.axis=1, cex.lab=1.4)
lines(p_grid , posterior_FEL_int_B,col="blue", lwd=3)
lines(p_grid , posterior_FEL_dis_G,col="green",  lwd=3)
lines(p_grid , posterior_FEL_dis_B,col="black", lwd=3)
lines(p_grid, priorL/60, col="darkgray", lwd=3)
legend(0.32, 0.09, legend=c("Par. Good", "Par. Bad", "Non-par. Good","Non-par. Bad", "Prior"),
       col=c("red","blue", "green", "black", "darkgray"),cex=1, lwd=3,
       box.lty=0)
dev.off()

plot(p_grid , posterior_FEL_B_int_G, type="l")

plot(p_grid , posterior_FEL_B_int_B, type="l")

plot(p_grid , posterior_FEL_B_dis_G, type="l")

plot(p_grid , posterior_FEL_B_dis_B, type="l")


##Plotting priors
# png("Priors.png", width = 200, height = 150, units='mm',  res = 300)
# plot(p_grid, priorL,type='l', col = "black", ylab=("Prior distributions"),
#      xlab=("Utility"),lwd =3, cex.axis=1, cex.lab=1.4)
# 
# dev.off()
# 
# #plotting signals
# png("Signals_FEL.png", width = 200, height = 150, units='mm',  res = 300)
# plot(p_grid , FEL_int_signal_G , type="l",col="black", ylab=("Signals - FEL"),
#      xlab=("Utility"), ylim=c(0,10),lwd =3, cex.axis=1, cex.lab=1.4)
# lines(p_grid , FEL_int_signal_B ,col="green",lwd =3)
# lines(p_grid , FEL_dis_signal_G,col="blue",lwd =3)
# lines(p_grid , FEL_dis_signal_B,col="red",lwd =3)
# legend(0.30, 10.5, legend=c("Par. Good \nTN(0.8,0.6,0,1)", "Par. Bad \nTN(0.2,0.6,0,1)", "Non-par. Good \nTN(0.8,0.05,0,1)",
#                          "Non-par. Bad \nTN(0.2,0.05,0,1)"),
#        col=c("black", "green","blue", "red"),lty=c(1,1,1,1), cex=1,lwd =3,y.intersp=1.5,
#        bty="n")
# dev.off()
# 
# png("Signals_FEI.png", width = 200, height = 150, units='mm',  res = 300)
# plot(p_grid , FEI_int_signal_G , type="l",col="black", ylab=("Signals - FEI"),
#      xlab=("Utility"), ylim=c(0,10),lwd =3, cex.axis=1, cex.lab=1.4)
# lines(p_grid , FEI_int_signal_B ,col="green",lwd =3)
# lines(p_grid , FEI_dis_signal_G,col="blue",lwd =3)
# lines(p_grid , FEI_dis_signal_B,col="red",lwd =3)
# legend(0.30, 10.5, legend=c("Par. Good \nTN(0.8,0.05,0,1)", "Par. Bad \nTN(0.2,0.05,0,1)",
#                            "Non-par. Good \nTN(0.8,0.6,0,1)","Non-par. Bad \nTN(0.2,0.6,0,1)"),
#        col=c("black", "green","blue", "red"),lty=c(1,1,1,1), cex=1,lwd =3,y.intersp=1.5,
#        bty="n")
# dev.off()

