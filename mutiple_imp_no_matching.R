### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Run all models without matching

# Run multiple_imp first.

# Logistic model for price controls

z.out <- zelig(policy ~ treatment*lit + female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)
# extract model
mod3 <- (extract.Zelig(z.out))

# create simulations with zelig for different scenarios
x.out1 <- setx(z.out, treatment=0:2, lit=0)
x.out2 <- setx(z.out, treatment=0:2, lit=1)

s.out1 <- Zelig::sim(z.out, x = x.out1, x1 = x.out2)
summary(s.out1)
# all scenarios
lit1t0 <- setx(z.out, lit = 1, treatment=0)
lit1t1 <- setx(z.out, lit = 1, treatment=1)
lit1t2 <- setx(z.out, lit = 1, treatment=2)
lit0t0 <- setx(z.out, lit = 0, treatment=0)
lit0t1 <- setx(z.out, lit = 0, treatment=1)
lit0t2 <- setx(z.out, lit = 0, treatment=2)

#first scenario
s.out2 <- Zelig::sim(z.out, x = lit0t0, x1 = lit0t1)
summary(s.out2) # significant
# extract all first differences (differences in probability)
fd1 <- s.out2$get_qi(qi="fd", xvalue = "x1")

s.out3 <- Zelig::sim(z.out, x = lit0t0, x1 = lit0t2)
summary(s.out3) #not significant
fd2 <- s.out3$get_qi(qi="fd", xvalue = "x1")

s.out4 <- Zelig::sim(z.out, x = lit1t0, x1 = lit1t1)
summary(s.out4) #not significant
fd3 <- s.out4$get_qi(qi="fd", xvalue = "x1")

s.out5 <- Zelig::sim(z.out, x = lit1t0, x1 = lit1t2)
summary(s.out5)  # significant
fd4 <- s.out5$get_qi(qi="fd", xvalue = "x1")

s.out6 <- Zelig::sim(z.out, x = lit0t0, x1 = lit1t0)
summary(s.out6) #not significant
fd5 <- s.out6$get_qi(qi="fd", xvalue = "x1")

s.out7 <- Zelig::sim(z.out, x = lit0t1, x1 = lit1t1)
summary(s.out7) # significant
fd6 <- s.out7$get_qi(qi="fd", xvalue = "x1")


s.out8 <- Zelig::sim(z.out, x = lit0t2, x1 = lit1t2)
summary(s.out8) #significant
fd7 <- s.out8$get_qi(qi="fd", xvalue = "x1")

#### plots

# When quantities of interest are plotted, such as expected and predicted values and first differences, 
# these are correctly pooled across those from each of the m imputed datasets:

#extract ev (expected values)

ev1 <- setx(z.out, treatment = 0, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev2 <- setx(z.out, treatment = 1, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev3 <- setx(z.out, treatment = 2, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev4 <- setx(z.out, treatment = 0, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev5 <- setx(z.out, treatment = 1, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev6 <- setx(z.out, treatment = 2, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")

###EV treatment literacy

results <- data.frame(
  literacy = c(0, 0, 0, 1, 1, 1),
  treatment = c(0, 1, 2, 0, 1, 2),
  pe.A = NA,
  lwb.A = NA,
  upb.A = NA
)

####
varnames <- list(ev1,ev2,ev3,ev4,ev5,ev6)

for (i in 1:6) {
  results$pe.A[i] <- mean(varnames[[i]][,1])
  results$lwb.A[i] <- quantile(varnames[[i]][,1], probs = 0.025)
  results$upb.A[i] <- quantile(varnames[[i]][,1], probs = 0.975)
}

results

# Create plots of predicted probabilities for three scenarios (final plots will be in plots_EV_FD.R)

scenNames <- c("Control","Party cue", "Cost-benefit exercise")

cols <- brewer.pal(9,"Greys")

##

trace1b <- ropeladder(x = results$pe.A[results$literacy==0],
                      lower = results$lwb.A[results$literacy==0],
                      upper = results$upb.A[results$literacy==0],
                      labels = scenNames,
                      sublabels="Low literacy",
                      sublabelsyoffset=0.04,
                      entryheight=0.30,
                      subentryheight=1,
                      size=0.65,
                      col="black",
                      lex=1.75,
                      lineend="square",
                      plot=1
)

trace2b <- ropeladder(x = results$pe.A[results$literacy==1],
                      lower = results$lwb.A[results$literacy==1],
                      upper = results$upb.A[results$literacy==1],
                      sublabels="High literacy",
                      sublabelsyoffset=0.04,
                      entryheight=0.30,
                      subentryheight=1,
                      col=cols[6],
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

# file <- "plot_EV_policy"
# tile(trace1b, trace2b,
#      limits = c(0.35,0.95),
#      gridlines = list(type="xt"),
#      xaxis=list(at=c(0.4, 0.5, 0.6, 0.7,0.8,0.9)),
#      topaxis=list(add=TRUE, at=c(0.4, 0.5, 0.6,0.7,0.8,0.9)),
#      xaxistitle=list(labels="probability"),
#      topaxistitle=list(labels="probability"),
#      plottitle=list(labels=c("Approve of price controls")),
#      width=list(plot=1),
#      height=list(plottitle=2,xaxistitle=2.5,topaxistitle=2.5),
#      output=list(outfile=file, width=7)
# )


#### First differences

fd <- cbind(fd1,fd2, fd3,fd4,fd5,fd6,fd7)

fd_results <- data.frame(
  literacy = c(0, 0, 1, 1),
  treatment = c(1, 2, 1, 2),
  fd = NA,
  lwb = NA,
  upb = NA
)

varnames <- list(fd1,fd2,fd3,fd4)

for (i in 1:4) {
  fd_results$fd[i] <- mean(varnames[[i]][,1])
  fd_results$lwb[i] <- quantile(varnames[[i]][,1], probs = 0.025)
  fd_results$upb[i] <- quantile(varnames[[i]][,1], probs = 0.975)
}

fd_results

# Make a new ropeladder plot, showing just change in probability 
##

scenNames <- c("Party Cue (Control)","CB Exercise (Control)")

trace1c <- ropeladder(x = fd_results$fd[fd_results$literacy==0],
                      lower = fd_results$lwb[fd_results$literacy==0],
                      upper = fd_results$upb[fd_results$literacy==0],
                      labels = scenNames,
                      sublabels="Low literacy",
                      sublabelsyoffset=0.04,
                      entryheight=0.30,
                      subentryheight=1,
                      size=0.65,
                      col="black",
                      lex=1.75,
                      lineend="square",
                      plot=1
)

trace2c <- ropeladder(x = fd_results$fd[fd_results$literacy==1],
                      lower = fd_results$lwb[fd_results$literacy==1],
                      upper = fd_results$upb[fd_results$literacy==1],
                      sublabels="High literacy",
                      sublabelsyoffset=0.04,
                      entryheight=0.30,
                      subentryheight=1,
                      col=cols[6],
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

sigMark1 <- fd_results$fd[fd_results$literacy==0]
is.na(sigMark1) <- (fd_results$lwb[fd_results$literacy==0]>0)
traceSig1 <- ropeladder(x=sigMark1,
                        col="white",
                        pch=15,
                        group=1,
                        plot=1)

sigMark2 <- fd_results$fd[fd_results$literacy==1]
is.na(sigMark2) <- (fd_results$upb[fd_results$literacy==1]<0)
traceSig2 <- ropeladder(x=sigMark2,
                        col="white",
                        pch=18,
                        group=2,
                        plot=1)

# vertmark <- linesTile(x=c(0,0), y=c(0,1), plot=1)
# 
# xat <- c(-0.30, -0.20,-0.10,0,0.10)
# xlab <- c("-30%","-20%","-10%", "0%", "+10%")

# file <- "plot_FD_policy"
# tile(trace1c, trace2c, vertmark, traceSig1, traceSig2,
#      xaxis=list(at=xat, labels=xlab),
#      topaxis=list(add=TRUE, at=xat, labels=xlab),
#      gridlines=list(type="xt"),
#      xaxistitle=list(labels="difference in probability approve"),
#      topaxistitle=list(labels="difference in probability approve"),
#      plottitle=list(labels="Approve of price controls"),
#      width=list(plot=1),
#      height=list(plottitle=2,xaxistitle=2.5,topaxistitle=2.5),
#      output=list(outfile=file, width=6.75)
# )

######  fd between lit and illits

fd_results2 <- data.frame(
  treatment = c(0,1,2),
  fd = NA,
  lwb = NA,
  upb = NA
)

varnames <- list(fd5,fd6,fd7)

for (i in 1:3) {
  fd_results2$fd[i] <- mean(varnames[[i]][,1])
  fd_results2$lwb[i] <- quantile(varnames[[i]][,1], probs = 0.025)
  fd_results2$upb[i] <- quantile(varnames[[i]][,1], probs = 0.975)
}

fd_results2

# Make a new ropeladder plot, showing just change in probability of any agreement
##


traceFD <- ropeladder(x=fd_results2$fd,
                      lower=fd_results2$lwb,
                      upper=fd_results2$upb,
                      labels=c("Control",
                               "Party Cue",
                               "CB Exercise"),
                      size=0.65,
                      entryheight=0.30,
                      subentryheight=1,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

FDsigMark1 <- fd_results2$fd
is.na(FDsigMark1) <- (fd_results2$upb<0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

# vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)
# 
# xat <- c(-0.25,-0.15,0,0.15, 0.25)
# xlab <- c("-25%","-15%", "0%", "+15%", "+25%")

# Make plot with tile
# file <- "plot_FD_lit"
# tile(traceFD, FDtraceSig1, vertmarkFD,
#      xaxis=list(at=xat, labels=xlab),
#      topaxis=list(add=TRUE, at=xat, labels=xlab),
#      plottitle=list(labels="High literacy compared to low literacy"),
#      xaxistitle=list(labels="difference in probability of approving price controls"),
#      width=list(null=4),
#      height=list(xaxistitle=3, plottitle=4),
#      gridlines=list(type="xt"),
#      output=list(file=file, width=7)   
# )




####cb_analysis logistic regressions. Probability of doing cb analysis correctly.

z.out <- zelig(cb_correct ~ lit + female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)
#extract model
mod1 <- (extract.Zelig(z.out))

x.out <- setx(z.out, lit=c(0,1))
# simulations for different scenarios
s.out1 <- Zelig::sim(z.out, x = x.out)
summary(s.out1)

lit1 <- setx(z.out, lit = 1)
lit0 <- setx(z.out, lit = 0)


s.out2 <- Zelig::sim(z.out, x = lit0, x1 = lit1)
summary(s.out2)
fd1 <- s.out2$get_qi(qi="fd", xvalue = "x1")

### plot

###EV treatment literacy

results <- data.frame(
  literacy = c(0,1),
  cb_correct = c(1,1),
  pe.C = NA,
  lwb.C = NA,
  upb.C = NA
)

ev1 <- setx(z.out, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev2 <- setx(z.out, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")

varnames <- list(ev1,ev2)

for (i in 1:2) {
  results$pe.C[i] <- mean(varnames[[i]][,1])
  results$lwb.C[i] <- quantile(varnames[[i]][,1], probs = 0.025)
  results$upb.C[i] <- quantile(varnames[[i]][,1], probs = 0.975)
}

results

# Make a new ropeladder plot, showing just change in probability of any agreement
##


traceEV1 <- ropeladder(x=results$pe.C,
                       lower=results$lwb.C,
                       upper=results$upb.C,
                       labels=c("Low literacy",
                                "High literacy"),
                       sublabels="CB correct",
                       col=cols[6],
                       entryheight=0.30,
                       subentryheight=1,
                       size=0.65,
                       lex=1.75,
                       lineend="square",
                       plot=1
)



#### First differences

fd_results <- data.frame(
  cb = c(1,2),
  fd = NA,
  lwb = NA,
  upb = NA
)


fd_results$fd[1] <- mean(fd1)
fd_results$lwb[1] <- quantile(fd1, probs = 0.025)
fd_results$upb[1] <- quantile(fd1, probs = 0.975)


# Make a new ropeladder plot, showing just change in probability of any agreement
##


traceFD1 <- ropeladder(x=fd_results$fd,
                       lower=fd_results$lwb,
                       upper=fd_results$upb,
                       labels=c("CB correct"),
                       size=0.65,
                       entryheight=0.30,
                       subentryheight=1,
                       lex=1.75,
                       lineend="square",
                       plot=1
)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.05, 0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
xlab <- c("-5%","0%","+5%","+10%", "+15%","+20%","+25%","+30%" )


####cb_negative


z.out <- zelig(cb_negative ~ lit + female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod2 <-(extract.Zelig(z.out))

x.out <- setx(z.out, lit=c(0,1))

s.out1 <- Zelig::sim(z.out, x = x.out)
summary(s.out1)

lit1 <- setx(z.out, lit = 1)
lit0 <- setx(z.out, lit = 0)


s.out2 <- Zelig::sim(z.out, x = lit0, x1 = lit1)
summary(s.out2)
fd2 <- s.out2$get_qi(qi="fd", xvalue = "x1")

### plot

###EV treatment literacy

results <- data.frame(
  literacy = c(0,1),
  cb_negative = c(1,1),
  pe.C = NA,
  lwb.C = NA,
  upb.C = NA
)

ev1 <- setx(z.out, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
ev2 <- setx(z.out, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")

varnames <- list(ev1,ev2)

for (i in 1:2) {
  results$pe.C[i] <- mean(varnames[[i]][,1])
  results$lwb.C[i] <- quantile(varnames[[i]][,1], probs = 0.025)
  results$upb.C[i] <- quantile(varnames[[i]][,1], probs = 0.975)
}

results

# Make a new ropeladder plot, showing just change in probability of any agreement
##


traceEV2 <- ropeladder(x=results$pe.C,
                       lower=results$lwb.C,
                       upper=results$upb.C,
                       labels=c("Low literacy",
                                "High literacy"),
                       sublabels="CB correct direction",
                       col="black",
                       sublabelsyoffset=0.04,
                       entryheight=0.30,
                       subentryheight=1,
                       size=0.65,
                       lex=1.75,
                       lineend="square",
                       plot=1
)

#total plot cb_correct and negative
# file <- "plot_EV_cb"
# tile(traceEV1,traceEV2,
#      limits = c(0,1),
#      gridlines = list(type="xt"),
#      xaxis=list(at=c(0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)),
#      topaxis=list(add=TRUE, at=c(0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)),
#      xaxistitle=list(labels="probability"),
#      topaxistitle=list(labels="probability"),
#      plottitle=list(labels=c("Answer to cost-benefit exercise")),
#      width=list(null=4),
#      height=list(plottitle=2,xaxistitle=2.5,topaxistitle=2.5),
#      output=list(outfile=file, width=7)
# )


#### First differences


fd_results$fd[2] <- mean(fd2)
fd_results$lwb[2] <- quantile(fd2, probs = 0.025)
fd_results$upb[2] <- quantile(fd2, probs = 0.975)

fd_results
# Make a new ropeladder plot, showing just change in probability of any agreement
##


traceCBFD <- ropeladder(x=fd_results$fd,
                        lower=fd_results$lwb,
                        upper=fd_results$upb,
                        labels=c("CB Correct",
                                 "CB Correct Direction"),
                        size=0.65,
                        lex=1.75,
                        entryheight=0.30,
                        subentryheight=1,
                        lineend="square",
                        plot=1
)

# vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)
# 
# xat <- c(-0.05, 0,0.10,0.20,0.30)
# xlab <- c("-5%","0%", "+10%","+20%", "+30%")

# Make plot with tile
# file <- "plot_FD_cb"
# tile(traceFD, vertmarkFD,
#      xaxis=list(at=xat, labels=xlab),
#      topaxis=list(add=TRUE, at=xat, labels=xlab),
#      plottitle=list(labels="High literacy compared to low literacy"),
#      xaxistitle=list(labels="difference in probability of doing CB exercise"),
#      width=list(null=4),
#      height=list(xaxistitle=3, plottitle=4),
#      gridlines=list(type="xt"),
#      output=list(file=file, width=7)   
# )





