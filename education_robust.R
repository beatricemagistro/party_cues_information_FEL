### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Run all models with education 


## run multiple_imp, multiple_imp_no_matching, and multiple_imp_matching first


### education instead of literacy


z.out <- zelig(policy ~ treatment*edbin + female +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)
# extract model
mod20 <- (extract.Zelig(z.out))

# run simulations on scenarios
x.out1 <- setx(z.out, treatment=0:2, edbin=0)
x.out2 <- setx(z.out, treatment=0:2, edbin=1)

s.out1 <- Zelig::sim(z.out, x = x.out1, x1 = x.out2)
summary(s.out1)

lit1t0 <- setx(z.out, edbin = 1, treatment=0)
lit1t1 <- setx(z.out, edbin = 1, treatment=1)
lit1t2 <- setx(z.out, edbin = 1, treatment=2)
lit0t0 <- setx(z.out, edbin = 0, treatment=0)
lit0t1 <- setx(z.out, edbin = 0, treatment=1)
lit0t2 <- setx(z.out, edbin = 0, treatment=2)

s.out2 <- Zelig::sim(z.out, x = lit0t0, x1 = lit0t1)
summary(s.out2) # significant
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



######  fd between high ed and low ed

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


traceFDeduc <- ropeladder(x=fd_results2$fd,
                      lower=fd_results2$lwb,
                      upper=fd_results2$upb,
                      labels=c("Control",
                               "Party Cue",
                               "CB Exercise"),
                      size=0.65,
                      sublabels="Education",
                      sublabelsyoffset=0.06,
                      entryheight=0.30,
                      subentryheight=1,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

FDsigMark1Educ <- fd_results2$fd
is.na(FDsigMark1Educ) <- (fd_results2$upb<0)
FDtraceSig1Educ <- ropeladder(x=FDsigMark1Educ,
                          col="white",
                          group=2,
                          plot=1)


### cost benefit with education instead of lit

z.out <- zelig(cb_correct ~ female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod21 <- (extract.Zelig(z.out))

x.out <- setx(z.out, edbin=c(0,1))

s.out1 <- Zelig::sim(z.out, x = x.out)
summary(s.out1)

lit1 <- setx(z.out, edbin = 1)
lit0 <- setx(z.out, edbin = 0)


s.out2 <- Zelig::sim(z.out, x = lit0, x1 = lit1)
summary(s.out2)
fd1 <- s.out2$get_qi(qi="fd", xvalue = "x1")

### plot

###EV treatment literacy

# results <- data.frame(
#   literacy = c(0,1),
#   cb_correct = c(1,1),
#   pe.C = NA,
#   lwb.C = NA,
#   upb.C = NA
# )
# 
# ev1 <- setx(z.out, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
# ev2 <- setx(z.out, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
# 
# varnames <- list(ev1,ev2)
# 
# for (i in 1:2) {
#   results$pe.C[i] <- mean(varnames[[i]][,1])
#   results$lwb.C[i] <- quantile(varnames[[i]][,1], probs = 0.025)
#   results$upb.C[i] <- quantile(varnames[[i]][,1], probs = 0.975)
# }
# 
# results
# 
# # Make a new ropeladder plot, showing just change in probability of any agreement
# ##
# 
# 
# traceEV1 <- ropeladder(x=results$pe.C,
#                        lower=results$lwb.C,
#                        upper=results$upb.C,
#                        labels=c("Low literacy",
#                                 "High literacy"),
#                        sublabels="CB correct",
#                        col=cols[6],
#                        entryheight=0.30,
#                        subentryheight=1,
#                        size=0.65,
#                        lex=1.75,
#                        lineend="square",
#                        plot=1
# )



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


traceFD1educ <- ropeladder(x=fd_results$fd,
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


z.out <- zelig(cb_negative ~ female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod22 <-(extract.Zelig(z.out))

x.out <- setx(z.out, edbin=c(0,1))

s.out1 <- Zelig::sim(z.out, x = x.out)
summary(s.out1)

lit1 <- setx(z.out, edbin = 1)
lit0 <- setx(z.out, edbin = 0)


s.out2 <- Zelig::sim(z.out, x = lit0, x1 = lit1)
summary(s.out2)
fd2 <- s.out2$get_qi(qi="fd", xvalue = "x1")

### plot

###EV treatment literacy

# results <- data.frame(
#   literacy = c(0,1),
#   cb_negative = c(1,1),
#   pe.C = NA,
#   lwb.C = NA,
#   upb.C = NA
# )
# 
# ev1 <- setx(z.out, lit=0) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
# ev2 <- setx(z.out, lit=1) %>% Zelig::sim() %>% get_qi(qi = "ev", xvalue = "x")
# 
# varnames <- list(ev1,ev2)
# 
# for (i in 1:2) {
#   results$pe.C[i] <- mean(varnames[[i]][,1])
#   results$lwb.C[i] <- quantile(varnames[[i]][,1], probs = 0.025)
#   results$upb.C[i] <- quantile(varnames[[i]][,1], probs = 0.975)
# }
# 
# results
# 
# # Make a new ropeladder plot, showing just change in probability of any agreement
# ##
# 
# 
# traceEV2 <- ropeladder(x=results$pe.C,
#                        lower=results$lwb.C,
#                        upper=results$upb.C,
#                        labels=c("Low literacy",
#                                 "High literacy"),
#                        sublabels="CB correct direction",
#                        col="black",
#                        sublabelsyoffset=0.04,
#                        entryheight=0.30,
#                        subentryheight=1,
#                        size=0.65,
#                        lex=1.75,
#                        lineend="square",
#                        plot=1
# )

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


traceCBFDeduc <- ropeladder(x=fd_results$fd,
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


### no education

z.out <- zelig(policy ~ treatment*lit + female + 
                  income_group + region_group + age_group + leftright,
                model = "logit", data = miData)

summary(z.out)

mod23 <- (extract.Zelig(z.out))

x.out1 <- setx(z.out, treatment=0:2, lit=0)
x.out2 <- setx(z.out, treatment=0:2, lit=1)

s.out1 <- Zelig::sim(z.out, x = x.out1, x1 = x.out2)
summary(s.out1)

lit1t0 <- setx(z.out, lit = 1, treatment=0)
lit1t1 <- setx(z.out, lit = 1, treatment=1)
lit1t2 <- setx(z.out, lit = 1, treatment=2)
lit0t0 <- setx(z.out, lit = 0, treatment=0)
lit0t1 <- setx(z.out, lit = 0, treatment=1)
lit0t2 <- setx(z.out, lit = 0, treatment=2)

s.out2 <- Zelig::sim(z.out, x = lit0t0, x1 = lit0t1)
summary(s.out2) #significant


s.out3 <- Zelig::sim(z.out, x = lit0t0, x1 = lit0t2)
summary(s.out3) #not sign

s.out4 <- Zelig::sim(z.out, x = lit1t0, x1 = lit1t1)
summary(s.out4) #not significant

s.out5 <- Zelig::sim(z.out, x = lit1t0, x1 = lit1t2)
summary(s.out5)  #significant

s.out6 <- Zelig::sim(z.out, x = lit0t0, x1 = lit1t0)
summary(s.out6) #not significant

s.out7 <- Zelig::sim(z.out, x = lit0t1, x1 = lit1t1)
summary(s.out7) #sign

s.out8 <- Zelig::sim(z.out, x = lit0t2, x1 = lit1t2)
summary(s.out8) # sign

z.out <- zelig(cb_correct ~ female +lit+
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod24 <- (extract.Zelig(z.out))

z.out <- zelig(cb_negative ~  female + lit+
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod25 <- (extract.Zelig(z.out))

## interaction between education and literacy

z.out <- zelig(policy ~ treatment*lit*edbin + female + 
                  income_group + region_group + age_group + leftright,
                model = "logit", data = miData)

summary(z.out)

mod26 <- (extract.Zelig(z.out))

x.out1 <- setx(z.out, treatment=0:2, lit=0)
x.out2 <- setx(z.out, treatment=0:2, lit=1)

s.out1 <- Zelig::sim(z.out, x = x.out1, x1 = x.out2)
summary(s.out1)

lit1t0e1 <- setx(z.out, lit = 1, treatment=0,edbin=1)
lit1t1e1 <- setx(z.out, lit = 1, treatment=1,edbin=1)
lit1t2e1 <- setx(z.out, lit = 1, treatment=2,edbin=1)

lit1t0e0 <- setx(z.out, lit = 1, treatment=0,edbin=0)
lit1t1e0 <- setx(z.out, lit = 1, treatment=1,edbin=0)
lit1t2e0 <- setx(z.out, lit = 1, treatment=2,edbin=0)

lit0t0e1 <- setx(z.out, lit = 0, treatment=0,edbin=1)
lit0t1e1 <- setx(z.out, lit = 0, treatment=1,edbin=1)
lit0t2e1 <- setx(z.out, lit = 0, treatment=2,edbin=1)
lit0t0e0 <- setx(z.out, lit = 0, treatment=0,edbin=0)
lit0t1e0 <- setx(z.out, lit = 0, treatment=1,edbin=0)
lit0t2e0 <- setx(z.out, lit = 0, treatment=2,edbin=0)

s.out1 <- Zelig::sim(z.out, x = lit0t0e0, x1 = lit0t0e1)
summary(s.out1) #not significant
s.out2 <- Zelig::sim(z.out, x = lit0t1e0, x1 = lit0t1e1)
summary(s.out2) # educated illiterates are more likely to favvor price controls after treat1
s.out3 <- Zelig::sim(z.out, x = lit0t2e0, x1 = lit0t2e1)
summary(s.out3) # not significant

s.out4 <- Zelig::sim(z.out, x = lit1t0e0, x1 = lit1t0e1)
summary(s.out4) #not sign
s.out5 <- Zelig::sim(z.out, x = lit1t1e0, x1 = lit1t1e1)
summary(s.out5)#not sign
s.out6 <- Zelig::sim(z.out, x = lit1t2e0, x1 = lit1t2e1)
summary(s.out6) #not sign


z.out <- zelig(cb_correct ~ lit*edbin + female +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod27 <- (extract.Zelig(z.out))

z.out <- zelig(cb_negative ~ lit*edbin + female +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = miData)

summary(z.out)

mod28 <- (extract.Zelig(z.out))

##correlation between education and literacy

library(xtable)

corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

mdata <- dplyr::select(miData$imp1, education,edbin, lit)
tab <- corstarsl(mdata[,1:3])
tab <- slice(tab,3)
xtable(tab) #Latex code