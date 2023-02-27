### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Run all models with matching


## run multiple_imp and multiple_imp_no_matching first


# extension for Zelig objects (Zelig package >= 5.0)
extract.Zelig <- function(model, include.nobs = TRUE, include.nimp = TRUE, ...) {
  if (model$mi) {
    if (!exists("combine_coef_se", where = "package:Zelig",
                mode = "function")) {
      stop("texreg relies on Zelig's combine_coef_se function to extract model information. Install Zelig >= 5.0-17 to see if texreg can format your model.")
    }
    combined <- Zelig::combine_coef_se(model, messages = FALSE)
    gof <- gof.names <- gof.decimal <- NULL
    if (include.nobs) {
      gof <- c(gof, nrow(model$data))
      gof.names <- c(gof.names, "Num. obs.")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    if (include.nimp) {
      if (class(model$originaldata)[1] == "amelia") {
        gof <- c(gof, model$originaldata$m)
        gof.names <- c(gof.names, "Num. imp.")
        gof.decimal <- c(gof.decimal, FALSE)
      } else if (class(model$originaldata)[1] == "mi") { # when imputed dataset was created using to_zelig_mi
        gof <- c(gof, length(model$originaldata))
        gof.names <- c(gof.names, "Num. imp.")
        gof.decimal <- c(gof.decimal, FALSE)
      }
    }
    out <- createTexreg(coef.names = row.names(combined),
                        coef = combined[, "Estimate"],
                        se = combined[, "Std.Error"],
                        pvalues = combined[, "Pr(>|z|)"],
                        gof.names = gof.names,
                        gof = gof,
                        gof.decimal = gof.decimal)
  } else {
    if ("Zelig-relogit" %in% class(model)) { # remove when users update to Zelig 5.0-16
      mod_original <- model$zelig.out$z.out[[1]]
      class(mod_original) <- "glm"
    }
    else if ("Zelig-tobit" %in% class(model)) { # remove when users update to Zelig 5.0-16
      mod_original <- model$zelig.out$z.out[[1]]
    } else {
      if (!exists("from_zelig_model", where = "package:Zelig",
                  mode = "function")) {
        stop("texreg relies on Zelig's from_zelig_model function to extract model information. Install Zelig >= 5.0-16 to see if texreg can format your model.")
      }
      mod_original <- try(Zelig::from_zelig_model(model), silent = TRUE)
      if (class(mod_original)[1] == "try-error") {
        stop("texreg relies on Zelig's from_zelig_model function to extract information from Zelig models. from_zelig_model does not appear to support models of class ",
             class(model)[1], ".")
      }
    }
    out <- extract(mod_original, include.nobs = include.nobs, ...)
  }
  return(out)
}

setMethod("extract", signature = className("Zelig", "Zelig"),
          definition = extract.Zelig)

library(cem)

#omit NAs to do matching
for (i in 1:m) {
  miData[[i]] <- miData[[i]]  %>%
    na.omit()
}


# mat1 <- cem(treatment="lit", datalist=miData, drop=c("eclit1","eclit2","eclit3","finlit1","finlit2","finlit3","finlit","eclit",
#                                            "fel","treatment","policy","certainty","cb","income","education",
#                                           " age","gender","region","cb_correct",
#                                            "region_group2","region_group3","cb_negative"))
# mat1

m.out4 <- vector("list", m)
# do cem matching
for (i in 1:m) {
  m.out4[[i]] <- matchit(lit ~ edbin + income_group + region_group + age_group + 
                           female + leftright, data = miData[[i]],method = "cem")
}
# extract matched datasets
m.out4data1 <- match.data(m.out4[[1]])
m.out4data2 <- match.data(m.out4[[2]])
m.out4data3 <- match.data(m.out4[[3]])
m.out4data4 <- match.data(m.out4[[4]])
m.out4data5 <- match.data(m.out4[[5]])
m.out4data6 <- match.data(m.out4[[6]])
m.out4data7 <- match.data(m.out4[[7]])
m.out4data8 <- match.data(m.out4[[8]])
m.out4data9 <- match.data(m.out4[[9]])
m.out4data10 <- match.data(m.out4[[10]])
m.out4data11 <- match.data(m.out4[[11]])
m.out4data12 <- match.data(m.out4[[12]])
m.out4data13 <- match.data(m.out4[[13]])
m.out4data14 <- match.data(m.out4[[14]])
m.out4data15 <- match.data(m.out4[[15]])
m.out4data16 <- match.data(m.out4[[16]])
m.out4data17 <- match.data(m.out4[[17]])
m.out4data18 <- match.data(m.out4[[18]])
m.out4data19 <- match.data(m.out4[[19]])
m.out4data20 <- match.data(m.out4[[20]])

###zelig doesn't work with datasets of different lenghts. So I randomly remove
#observations to make them all of the same length (same as the smallest one - 1978 m.out4data17)


# m.out4data2 <- subset(m.out4data2, select = -c(age,gender,region))
set.seed(1234)

m.out4data1 <- m.out4data1 %>%
  dplyr::sample_n(1978)
m.out4data2 <- m.out4data2 %>%
  dplyr::sample_n(1978)

m.out4data3 <- m.out4data3 %>%
  dplyr::sample_n(1978)

m.out4data4 <- m.out4data4 %>%
  dplyr::sample_n(1978)

m.out4data5 <- m.out4data5 %>%
  dplyr::sample_n(1978)
m.out4data6 <- m.out4data6 %>%
  dplyr::sample_n(1978)
m.out4data7 <- m.out4data7 %>%
  dplyr::sample_n(1978)
m.out4data8 <- m.out4data8 %>%
  dplyr::sample_n(1978)
m.out4data9 <- m.out4data9 %>%
  dplyr::sample_n(1978)
m.out4data10 <- m.out4data10 %>%
  dplyr::sample_n(1978)
m.out4data11 <- m.out4data11 %>%
  dplyr::sample_n(1978)
m.out4data12 <- m.out4data12 %>%
  dplyr::sample_n(1978)
m.out4data13 <- m.out4data13 %>%
  dplyr::sample_n(1978)
m.out4data14 <- m.out4data14 %>%
  dplyr::sample_n(1978)
m.out4data15 <- m.out4data15 %>%
  dplyr::sample_n(1978)
m.out4data16 <- m.out4data16 %>%
  dplyr::sample_n(1978)
m.out4data18 <- m.out4data18 %>%
  dplyr::sample_n(1978)
m.out4data19 <- m.out4data19 %>%
  dplyr::sample_n(1978)
m.out4data20 <- m.out4data20 %>%
  dplyr::sample_n(1978)

# Bundle Multiply Imputed Data Sets into an Object for Zelig
mi.out <- to_zelig_mi(m.out4data1, m.out4data2,m.out4data3,m.out4data4,m.out4data5,m.out4data6,
                      m.out4data7,m.out4data8,m.out4data9,m.out4data10,m.out4data11,m.out4data12,
                      m.out4data13,m.out4data14,m.out4data15,m.out4data16,m.out4data17,
                      m.out4data18,m.out4data19,m.out4data20)

# run model on multiply imputed matched datasets 
z.out <- zelig(policy ~ treatment*lit + female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = mi.out)

summary(z.out)
#extract model
mod4 <- (extract.Zelig(z.out))
# create scenarios for simulations
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


#### plots

# When quantities of interest are plotted, such as expected and predicted values and first differenences, 
# these are correctly pooled across those from each of the m imputed datasets:

###EV treatment literacy

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

# Plot predicted probabilities for (all quantities of interest from all models will be plotted in plots_EV_FD.R)

scenNames <- c("Control","Party cue", "Cost-benefit exercise")

cols <- brewer.pal(9,"Greys")

##

trace1bCEM <- ropeladder(x = results$pe.A[results$literacy==0],
                         lower = results$lwb.A[results$literacy==0],
                         upper = results$upb.A[results$literacy==0],
                         labels = scenNames,
                         sublabels="Low literacy",
                         sublabelsyoffset=0.04,
                         entryheight=0.30,
                         subentryheight=1,
                         pch=17,
                         size=0.65,
                         col="black",
                         lex=1.75,
                         lineend="square",
                         plot=1
)

trace2bCEM <- ropeladder(x = results$pe.A[results$literacy==1],
                         lower = results$lwb.A[results$literacy==1],
                         upper = results$upb.A[results$literacy==1],
                         sublabels="High literacy",
                         sublabelsyoffset=0.04,
                         entryheight=0.30,
                         subentryheight=1,
                         pch=17,
                         col=cols[6],
                         size=0.65,
                         lex=1.75,
                         lineend="square",
                         plot=1
)

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

# Make a new ropeladder plot, showing just change in probability of any agreement
##
scenNames <- c("Party Cue (Control)","CB Exercise (Control)")



trace1cCEM <- ropeladder(x = fd_results$fd[fd_results$literacy==0],
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
                         pch=17,
                         lineend="square",
                         plot=1
)

trace2cCEM <- ropeladder(x = fd_results$fd[fd_results$literacy==1],
                         lower = fd_results$lwb[fd_results$literacy==1],
                         upper = fd_results$upb[fd_results$literacy==1],
                         sublabels="High literacy",
                         sublabelsyoffset=0.04,
                         entryheight=0.30,
                         subentryheight=1,
                         col=cols[6],
                         size=0.65,
                         lex=1.75,
                         pch=17,
                         lineend="square",
                         plot=1
)

sigMark1CEM <- fd_results$fd[fd_results$literacy==0]
is.na(sigMark1CEM) <- (fd_results$lwb[fd_results$literacy==0]>0)
traceSig1CEM <- ropeladder(x=sigMark1CEM,
                           col="white",
                           group=3,
                           plot=1)

sigMark2CEM <- fd_results$fd[fd_results$literacy==1]
is.na(sigMark2CEM) <- (fd_results$upb[fd_results$literacy==1]<0)
traceSig2CEM <- ropeladder(x=sigMark2CEM,
                           col="white",
                           group=4,
                           plot=1)

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


traceFDCEM <- ropeladder(x=fd_results2$fd,
                         lower=fd_results2$lwb,
                         upper=fd_results2$upb,
                         labels=c("Control",
                                  "Party Cue",
                                  "CB Exercise"),
                         entryheight=0.30,
                         subentryheight=1,
                         size=0.65,
                         pch=17,
                         lex=1.75,
                         lineend="square",
                         plot=1
)

FDsigMark1CEM <- fd_results2$fd
is.na(FDsigMark1CEM) <- (fd_results2$upb<0)
FDtraceSig1CEM <- ropeladder(x=FDsigMark1CEM,
                             col="white",
                             group=2,
                             plot=1)

####CB analysis


z.out <- zelig(cb_correct ~ lit + female + edbin +
                 income_group + region_group + age_group + leftright,
               model = "logit", data = mi.out)

summary(z.out)

mod5 <- (extract.Zelig(z.out))

x.out <- setx(z.out, lit=c(0,1))

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


traceEV1CEM <- ropeladder(x=results$pe.C,
                          lower=results$lwb.C,
                          upper=results$upb.C,
                          labels=c("Low literacy",
                                   "High literacy"),
                          sublabels="CB correct",
                          col="grey",
                          entryheight=0.30,
                          subentryheight=1,
                          size=0.65,
                          lex=1.75,
                          pch=17,
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


traceFD1CEM <- ropeladder(x=fd_results$fd,
                          lower=fd_results$lwb,
                          upper=fd_results$upb,
                          labels=c("CB correct"),
                          size=0.65,
                          entryheight=0.30,
                          subentryheight=1,
                          pch=17,
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
               model = "logit", data = mi.out)

summary(z.out)

mod6 <- (extract.Zelig(z.out))

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


traceEV2CEM <- ropeladder(x=results$pe.C,
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
                          pch=17,
                          lineend="square",
                          plot=1
)

## FD


fd_results$fd[2] <- mean(fd2)
fd_results$lwb[2] <- quantile(fd2, probs = 0.025)
fd_results$upb[2] <- quantile(fd2, probs = 0.975)

fd_results
# Make a new ropeladder plot, showing just change in probability of any agreement
##


traceCBFDCEM <- ropeladder(x=fd_results$fd,
                           lower=fd_results$lwb,
                           upper=fd_results$upb,
                           labels=c("CB Correct",
                                    "CB Correct Direction"),
                           size=0.65,
                           entryheight=0.30,
                           subentryheight=1,
                           lex=1.75,
                           pch=17,
                           lineend="square",
                           plot=1
)


####
