### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Cleans the original datasets
### 2. Does Multiple Imputation
### 3. Creates all variables that will be used in analyses.

library(scales)
library(stringr)
library(xml2)
library(devtools)
library(labelled)
library(Hmisc)
library("rio")
library("ggplot2")
library("plyr")
library("dplyr")
library("broom")
library("haven")
library("readr")
library("sjmisc")
library("car")
library(tidyr)
library(simcf)
library(tile)
library(MASS)
library(nnet)
library(Amelia)
library(psych)
library(MNP)
library(gtools)
library(reshape2)
library(RColorBrewer)
library(mlogit)
library(clusterSEs)
library(abind)
library(Zelig)
library(zeligverse)
library(texreg)
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

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

set.seed(1234)

## The following lines of code clean the datasets and do multiple imputation. This is optional to run, as the multiply imputed
## datasets are already saved under imputations_it2.RData

# load data with political (party cue) treatment and control group
# newdata <- read.csv("survey_experiment_final4.csv",
#                     header=TRUE, na.strings=c("","NA"),
#                     colClasses=c(rep('factor',15), 'numeric',rep('factor',11),'numeric',
#                                  rep('factor',3)))
# 
# 
# 
# #recode financial literacy (finlit)
# levels(newdata$finlit1) <- c('1', '2', '3', '4')
# levels(newdata$finlit2) <- c('1', '2', '3', '4')
# levels(newdata$finlit3) <- c('1', '2', '3')
# 
# #recode economic literacy (eclit)
# levels(newdata$eclit1) <- c('1', '2', '3', '4')
# levels(newdata$eclit2) <- c('1', '2', '3', '4')
# levels(newdata$eclit3) <- c('1', '2', '3', '4')
# 
# 
# #recode finlit so that it takes value 1 for correct answers and 0 otherwise
# newdata$finlit1 <- car::recode(newdata$finlit1, "1:3 = 0; 4 = 1")
# 
# newdata$finlit2 <- car::recode(newdata$finlit2, "1 = 0; 2 = 1; 3 = 0; 4 = 0")
# 
# newdata$finlit3 <- car::recode(newdata$finlit3, "1 = 1; 2:3 = 0")
# 
# 
# #create finlit measure from 0 correct answers to 3.
# newdata <- mutate(newdata,finlit = ifelse(
#   (finlit1==0 & finlit2==0 & finlit3==0)
#   , 0,
#   ifelse((finlit1==0 & finlit2==0 & finlit3==1)|
#            (finlit1==0 & finlit2==1 & finlit3==0)|
#            (finlit1==1 & finlit2==0 & finlit3==0), 1,
#          ifelse((finlit1==0 & finlit2==1 & finlit3==1)|
#                   (finlit1==1 & finlit2==1 & finlit3==0)|
#                   (finlit1==1 & finlit2==0 & finlit3==1), 2,
#                 3))))
# 
# #recode eclit so that it takes value 1 for correct answers and 0 otherwise
# 
# newdata$eclit1 <- car::recode(newdata$eclit1, "1 = 0; 2 = 0; 3 = 1; 4 = 0")
# 
# newdata$eclit2 <- car::recode(newdata$eclit2, "1 = 1; 2:4 = 0")
# 
# newdata$eclit3 <- car::recode(newdata$eclit3, "1 = 0; 2 = 0; 3 = 1; 4 = 0")
# 
# 
# #create eclit measure from 0 correct answers to 3.
# newdata <- mutate(newdata,eclit = ifelse(
#   (eclit1==0 & eclit2==0 & eclit3==0)
#   , 0,
#   ifelse((eclit1==0 & eclit2==0 & eclit3==1)|
#            (eclit1==0 & eclit2==1 & eclit3==0)|
#            (eclit1==1 & eclit2==0 & eclit3==0), 1,
#          ifelse((eclit1==0 & eclit2==1 & eclit3==1)|
#                   (eclit1==1 & eclit2==1 & eclit3==0)|
#                   (eclit1==1 & eclit2==0 & eclit3==1), 2,
#                 3))))
# 
# create financial and economic literacy index from 0 to 6 correct answers
# newdata <- mutate(newdata,fel = ifelse(
#   (eclit1==0 & eclit2==0 & eclit3==1)|
#     (eclit1==0 & eclit2==1 & eclit3==0)|
#     (eclit1==1 & eclit2==0 & eclit3==0), finlit+1,
#   ifelse((eclit1==0 & eclit2==1 & eclit3==1)|
#            (eclit1==1 & eclit2==1 & eclit3==0)|
#            (eclit1==1 & eclit2==0 & eclit3==1), finlit+2,
#          ifelse((eclit1==1 & eclit2==1 & eclit3==1),
#                 finlit+3, finlit*1))))
# 
# create literacy (lit) variable where we only keep literate (mean literacy + 1 sd, hence 5/6 or 6/6 correct) and illiterate
# individuals (mean literacy - 1 sd, hence 0/6 and 1/6 correct answers)
# newdata <- mutate(newdata,lit = ifelse((fel==0) | (fel==1), 0, ifelse((fel==5) |
#                                                                         (fel==6), 1, NA)))
# 
# #recode price controls
# levels(newdata$control) <- c('0', '1')
# do you favor price controls? #0 no 1 yes
# levels(newdata$treat) <- c('0', '1')
# # do you favor price controls?  0 no 1 yes
# 
# #recode cb_c -- cost-benefit exercise for control and treatment group
# levels(newdata$cb_c) <- c('1', '2', '3', '4', "5")
# 
# levels(newdata$cb_t) <- c('1', '2', '3', '4', "5")
# 
# #recode cb_correct so that it takes value 1 for people who did it correctly and 0 otherwise
# 
# newdata$cb_correct_c <- car::recode(newdata$cb_c, "1 = 1; 2 = 0; 3 = 0; 4 = 0; 5=0")
# 
# newdata$cb_correct_t <- car::recode(newdata$cb_t, "1 = 1; 2 = 0; 3 = 0; 4 = 0; 5=0")
# 
# 
# create tretment variable that takes value 0 for control group and 1 for party cue treatment
# newdata <- mutate(newdata,treatment =
#                     case_when(
#                       control == 0 | control == 1 ~ 0,
#                       treat == 0 | treat == 1        ~ 1
#                     )
# )
# 
# reshape in long format
# newdata_long <- reshape(newdata,direction="long",
#                         varying=list(c("control","treat"), c("certainty_c","certainty_t"), c("cb_c","cb_t"),
#                                      c("workstatus_c","workstatus_t"), c("occupation_c","occupation_t"),
#                                      c("sector_c","sector_t"), c("income_c","income_t"),
#                                      c("education_c","education_t"), c("leftright_c","leftright_t"),
#                                      c("age_c","age_t"), c("gender_c","gender_t"), c("region_c","region_t"),
#                                      c("cb_correct_c","cb_correct_t")),
#                         v.names=c("policy","certainty","cb","workstatus","occupation","sector","income","education",
#                                   "leftright", "age", "gender", "region", "cb_correct"))
# 
# table(newdata_long$treatment)
# 
# newdata_long <- newdata_long %>%
#   drop_na(lit) %>%
#   arrange(treatment)
# 
# d <- subset(newdata_long, (treatment==0 & time==1) | (treatment==1 & time==2))
# 
# table(d$treatment)
# 
# d <-  subset(d, select = -c(time, id))
# 
# load dataset with cost benefit treatment 
# newdata2 <- read.csv("CB_survey_final.csv",
#                      header=TRUE, na.strings=c("","NA"),
#                      colClasses=c(rep('factor',15), 'numeric','factor','factor'))
# 
# #recode finlit
# levels(newdata2$finlit1) <- c('1', '2', '3', '4')
# levels(newdata2$finlit2) <- c('1', '2', '3', '4')
# levels(newdata2$finlit3) <- c('1', '2', '3')
# 
# #recode eclit
# levels(newdata2$eclit1) <- c('1', '2', '3', '4')
# levels(newdata2$eclit2) <- c('1', '2', '3', '4')
# levels(newdata2$eclit3) <- c('1', '2', '3', '4')
# 
# 
# #recode finlit
# newdata2$finlit1 <- car::recode(newdata2$finlit1, "1:3 = 0; 4 = 1")
# 
# newdata2$finlit2 <- car::recode(newdata2$finlit2, "1 = 0; 2 = 1; 3 = 0; 4 = 0")
# 
# newdata2$finlit3 <- car::recode(newdata2$finlit3, "1 = 1; 2:3 = 0")
# 
# 
# #create finlit measure
# newdata2 <- mutate(newdata2,finlit = ifelse(
#   (finlit1==0 & finlit2==0 & finlit3==0)
#   , 0,
#   ifelse((finlit1==0 & finlit2==0 & finlit3==1)|
#            (finlit1==0 & finlit2==1 & finlit3==0)|
#            (finlit1==1 & finlit2==0 & finlit3==0), 1,
#          ifelse((finlit1==0 & finlit2==1 & finlit3==1)|
#                   (finlit1==1 & finlit2==1 & finlit3==0)|
#                   (finlit1==1 & finlit2==0 & finlit3==1), 2,
#                 3))))
# 
# #recode eclit
# 
# newdata2$eclit1 <- car::recode(newdata2$eclit1, "1 = 0; 2 = 0; 3 = 1; 4 = 0")
# 
# newdata2$eclit2 <- car::recode(newdata2$eclit2, "1 = 1; 2:4 = 0")
# 
# newdata2$eclit3 <- car::recode(newdata2$eclit3, "1 = 0; 2 = 0; 3 = 1; 4 = 0")
# 
# 
# #create eclit measure
# newdata2 <- mutate(newdata2,eclit = ifelse(
#   (eclit1==0 & eclit2==0 & eclit3==0)
#   , 0,
#   ifelse((eclit1==0 & eclit2==0 & eclit3==1)|
#            (eclit1==0 & eclit2==1 & eclit3==0)|
#            (eclit1==1 & eclit2==0 & eclit3==0), 1,
#          ifelse((eclit1==0 & eclit2==1 & eclit3==1)|
#                   (eclit1==1 & eclit2==1 & eclit3==0)|
#                   (eclit1==1 & eclit2==0 & eclit3==1), 2,
#                 3))))
# create financial and economic literacy index
# newdata2 <- mutate(newdata2,fel = ifelse(
#   (eclit1==0 & eclit2==0 & eclit3==1)|
#     (eclit1==0 & eclit2==1 & eclit3==0)|
#     (eclit1==1 & eclit2==0 & eclit3==0), finlit+1,
#   ifelse((eclit1==0 & eclit2==1 & eclit3==1)|
#            (eclit1==1 & eclit2==1 & eclit3==0)|
#            (eclit1==1 & eclit2==0 & eclit3==1), finlit+2,
#          ifelse((eclit1==1 & eclit2==1 & eclit3==1),
#                 finlit+3, finlit*1))))
# 
#create literacy variable 
# newdata2 <- mutate(newdata2,lit = ifelse((fel==0) | (fel==1), 0, ifelse((fel==5) |
#                                                                           (fel==6), 1, NA)))
# 
# #recode cb -- cost benefit exercise 
# 
# levels(newdata2$cb) <- c('1', '2', '3', '4', "5")
# 
# newdata2$cb_correct <- car::recode(newdata2$cb, "1 = 1; 2 = 0; 3 = 0; 4 = 0; 5=0")
# 
# 
# newdata2 <- mutate(newdata2,treatment = 2)
# 
# newdata2 <- newdata2%>%
#   drop_na(lit)
# 
# #recode policy (favors price controls 0 no, 1 yes)
# levels(newdata2$policy) <- c('0', '1')
# 
# table(newdata2$lit)
# 
# ### merge two datasets
# 
# total <- rbind(d, newdata2)
# 
# d <- total
# 
# ### merge datasets
# 
# d <- subset(total, select = -c(occupation, CintID,cb_correct, sector, workstatus))
# 
# ###check age
# 
# d$age <- ifelse(d$age %in% c(100),
#                             NA, d$age)
# # check income
# #recode and reorder income
# levels(d$income) <- c('1', '10', '4', '5', '6','2','7','8','9','3')
# d$income <- factor(d$income, levels = c('1', '2', '3', '4', '5',
#                                                               '6','7','8','9','10'))
# #as integer
# d$income <- as.integer(d$income)
# 
# #political ideology recode and reorder
# levels(d$leftright) <- c('0', '1', '10', '2', '3','4', '5', '6', '7', '8', '9')
# 
# d$leftright <- factor(d$leftright, levels = c('0', '1', '2', '3', '4', '5',
#                                                                     '6','7','8','9','10'))
# d$leftright <- as.integer(d$leftright)
# 
# #certainty recode and reorder
# 
# levels(d$certainty) <- c('1', '10', '2', '3','4', '5', '6', '7', '8', '9')
# 
# d$certainty <- factor(d$certainty, levels = c('1', '2', '3', '4', '5',
#                                                                     '6','7','8','9','10'))
# 
# d$certainty <- as.integer(d$certainty)
# 
# 
# #impute NAs
# bds <- matrix(c(18, 18, 74), nrow = 1, ncol = 3)
# m <- 20 # number of imputations
# amelia_output_it <- Amelia::amelia(d, m=m,
#                         ords = c("income","leftright","certainty",
#                                  "education"),
#                         noms = c("policy","cb",
#                                  "gender","region"),
#                         idvars=c("eclit1","eclit2","eclit3","finlit1","finlit2",
#                                  "finlit3","finlit", "eclit", "fel" ,"lit","treatment"
#                                                     ),
#                         bounds = rbind(c(18, 18, 74)))
#                         


#save(amelia_output_it, file = "imputations_it2.RData")

# load imputations
load("imputations_it2.RData")

miData <-amelia_output_it$imputations
m <- 20

#recode cb -- cost benefit exercise correct (1 or 0)
for (i in 1:m) {
  miData[[i]]$cb_correct <- car::recode(miData[[i]]$cb, "1 = 1; 2 = 0; 3 = 0; 4 = 0; 5=0")
}



#recode and reorder education

for (i in 1:m) {
  levels(miData[[i]]$education) <- c('4', '2', '5', '3', '1','6')
}
for (i in 1:m) {
  miData[[i]]$education <- factor(miData[[i]]$education, levels = c('1', '2', '3', '4', '5',
                                                                    '6'))
}

#1 no title, 2 elementary, 3 middle school, 4 high school, 5 undergraduate degree, 6 post

for (i in 1:m) {
  miData[[i]]$female <- miData[[i]]$gender
}

for (i in 1:m) {
  levels(miData[[i]]$female) <- c('1', '0')
}

for (i in 1:m) {
  miData[[i]]$female <- factor(miData[[i]]$female, levels = c('0', '1'))
}

#create income group

for (i in 1:m) {
  miData[[i]]$income_group <- car::recode(miData[[i]]$income, "1:3 = 0; 4:6 = 1; 7:10=2")
}

#create binary education variable


for (i in 1:m) {
  miData[[i]]$edbin <- miData[[i]]$education
}

for (i in 1:m) {
  miData[[i]]$edbin <- car::recode(miData[[i]]$edbin, "1:4 = 0; 5:6 = 1")
}

#more than one measure of education for robustness checks

for (i in 1:m) {
  miData[[i]]$hs <- miData[[i]]$education
}

for (i in 1:m) {
  miData[[i]]$hs <- car::recode(miData[[i]]$hs, "1:3 = 0; 5:6 = 0;4=1")
}

for (i in 1:m) {
  miData[[i]]$edcat <- miData[[i]]$education
}

for (i in 1:m) {
  miData[[i]]$edcat <- car::recode(miData[[i]]$edcat, "1:3 = 0; 5:6 = 2;4=1")
}

for (i in 1:m) {
  miData[[i]]$edcat <- as.factor(miData[[i]]$edcat)
}

for (i in 1:m) {
  miData[[i]]$education <- as.numeric(as.character(miData[[i]]$education))
}
#region

#create region group

# north is the region with the least production of oil, followed by center with middle and south with most

for (i in 1:m) {
  miData[[i]] <- mutate(miData[[i]], region_group = ifelse(
    (region=="Liguria" | region=="Lombardia" | region=="Piemonte"|
       region=="Valle d'Aosta"|region=="Emilia-Romagna" |region=="Veneto" |
       region=="Friuli-Venezia Giulia" | region=="Trentino Alto Adige")
    , 0, 
    ifelse((region=="Lazio" | region=="Marche" | region=="Toscana"|
              region=="Umbria"), 1,
           2)))
}

# liguria center or north? its production is more aligned with center 

for (i in 1:m) {
  miData[[i]] <- mutate(miData[[i]], region_group2 = ifelse(
    (region=="Lombardia" | region=="Piemonte"|
       region=="Valle d'Aosta"|region=="Emilia-Romagna" |region=="Veneto" |
       region=="Friuli-Venezia Giulia" | region=="Trentino Alto Adige")
    , 0, 
    ifelse((region=="Liguria" | region=="Lazio" | region=="Marche" | region=="Toscana"|
              region=="Umbria"), 1,
           2)))
}


# north is the region with the least production of oil, followed by center with middle and south with most
#puglia produces way more than anyone else, put it alone


for (i in 1:m) {
  miData[[i]] <- mutate(miData[[i]], region_group3 = ifelse(
    (region=="Liguria" | region=="Lombardia" | region=="Piemonte"|
       region=="Valle d'Aosta"|region=="Emilia-Romagna" |region=="Veneto" |
       region=="Friuli-Venezia Giulia" | region=="Trentino Alto Adige")
    , 0, 
    ifelse((region=="Lazio" | region=="Marche" | region=="Toscana"|
              region=="Umbria"), 1, 
           ifelse((region=="Abruzzo" | region=="Basilicata" | region=="Calabria"|
                     region=="Campania" | region=="Molise"| region=="Sicilia"|
                     region=="Sardegna"),
                  2,3))))
}
##age group

#recode age_group


summary(miData[[1]]$age)

for (i in 1:m) {
  miData[[i]] <- mutate(miData[[i]], age_group = ifelse((age<=31),0,
                                                        ifelse((age>31 & age<=51),1,2)))
}



for (i in 1:m) {
  miData[[i]]$treatment <- as.factor(miData[[i]]$treatment)
}

for (i in 1:m) {
  miData[[i]]$income_group <- as.factor(miData[[i]]$income_group)
}

for (i in 1:m) {
  miData[[i]]$region_group <- as.factor(miData[[i]]$region_group)
}

for (i in 1:m) {
  miData[[i]]$age_group <- as.factor(miData[[i]]$age_group)
}

for (i in 1:m) {
  miData[[i]]$lit <- as.factor(miData[[i]]$lit)
}

# create dummy for those who understand that effect of policy is negative
for (i in 1:m) {
  miData[[i]]$cb_negative <- car::recode(miData[[i]]$cb, "1 = 1; 2 = 1; 3 = 1; 4 = 0; 5=0")
}


