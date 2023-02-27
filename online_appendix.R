### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Create tables in online appendix


## run multiple_imp, multiple_imp_no_matching, multiple_imp matching, and education_robust first


library(texreg)

##summary tables in the online appendix
prop.table(table(miData$imp1$treatment, miData$imp1$edbin),1)
prop.table(table(miData$imp1$treatment, miData$imp1$income_group),1)
prop.table(table(miData$imp1$treatment, miData$imp1$region_group),1)
prop.table(table(miData$imp1$treatment, miData$imp1$female),1)
prop.table(table(miData$imp1$treatment, miData$imp1$age_group),1)
mean(miData$imp1$age[miData$imp1$treatment==0], na.rm=T)
mean(miData$imp1$age[miData$imp1$treatment==1], na.rm=T)
mean(miData$imp1$age[miData$imp1$treatment==2], na.rm=T)
sd(miData$imp1$age[miData$imp1$treatment==0], na.rm=T)
sd(miData$imp1$age[miData$imp1$treatment==1], na.rm=T)
sd(miData$imp1$age[miData$imp1$treatment==2], na.rm=T)
mean(miData$imp1$leftright[miData$imp1$treatment==0], na.rm=T)
mean(miData$imp1$leftright[miData$imp1$treatment==1], na.rm=T)
mean(miData$imp1$leftright[miData$imp1$treatment==2], na.rm=T)
sd(miData$imp1$leftright[miData$imp1$treatment==0], na.rm=T)
sd(miData$imp1$leftright[miData$imp1$treatment==1], na.rm=T)
sd(miData$imp1$leftright[miData$imp1$treatment==2], na.rm=T)

prop.table(table(miData$imp1$lit, miData$imp1$edbin),1)
prop.table(table(miData$imp1$lit, miData$imp1$income_group),1)
prop.table(table(miData$imp1$lit, miData$imp1$region_group),1)
prop.table(table(miData$imp1$lit, miData$imp1$age_group),1)
prop.table(table(miData$imp1$lit, miData$imp1$female),1)
mean(miData$imp1$age[miData$imp1$lit==0], na.rm=T)
mean(miData$imp1$age[miData$imp1$lit==1], na.rm=T)
sd(miData$imp1$age[miData$imp1$lit==0], na.rm=T)
sd(miData$imp1$age[miData$imp1$lit==1], na.rm=T)
mean(miData$imp1$leftright[miData$imp1$lit==0], na.rm=T)
mean(miData$imp1$leftright[miData$imp1$lit==1], na.rm=T)
sd(miData$imp1$leftright[miData$imp1$lit==0], na.rm=T)
sd(miData$imp1$leftright[miData$imp1$lit==1], na.rm=T)


### balance tables 

# nearest neighbor
m.out <- vector("list", m)
# do cem matching
for (i in 1:m) {
  m.out[[i]] <- matchit(lit ~ edbin + income_group + region_group + age_group + 
                           female + leftright, data = miData[[i]],method = "nearest")
}

summary(m.out[[1]])

#full matching
m.out2 <- vector("list", m)
# do cem matching
for (i in 1:m) {
  m.out2[[i]] <- matchit(lit ~ edbin + income_group + region_group + age_group + 
                          female + leftright, data = miData[[i]],method = "full")
}

summary(m.out2[[1]])

#cem
m.out4 <- vector("list", m)
# do cem matching
for (i in 1:m) {
  m.out4[[i]] <- matchit(lit ~ edbin + income_group + region_group + age_group + 
                           female + leftright, data = miData[[i]],method = "cem")
}


summary(m.out4[[1]])

## Sensitivity analyses

#fel only Table C1
texreg(list(mod24,mod25,mod23),custom.model.names = 
         c("Correct CB","Correct Direction CB","Information"),
       custom.coef.names = c("Intercept", "Female","FEL", 
                             "Middle Income", "High Income",
                             "Region Center", "Region South", "Age 32-51","Age over 51",
                             "Political Ideology",
                             "Party Cue", "Cost-benefit exercise",
                             "Party Cue: FEL",
                             "Cost-benefit exercise: FEL"))


#Interaction fel and edbin Table C2

texreg(list(mod27,mod28,mod26),custom.model.names = 
         c("Correct CB","Correct Direction CB","Information"),
       custom.coef.names = c("Intercept", "FEL","High Education","Female",
                             "Middle Income", "High Income",
                             "Region Center", "Region South", "Age 32-51","Age over 51",
                             "Political Ideology", "FEL: High ed.",
                             "Party Cue", "Cost-benefit exercise",
                             "Party Cue: FEL",
                             "Cost-benefit exercise: FEL", "Party Cue: High ed.",
                             "Cost-benefit exercise: High ed.", "Party Cue: FEL: High ed.",
                             "Cost-benefit exercise: FEL: High ed."))

#education only Table C3
texreg(list(mod21,mod22,mod20),custom.model.names = 
         c("Correct CB","Correct Direction CB","Information"),
       custom.coef.names = c("Intercept", "Female","High Education", 
                             "Middle Income", "High Income",
                             "Region Center", "Region South",  "Age 32-51","Age over 51",
                             "Political Ideology",
                             "Party Cue", "Cost-benefit exercise",
                             "Party Cue: High ed.",
                             "Cost-benefit exercise: High ed."))