### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021

### This code does the following:
### 1. Create tables 1 and 2 in the paper


## run multiple_imp, multiple_imp_no_matching, multiple_imp matching, and education_robust first


library(texreg)

# Table 1

texreg(list(mod1,mod2,mod3),custom.model.names = 
         c("Correct CB","Correct Direction CB","Information"),
       custom.coef.names = c("Intercept", "FEL", "Female", "High Education",
                             "Middle Income", "High Income",
                             "Region Center", "Region South",  "Age 32-51","Age over 51",
                             "Political Ideology",
                             "Party Cue", "Cost-benefit exercise",
                             "Party Cue: FEL",
                             "Cost-benefit exercise: FEL"))

# Table 2

texreg(list(mod5,mod6,mod4),custom.model.names = 
         c("Correct CB","Correct Direction CB","Information"),
       custom.coef.names = c("Intercept", "FEL", "Female", "High Education",
                             "Middle Income", "High Income",
                             "Region Center", "Region South",  "Age 32-51","Age over 51",
                             "Political Ideology",
                             "Party Cue", "Cost-benefit exercise",
                             "Party Cue: FEL",
                             "Cost-benefit exercise: FEL"))


